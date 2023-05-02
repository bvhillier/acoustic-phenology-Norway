############################
############################
# repeat the code again to calculate detections per day for one species at each cluster
# this code creates a files per day variable from data frame of files recorded at each cluster
# this allows correction to be made for recorder effort/uptime
# create count of files per day using aggregate function
# then use inner join to add files per day variable to n.birds data frame
# load working dataset to avoid constant data loading

# Install packages (run packages script)

n.birds <- read_csv("data/norway/norway.birds.8.csv")
n.birds <- subset(n.birds, day > '2022-02-28' & day < '2022-10-01')
n.list.clust.day <- read_csv('data/norway/n.list.clust.day.csv')
n.list.clust.day$julian_day <- yday(n.list.clust.day$day)
n.bird.dates <- data.frame()

# subset for a certain species or group of species or site or cluster
# can do manually for each migrant species or use a for loop. manually to start
# subset for a particular cluster or set of clusters manually or with a for loop after checking if detections are greater than 20

migrants <- c('Willow Warbler', 'Barn Swallow', 'Common Chiffchaff', 'Spotted Flycatcher', 'Common Cuckoo', 'Common Swift', 'European Pied Flycatcher',
              'Greater Whitethroat', 'Eurasian Blackcap')
# migrants <- c('Willow Warbler')
# n.birds <- subset(n.birds, tags %in% migrants)
# n.birds <- subset(n.birds, cluster == '1_kris')
# migrants <- c('Willow Warbler')
# only used for testing
x=5
# start the species for loop
# for (x in 1:(length(migrants))) {
rare.str <- migrants[x]
n.birds.loop <- subset(n.birds, tags %in% rare.str)

# create new data frame with counts of observations per day and species
n.sp.clust.day <- aggregate(list(cluster = n.birds.loop$cluster), by = list(day = n.birds.loop$day, files = n.birds.loop$cluster), length)

# rename first column to match desired name
names(n.sp.clust.day)[1] <- "day"
names(n.sp.clust.day)[2] <- "cluster"
names(n.sp.clust.day)[3] <- "detect"
# print new data frame
n.sp.clust.day

# create a simple data frame of day, cluster, cluster_files, detect_files. Use left_join.
n.sp <- left_join(n.sp.clust.day, n.list.clust.day, by = c('day','cluster'))
n.sp$species <- rare.str

# create frequency variable, detections per file recorded
n.sp$freq <- n.sp$detect/n.sp$files
n.sp$norm <- n.sp$freq/max(n.sp$freq)
# plot(n.sp$norm~n.sp$day)
#####################################

n.arrival <- n.sp
summary(n.arrival)

clusters <- c('1_kris', '2_stav', '3_østf', '4_berg', '5_lill', '6_tron', '7_trom', '8_finn', '9_finn')
# clusters <- c('1_kris', '4_berg')
# only used for testing
y=2
# start the cluster for loop
# for (y in 1:length(clusters)) {
  cluster <- clusters[y]
  n.arrival.loop <- subset(n.arrival, cluster %in% clusters[y])
  if (length(n.arrival.loop$detect) > 10) {
  # plot(n.arrival.loop$norm~n.arrival.loop$day)
  migrant_name  <- rare.str
  migrant_name  <- tolower(gsub(" ", "", migrant_name))

# create a cumulative detections variable for freq variable
  n.arrival.loop$cum <- cumsum(n.arrival.loop$freq)
  # plot(n.arrival.loop$cum~n.arrival.loop$day)

# create a julian day variable for modelling
  n.arrival.loop$julian_day <- yday(n.arrival.loop$day)

# normalise the cumulative freq variable to 0.0-1.0 for glm modelling
  n.arrival.loop$norm <- n.arrival.loop$freq/max(n.arrival.loop$freq)
  n.arrival.loop$cum_norm <- n.arrival.loop$cum/max(n.arrival.loop$cum)
  # plot(n.arrival.loop$cum_norm~n.arrival.loop$day)

# model the arrivals with glm, binomial family and logit link (logistic regression or s-curve)
  model <- glm(0 + data_cum ~ julian_day, data = n.arrival.loop, family = binomial(link = logit))
  summary(model)

# Generate predicted probability points for each julian day of the year 
  predicted <- data.frame(julian_day = n.arrival.loop$julian_day)
  predicted$cum_norm <- predict(model, newdata = predicted, type = "response")
  # plot(predicted$cum_norm~predicted$julian_day)

# Create a new data frame for this species that includes the original data and the predicted probabilities
# set character variables for the plot later
  text_species <- n.arrival.loop[1,6]
  text_cluster <- n.arrival.loop[1,2]

  n.arrival.loop <- cbind(n.arrival.loop, predicted)
  n.arrival.loop <- n.arrival.loop[, -11]
  colnames(n.arrival.loop)[10:11] <- c('data_cum', 'model_cum')
  
# create set of arrival and departure times my various methods, and first and last file dates
row_index <- which.min(abs(predicted$cum_norm - 0.5))
arrive_glm50 <- predicted[row_index,1]
row_index <- which.min(abs(n.arrival.loop$data_cum - 0.5))
arrive_cum50 <- n.arrival.loop[row_index,5]
row_index <- which.min(abs(n.arrival.loop$data_cum - 0.05))
arrive_cum05 <- n.arrival.loop[row_index,5]


# Plot the real data and the predicted data from the model
# p1 <- ggplot() +
#   # geom_point(data=n.arrival, aes(x = julian_day, y = norm), color = "darkgreen", size=0.5) +
#   geom_point(data=n.arrival, aes(x = julian_day, y = data_cum), color = "darkblue") +
#   geom_point(data=n.arrival, aes(x = julian_day, y = model_cum), color = "red") +
#   geom_line(data=n.arrival, aes(x= julian_day, y = model_cum), color = "red") +
#   xlim(c(60,270)) +
#   labs(x = "Day of year", y = "Probability of occurrence") +
#   ggtitle(paste("Example plot to determine acoustic arrival time:",text_species,"at Cluster" ,text_cluster)) 
# p1

################################
# ACOUSTIC ACOUSTIC try an acoustic daily detection version: detection = and no detection = 0 for logistic regression modelling
# create an empty data frame of julian day enable the join with n.arrival dataset
# change the detection days to 1 and the NAs to 0
empty <- as.data.frame(c(1:365))
colnames(empty)[1] <- c('julian_day')
n.full <- left_join(empty, n.arrival.loop, by = 'julian_day')
n.full <- n.full %>% mutate(on_off = if_else(is.na(detect), 0, 1))
length(n.arrival.loop$detect)

# create a start date and range to anchor the logistic model around, start should be close to first detection, end should be after that
# this parameter needs to be tested and standardized
max(n.arrival.loop$julian_day)-min(n.arrival.loop$julian_day)
# plot(n.full$on_off~n.full$julian_day)
# this code selects julian day of first detection, could also be changed to 5th or 10th etc
first <- which(n.full$on_off == 1)[1]
start <- n.full$julian_day[first]
last <- as.integer(max(n.arrival.loop$julian_day))
finish  <- n.full$julian_day[last]
detects <- length(n.arrival.loop$detect)

# create some arrival thresholds by various methods using full dataset
first_detect <- which(n.full$on_off == 1)[1]
fifth_detect <- which(n.full$on_off == 1)[5] 
last_detect <- as.integer(max(n.arrival.loop$julian_day))

# n.full.range <- subset(n.full, julian_day>=(start-20) & julian_day<=(start+30))
n.full.range <- subset(n.full, julian_day>=(arrive_cum05-30) & julian_day<=(arrive_cum05+30))

# plot(n.full.range$on_off~n.full.range$julian_day)

# model the arrivals with glm, binomial family and logit link (logistic regression or s-curve)
model <- glm(0 + on_off ~ julian_day, data = n.full.range, family = binomial(link = logit))
summary(model)

# Generate predicted probability points for each julian day of the year 
predicted_sig <- data.frame(julian_day = n.full.range$julian_day)
predicted_sig$detect <- predict(model, newdata = predicted_sig, type = "response")
row_index <- which.min(abs(predicted_sig$detect - 0.5))
arrive_sig50 <- predicted_sig[row_index,1]
# plot(predicted_sig$detect~predicted_sig$julian_day)
five_percent <- ceiling(length(n.arrival.loop$detect)/20)
ninetyfive_percent <- ceiling(length(n.arrival.loop$detect)/20*19)
arrive_05 <- n.arrival.loop[(five_percent), 5]
depart_95 <- n.arrival.loop[(ninetyfive_percent), 5]

n.bird.arrival <- data.frame()
n.bird.arrival[1,1] <- rare.str
n.bird.arrival[1,2] <- cluster
n.bird.arrival[1,3] <- arrive_glm50
n.bird.arrival[1,4] <- arrive_cum50
n.bird.arrival[1,5] <- arrive_cum05
n.bird.arrival[1,6] <- arrive_sig50
n.bird.arrival[1,7] <- arrive_05
n.bird.arrival[1,8] <- depart_95
n.bird.arrival[1,9] <- fifth_detect
n.bird.arrival[1,10] <- last_detect
colnames(n.bird.arrival) <- c('species', 'cluster', 'arrive_glm50', 'arrive_cum50', 'arrive_cum05', 'arrive_sig50', 'arrive_05', 'depart_95', 'fifth_detect', 'last_detect')

n.bird.dates <- rbind(n.bird.dates, n.bird.arrival)

# text_box <- paste('First detection =', first_detect, "\n", 'Fifth detection =', fifth_detect, "\n",'5% detection =', arrive_05, "\n",'0.5 sigmoid model =', arrive_sig50, "\n",
#                   '5% cumulative =', arrive_cum05)
text_box <- paste(' 5.day =', fifth_detect, "\n",'5.percent.day =', arrive_05, "\n",'5.percent.detect =', arrive_cum05)

p2 <- ggplot() +
  theme_linedraw() +
  geom_point(data=n.full, aes(x = julian_day, y = on_off), color = "aquamarine4", size=4, shape='circle') +
  geom_point(data=n.arrival.loop, aes(x = julian_day, y = norm), color = "darkgrey", size=2, shape='square') +
  geom_point(data=n.arrival.loop, aes(x = julian_day, y = data_cum), color = "darkblue", size=4, shape='triangle') +
  
  # geom_point(data=n.arrival.loop, aes(x = julian_day, y = model_cum), color = "darkblue", size=2, shape='triangle') +
  # geom_line(data=n.arrival.loop, aes(x= julian_day, y = model_cum), color = "darkblue") +
  # 
  # geom_point(data=predicted_sig, aes(x = julian_day, y = detect), color = "aquamarine4", size=2, shape='triangle') +
  # geom_line(data=predicted_sig, aes(x= julian_day, y = detect), color = "aquamarine4") +
  
  # geom_vline(xintercept = arrive_glm50, linetype="solid", color = "darkblue", size=0.5) +
  # geom_vline(xintercept = arrive_cum50, linetype="solid", color = "darkblue", size=0.5) +
  geom_vline(xintercept = arrive_cum05, linetype="dotted", color = "darkblue", size=1.5) +
  geom_vline(xintercept = arrive_sig50, linetype="dotted", color = "firebrick", size=0.5) +
  geom_vline(xintercept = arrive_05, linetype="dashed", color = "aquamarine4", size=1.5) +
  geom_vline(xintercept = fifth_detect, linetype="dotdash", color = "firebrick", size=1.5) +
  # geom_vline(xintercept = first_detect, linetype="dotted", color = "green", size=1) +
  # geom_vline(xintercept = last_detect, linetype="dotted", color = "green", size=1,) +
  
  xlim(c(90,180)) +
  theme(text=element_text(size=20)) +
  theme(axis.text= element_text(size=20)) +
  labs(x = "Day of year (day 100 = April 10th)", y = ("Acoustic detecions (corrected for effort and normalised)")) +
  # ggtitle(paste("Acoustic arrival times:",text_species,"at Cluster" ,text_cluster)) +
  geom_text(aes(x = 90, y = 0.25, label = text_box), size = 6, hjust = 0, vjust = 1) 
p2
# ggsave(paste0("data/norway/arrivals.july/", migrant_name, "_", cluster, "_acoustic2.jpg"), p2, width = 16, height = 16, dpi = 300)
   }
#   }
# }

# create a data frame with the arrival data and the cluster lat/longs
cluster.full <- read_csv('data/norway/cluster.full.csv')
n.bird.dates <- left_join(n.bird.dates, cluster.full, by = 'cluster')
n.bird.dates <- n.bird.dates[, -11]
# output to csv for analysis and plotting
write_csv(n.bird.dates, 'data/norway/arrivals.july/n.bird.dates2.csv')

##############################################
# code to plot the various arrival methods data as multi species or as individual species
n.bird.dates <- read_csv('data/norway/arrivals.july/n.bird.dates2.csv')

# migrants <- c('Willow Warbler', 'Barn Swallow', 'Common Chiffchaff', 'Spotted Flycatcher', 'Common Cuckoo', 'Common Swift', 'European Pied Flycatcher',
#               'Greater Whitethroat', 'Eurasian Blackcap')
migrant <- c('Willow Warbler')
n.bird.dates <- subset(n.bird.dates, species %in% migrant)

#1 lat
plot(n.bird.dates$lat~n.bird.dates$fifth_detect)
abline(lm(n.bird.dates$lat~n.bird.dates$fifth_detect))
model <- lm(n.bird.dates$lat~n.bird.dates$fifth_detect)
summary(model)
plot(fitted(model),residuals(model))
abline(h=0,lwd=2,lty=2,col="red")
qqnorm(residuals(model))
qqline(residuals(model))
#2
plot(n.bird.dates$lat~n.bird.dates$arrive_cum05)
abline(lm(n.bird.dates$lat~n.bird.dates$arrive_cum05))
model <- lm(n.bird.dates$lat~n.bird.dates$arrive_cum05)
summary(model)
plot(fitted(model),residuals(model))
abline(h=0,lwd=2,lty=2,col="red")
qqnorm(residuals(model))
qqline(residuals(model))
#3
plot(n.bird.dates$lat~n.bird.dates$arrive_05)
abline(lm(n.bird.dates$lat~n.bird.dates$arrive_05))
model <- lm(n.bird.dates$lat~n.bird.dates$arrive_05)
summary(model)
plot(fitted(model),residuals(model))
abline(h=0,lwd=2,lty=2,col="red")
qqnorm(residuals(model))
qqline(residuals(model))

p3.8 <- ggplot() +
  theme_linedraw() +
  # geom_point(data=n.bird.dates, aes(x = first_detect, y = lat), color = "aquamarine3", size=1) +
  geom_point(data=n.bird.dates, aes(x = arrive_05, y = lat), color = "firebrick", shape = 'diamond', size=4) +
  geom_point(data=n.bird.dates, aes(x = fifth_detect, y = lat), color = "aquamarine3", shape= 'circle', size=4) +
  geom_point(data=n.bird.dates, aes(x = arrive_cum05, y = lat), color = "dodgerblue4", shape = 'triangle', size=6) +
  # geom_point(data=n.bird.dates, aes(x = arrive_cum50, y = lat), color = "dodgerblue3", size=1) +
  # geom_point(data=n.bird.dates, aes(x = arrive_glm50, y = lat), color = "mediumpurple1", size=1) +
  # geom_point(data=n.bird.dates, aes(x = depart_95, y = lat), color = "firebrick", size=1) +
  # geom_point(data=n.bird.dates, aes(x = last_detect, y = lat), color = "firebrick3",size=1) +
  # geom_jitter(width = 0.5, height = 1) +
  
  # geom_smooth(data=n.bird.dates, aes(x = first_detect, y = lat), method = lm, color = "aquamarine3", fill = "aquamarine3", size=1.3, alpha = 0.1) +
  geom_smooth(data=n.bird.dates, aes(x = arrive_05, y = lat), method = lm, linetype = 'dotted', color = "firebrick", fill = "firebrick", size=1, alpha = 0.1, se = FALSE) +
  geom_smooth(data=n.bird.dates, aes(x = fifth_detect, y = lat), method = lm, linetype = 'dotted', color = "aquamarine4", fill = "aquamarine4", size=1, alpha = 0.1, se = FALSE) +
  geom_smooth(data=n.bird.dates, aes(x = arrive_cum05, y = lat), method = lm, linetype = 'solid', color = "dodgerblue4", fill = "dodgerblue4", size=2, alpha = 0.1, se = TRUE) +
  # geom_smooth(data=n.bird.dates, aes(x = arrive_cum50, y = lat), method = lm, color = "dodgerblue3", fill = "dodgerblue3", size=1.3, alpha = 0.1) +
  # geom_smooth(data=n.bird.dates, aes(x = arrive_glm50, y = lat), method = lm, color = "mediumpurple1", fill = "mediumpurple1", size=1.3, alpha = 0.1) +
  # geom_smooth(data=n.bird.dates, aes(x = depart_95, y = lat), method = lm, color = "firebrick", fill = "firebrick", size=1.3, alpha = 0.1) +
  # geom_smooth(data=n.bird.dates, aes(x = last_detect, y = lat), method = lm, color = "firebrick3", fill = "firebrick3", size=1.3, alpha = 0.1) +
  
  xlim(c(110,145)) +
  labs(x = "Julian day of the year", y = "Acoustic detection latitude") +
  theme(text=element_text(size=20))
  # ggtitle(paste("Acoustic arrival times using all methods: arrival day versus cluster latitudes")) 
  #  geom_text(aes(x = 60, y = 0.75, label = text_box), size = 4, hjust = 0, vjust = 1) 
# p3.8
# ggsave(paste0("data/norway/arrivals.july/acoustic_latitude2.jpg"), p3.8, width = 16, height = 8, dpi = 300)

p3.9 <- ggplot() +
  theme_linedraw() +
  # geom_point(data=n.bird.dates, aes(x = first_detect, y = lat), color = "aquamarine3", size=1) +
  geom_point(data=n.bird.dates, aes(x = as.Date(arrive_05, origin = "2022-01-01"), y = lat), color = "firebrick", shape = 'diamond', size=3) +
  geom_point(data=n.bird.dates, aes(x = as.Date(fifth_detect, origin = "2022-01-01"), y = lat), color = "aquamarine3", shape= 'circle', size=3) +
  geom_point(data=n.bird.dates, aes(x = as.Date(arrive_cum05, origin = "2022-01-01"), y = lat), color = "dodgerblue4", shape = 'triangle', size=8) +
  # geom_point(data=n.bird.dates, aes(x = arrive_cum50, y = lat), color = "dodgerblue3", size=1) +
  # geom_point(data=n.bird.dates, aes(x = arrive_glm50, y = lat), color = "mediumpurple1", size=1) +
  # geom_point(data=n.bird.dates, aes(x = depart_95, y = lat), color = "firebrick", size=1) +
  # geom_point(data=n.bird.dates, aes(x = last_detect, y = lat), color = "firebrick3",size=1) +
  # geom_jitter(width = 0.5, height = 1) +
  
  # geom_smooth(data=n.bird.dates, aes(x = first_detect, y = lat), method = lm, color = "aquamarine3", fill = "aquamarine3", size=1.3, alpha = 0.1) +
  geom_smooth(data=n.bird.dates, aes(x = as.Date(arrive_05, origin = "2022-01-01"), y = lat), method = lm, linetype = 'dotted', color = "firebrick", fill = "firebrick", size=1, alpha = 0.1, se = FALSE) +
  geom_smooth(data=n.bird.dates, aes(x = as.Date(fifth_detect, origin = "2022-01-01"), y = lat), method = lm, linetype = 'dotted', color = "aquamarine4", fill = "aquamarine4", size=1, alpha = 0.1, se = FALSE) +
  geom_smooth(data=n.bird.dates, aes(x = as.Date(arrive_cum05, origin = "2022-01-01"), y = lat), method = lm, linetype = 'solid', 
              fullrange=TRUE, color = "dodgerblue4", fill = "dodgerblue4", size=2, alpha = 0.1, se = TRUE) +
  # geom_smooth(data=n.bird.dates, aes(x = arrive_cum50, y = lat), method = lm, color = "dodgerblue3", fill = "dodgerblue3", size=1.3, alpha = 0.1) +
  # geom_smooth(data=n.bird.dates, aes(x = arrive_glm50, y = lat), method = lm, color = "mediumpurple1", fill = "mediumpurple1", size=1.3, alpha = 0.1) +
  # geom_smooth(data=n.bird.dates, aes(x = depart_95, y = lat), method = lm, color = "firebrick", fill = "firebrick", size=1.3, alpha = 0.1) +
  # geom_smooth(data=n.bird.dates, aes(x = last_detect, y = lat), method = lm, color = "firebrick3", fill = "firebrick3", size=1.3, alpha = 0.1) +
  xlim(c(as.Date("2022-04-15"), as.Date("2022-06-05"))) +
  # xlim(c(110,145)) +
  labs(x = "Acoustic arrival date", y = "Acoustic detection latitude") +
  theme(text=element_text(size=20)) +
  theme(axis.text= element_text(size=20))
# ggtitle(paste("Acoustic arrival times using all methods: arrival day versus cluster latitudes")) 
#  geom_text(aes(x = 60, y = 0.75, label = text_box), size = 4, hjust = 0, vjust = 1) 
# p3.9
# ggsave(paste0("data/norway/arrivals.july/acoustic_latitude.poster2.jpg"), p3.9, width = 16, height = 8, dpi = 300)

################################################
################################################
# plot acoustic arrivals vs ebird arrivals
n.bird.dates <- read_csv('data/norway/arrivals.july/n.bird.dates2.csv')
n.ebird.dates <- read_csv('data/norway/arrivals.july.ebird/n.ebird.dates2.csv')


emean <- mean(n.ebird.dates$arrive_05)
nmean <- mean(n.bird.dates$arrive_05)

n.all.dates <- full_join(n.bird.dates, n.ebird.dates, by = c('cluster','species'))

# n.all.dates$rowmin <- apply(n.all.dates[, c(7,17)], 1, min, na.rm = TRUE)
# n.all.dates <- n.all.dates %>% mutate(species = fct_reorder(species, rowmin)) 
# p4.8.arrive_05 <- ggplot() +
#   theme_linedraw() +
#   geom_point(data=n.all.dates, aes(x = species, y = as.Date(arrive_05.x, origin = "2022-01-01"), shape=cluster), color = "dodgerblue3", size=4) +
#   geom_point(data=n.all.dates, aes(x = species, y = as.Date(arrive_05.y, origin = "2022-01-01"), shape=cluster), color = "aquamarine3", size=4, shape='square', alpha = 0.7) +
#   # geom_hline(yintercept = nmean, linetype= "dotted", color = "coral1", size=1.0) +
#   # geom_hline(yintercept = emean, linetype= "dotted", color = "aquamarine3", size=1.0) +
#   scale_y_date(date_breaks = "1 month",
#                date_labels="%b-%d",
#                limits = as.Date(c('2022-03-01','2022-07-01'))) +
#   labs(x = "Migrant species", y = "Arrival day") +
#   theme(axis.text.x = element_text(size=15, angle=45,hjust=1)) +
#   theme(text=element_text(size=15, face = "bold")) +
#   ggtitle(paste('Arrival times by fifth percentile method all migrant species. Acoustic (blue triangles), eBird (green squares)'))
# p4.8.arrive_05
# ggsave(paste0("data/norway/arrivals.july/arrive_05.jpg"), p4.8.arrive_05, width = 16, height = 8, dpi = 300)
# 
# 
# n.all.dates$rowmin <- apply(n.all.dates[, c(5,15)], 1, min, na.rm = TRUE)
# n.all.dates <- n.all.dates %>% mutate(species = fct_reorder(species, rowmin)) 
# p4.8.arrive_cum05 <- ggplot() +
#   theme_linedraw() +
#   geom_point(data=n.all.dates, aes(x = species, y = as.Date(arrive_cum05.x, origin = "2022-01-01")), color = "coral1", size=4, shape='square') +
#   geom_point(data=n.all.dates, aes(x = species, y = as.Date(arrive_cum05.y, origin = "2022-01-01")), color = "dodgerblue3", size=4, shape='square', alpha = 0.7) +
#   # geom_hline(yintercept = nmean, linetype= "dotted", color = "coral1", size=1.0) +
#   # geom_hline(yintercept = emean, linetype= "dotted", color = "aquamarine3", size=1.0) +
#   scale_y_date(date_breaks = "1 month",
#                date_labels="%b-%d",
#                limits = as.Date(c('2022-03-01','2022-07-01'))) +
#   labs(x = "Migrant species", y = "Arrival day") +
#   theme(axis.text.x = element_text(size=15, angle=45,hjust=1)) +
#   theme(text=element_text(size=15, face = "bold"))
#   # ggtitle(paste('Arrival times by fifth percentile cumulative method all migrant species'))
# p4.8.arrive_cum05
# ggsave(paste0("data/norway/arrivals.july/arrive_cum05.jpg"), p4.8.arrive_cum05, width = 16, height = 8, dpi = 300)
# 
# n.all.dates$rowmin <- apply(n.all.dates[, c(6,16)], 1, min, na.rm = TRUE)
# n.all.dates <- n.all.dates %>% mutate(species = fct_reorder(species, rowmin)) 
# levels(n.all.dates$species)
# 
# p4.8.arrive_sig50 <- ggplot() +
#   theme_linedraw() +
#   geom_point(data=n.all.dates, aes(x = species, y = as.Date(arrive_sig50.x, origin = "2022-01-01")), color = "coral1", size=4, shape='square') +
#   geom_point(data=n.all.dates, aes(x = species, y = as.Date(arrive_sig50.y, origin = "2022-01-01")), color = "darkblue", size=4, shape='square', alpha = 0.7) +
#   # geom_hline(yintercept = nmean, linetype= "dotted", color = "coral1", size=1.0) +
#   # geom_hline(yintercept = emean, linetype= "dotted", color = "aquamarine3", size=1.0) +
#   scale_y_date(date_breaks = "1 month", 
#                date_labels="%b-%d",
#                limits = as.Date(c('2022-03-01','2022-07-01'))) +
#   labs(x = "Migrant species", y = "Arrival day") +
#   theme(axis.text.x = element_text(size=15, angle=45,hjust=1)) +
#   theme(text=element_text(size=15, face = "bold"))
#   ggtitle(paste('Arrival times by sigmoidal binary method all migrant species'))
# p4.8.arrive_sig50
# ggsave(paste0("data/norway/arrivals.july/arrive_sig50.jpg"), p4.8.arrive_sig50, width = 16, height = 8, dpi = 300)
# 
n.both.dates <- inner_join(n.bird.dates, n.ebird.dates, by = c('cluster','species'))
# plot(n.both.dates$arrive_05.x~n.both.dates$arrive_05.y)
# 
# emean <- mean(n.ebird.dates$arrive_05)
# nmean <- mean(n.bird.dates$arrive_05)
# 
# n.both.dates$rowmin <- apply(n.both.dates[, c(7, 17)], 1, min)
# n.both.dates <- n.both.dates %>% mutate(species = fct_reorder(species, rowmin)) 
# p5.8 <- ggplot() +
#   theme_linedraw() +
#   geom_point(data=n.both.dates, aes(x = species, y = as.Date(arrive_05.x, origin = "2022-01-01"), shape=cluster), color = "dodgerblue3", size=5, ) +
#   geom_point(data=n.both.dates, aes(x = species, y = as.Date(arrive_05.y, origin = "2022-01-01"), shape=cluster), color = "aquamarine3", size=5, alpha=0.7) +
#   # geom_hline(yintercept = nmean, linetype= "dotted", color = "coral1", size=1.0) +
#   # geom_hline(yintercept = emean, linetype= "dotted", color = "aquamarine3", size=1.0) +
#   scale_y_date(date_breaks = "1 month", 
#                date_labels="%b-%d",
#                limits = as.Date(c('2022-03-01','2022-07-01'))) +
#   labs(x = "Migrant species", y = "Arrival day") +
#   theme(axis.text.x = element_text(size=10, angle=45, hjust=1)) +
#   theme(text=element_text(size=15, face = "bold")) +
#   theme(text = element_text(family = "Arial")) +
#   ggtitle(paste('Arrival times by fifth percentile method, direct comparison for selected migrant species'))
# p5.8
# ggsave(paste0("data/norway/arrivals.july/compare.arrive_05.jpg"), p5.8, width = 16, height = 8, dpi = 300)

n.both.dates$rowmin <- apply(n.both.dates[, c(3:10,13:20)], 1, min)
n.both.dates$species_order <- reorder(n.both.dates$species, n.both.dates$rowmin)
migrants <- c('Willow Warbler', 'Barn Swallow', 'Common Chiffchaff', 'Spotted Flycatcher', 'Common Cuckoo', 'Common Swift', 'European Pied Flycatcher',
              'Greater Whitethroat', 'Eurasian Blackcap')
# migrants <- c('Willow Warbler')
n.both.dates <- subset(n.both.dates, species %in% migrant)

plot(n.both.dates$arrive_05.x~n.both.dates$arrive_05.y)
abline(lm(n.both.dates$arrive_05.x~n.both.dates$arrive_05.y))
model <- lm(n.both.dates$arrive_05.x~n.both.dates$arrive_05.y)
summary(model)

plot(n.both.dates$arrive_cum05.x~n.both.dates$arrive_cum05.y)
abline(lm(n.both.dates$arrive_cum05.x~n.both.dates$arrive_cum05.y))
model <- lm(n.both.dates$arrive_cum05.x~n.both.dates$arrive_cum05.y)
summary(model)
plot(fitted(model),residuals(model))
abline(h=0,lwd=2,lty=2,col="red")
qqnorm(residuals(model))
qqline(residuals(model))

plot(n.both.dates$fifth_detect~n.both.dates$fifth_obs)
abline(lm(n.both.dates$arrive_cum05.x~n.both.dates$arrive_cum05.y))
model <- lm(n.both.dates$arrive_cum05.y~n.both.dates$arrive_cum05.x)
summary(model)

p6.8 <- ggplot() +
  theme_linedraw() +
  geom_point(data=n.both.dates, aes(x = as.Date(arrive_05.x, origin = "2022-01-01"), y = as.Date(arrive_05.y, origin = "2022-01-01")), color = "aquamarine4", size=4, shape = 'circle') +
  geom_point(data=n.both.dates, aes(x = as.Date(arrive_cum05.x, origin = "2022-01-01"), y = as.Date(arrive_cum05.y, origin = "2022-01-01")), color = "dodgerblue4", size=8, shape = 'triangle') +
  geom_point(data=n.both.dates, aes(x = as.Date(fifth_detect, origin = "2022-01-01"), y = as.Date(fifth_obs, origin = "2022-01-01")), color = "firebrick", size=4, shape = 'diamond') +
  # geom_point(data=n.both.dates, aes(x = as.Date(arrive_cum50.x, origin = "2022-01-01"), y = as.Date(arrive_cum50.y, origin = "2022-01-01"), shape = species), color = "firebrick", size=4) +
  
  # code to plot lm lines using julian day
  # geom_smooth(data=n.both.dates, aes(x = arrive_05.x, y = arrive_05.y), method = lm, color = "coral1", size=1.3, alpha = 0.1, se = FALSE) +
  # geom_smooth(data=n.both.dates, aes(x = as.Date(arrive_cum05.x, origin = "2022-01-01"), y = as.Date(arrive_cum05.y, origin = "2022-01-01")), method = lm, 
  #           fullrange=TRUE, fill = "dodgerblue4",color = "dodgerblue4", size=1.3, linetype = "dotdash", alpha = 0.1, se = FALSE) +
  # geom_smooth(data=n.both.dates, aes(x = arrive_sig50.x, y = arrive_sig50.y), method = lm, color = "dodgerblue3", size=1.3, alpha = 0.1, se = FALSE) +
  # geom_smooth(data=n.both.dates, aes(x = arrive_cum50.x, y = arrive_cum50.y), method = lm, color = "firebrick", size=1.3, alpha = 0.1, se = FALSE) +
  
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "dodgerblue4", size = 2.0) +
  # code to plot axies using julian day
  # xlim(c(60,240)) +
  # ylim(c(60,240)) +
  xlim(c(as.Date("2022-04-15"), as.Date("2022-06-15"))) +
  ylim(c(as.Date("2022-04-15"), as.Date("2022-06-15"))) +
  labs(x = "Acoustic arrival date", y = "eBird arrival date") +
  # theme(axis.text.x = element_text(size=15, angle=45,hjust=1)) +
  # ggtitle(paste('Arrival times by all methods, acoustic vs eBird (confidence threshold = 0.8)')) +
  guides(shape=guide_legend(title = "Species symbol")) +
  theme(legend.position = c(0.85, 0.2)) +
  theme(text=element_text(size=20)) +
  theme(axis.text= element_text(size=20))
# p6.8
# ggsave(paste0("data/norway/arrivals.july/acoustic.ebird2.jpg"), p6.8, width = 16, height = 8, dpi = 300)

model_arrive_05 <- lm(n.both.dates$arrive_05.x~n.both.dates$arrive_05.y)
summary(model_arrive_05 )
model_arrive_cum05 <- lm(n.both.dates$arrive_cum05.y~n.both.dates$arrive_cum05.x)
summary(model_arrive_cum05)
model_arrive_sig50 <- lm(n.both.dates$arrive_sig50.x~n.both.dates$arrive_sig50.y)
summary(model_arrive_sig50)
model_arrive_cum50 <- lm(n.both.dates$arrive_cum50.x~n.both.dates$arrive_cum50.y)
summary(model_arrive_cum50)
corr_arrive_05 <- cor.test(x=n.both.dates$arrive_05.x, y=n.both.dates$arrive_05.y, method = 'spearman')
corr_arrive_05 
corr_arrive_sig50 <- cor.test(x=n.both.dates$arrive_sig50.x, y=n.both.dates$arrive_sig50.y, method = 'pearson')
corr_arrive_sig50 
corr_arrive_cum05 <- cor.test(x=n.both.dates$arrive_cum05.x, y=n.both.dates$arrive_cum05.y, method = 'spearman')
corr_arrive_cum05 
corr_arrive_cum50 <- cor.test(x=n.both.dates$arrive_cum50.x, y=n.both.dates$arrive_cum50.y, method = 'spearman')
corr_arrive_cum50
