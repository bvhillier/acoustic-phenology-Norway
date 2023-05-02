############################
############################
# EBIRD EBIRD code to calculate observations per day for one species at each cluster
# repeat the code again to calculate observations per day for one species at each cluster
# this code creates a files per day variable from data frame of files recorded at each cluster
# this allows correction to be made for recorder effort/uptime
# create count of files per day using aggregate function
# then use inner join to add files per day variable to n.birds data frame
# load working dataset to avoid constant data loading

# Install packages (run packages script)

n.ebirds <- read_csv('data/ebird/n.eBird.cluster')
n.ebirds <- subset(n.ebirds, observation_date > '2022-02-28' & observation_date < '2022-07-01')
n.ebird.clust.day <- read_csv('data/ebird/n.ebird.clust.day.csv')
n.ebird.clust.day$julian_day <- yday(n.ebird.clust.day$day)
n.ebird.dates <- data.frame()

# subset for a certain species or group of species or site or cluster
# can do manually for each migrant species or use a for loop. manually to start
# subset for a particular cluster or set of clusters manually or with a for loop after checking if observations are greater than 20

migrants <- c('Willow Warbler', 'Barn Swallow', 'Common Chiffchaff', 'Spotted Flycatcher', 'Common Cuckoo', 'Common Swift', 'European Pied Flycatcher',
              'Greater Whitethroat', 'Eurasian Blackcap')
# migrants <- c('Willow Warbler')
x=1
# for (x in 1:(length(migrants))) {
rare.str <- migrants[x]
n.ebirds.loop <- subset(n.ebirds, common_name %in% rare.str)

# create new data frame with counts of observations per day and species
n.esp.clust.day <- aggregate(list(cluster = n.ebirds.loop$cluster), by = list(day = n.ebirds.loop$observation_date, files = n.ebirds.loop$cluster), length)

# rename first column to match desired name
names(n.esp.clust.day)[1] <- "day"
names(n.esp.clust.day)[2] <- "cluster"
names(n.esp.clust.day)[3] <- "obs"
# print new data frame
n.esp.clust.day

# create a simple data frame of day, cluster, cluster_files, obs_files. Use left_join.
n.esp <- left_join(n.esp.clust.day, n.ebird.clust.day, by = c('day','cluster'))
n.esp$species <- rare.str

# create frequency variable, observations per checklist
n.esp$freq <- n.esp$obs/n.esp$checklists
n.esp$norm <- n.esp$freq/max(n.esp$freq)
plot(n.esp$norm~n.esp$day)
# if using a for loop then need to create a different output file using chr string (the space in the ebird common name is an issue)
# write_csv(n.sp, 'data/norway/species/n.reed_bunting.csv')
#####################################

# the input file is a data frame with following variable: day, cluster, observations that day, checklists per cluster that day, freq per checklist, normalised freq
# n.sp <- read_csv('data/norway/n.spotted_flycatcher.csv')
n.earrival <- n.esp
summary(n.earrival)

clusters <- c('1_kris', '2_stav', '3_østf', '4_berg', '5_lill', '6_tron', '7_trom', '8_finn', '9_finn')
# clusters <- c('1_kris', '4_berg')
y=1
# for (y in 1:length(clusters)) {
  cluster <- clusters[y]
  n.earrival.loop <- subset(n.earrival, cluster %in% clusters[y])
  if (length(n.earrival.loop$obs) > 20) {
  plot(n.earrival.loop$norm~n.earrival.loop$day)
  migrant_name  <- rare.str
  migrant_name  <- tolower(gsub(" ", "", migrant_name))

# create a cumulative observations variable for freq variable
  n.earrival.loop$norm <- n.earrival.loop$freq/max(n.earrival.loop$freq)
  n.earrival.loop$cum <- cumsum(n.earrival.loop$freq)
  plot(n.earrival.loop$cum~n.earrival.loop$day)

# create a julian day variable for modelling
  n.earrival.loop$julian_day <- yday(n.earrival.loop$day)

# normalise the cumulative freq variable to 0.0-1.0 for glm modelling
  n.earrival.loop$cum_norm <- n.earrival.loop$cum/max(n.earrival.loop$cum)
  plot(n.earrival.loop$cum_norm~n.earrival.loop$day)

# model the arrivals with glm, binomial family and logit link (logistic regression or s-curve)
  model <- glm(0 + cum_norm ~ julian_day, data = n.earrival.loop, family = binomial(link = logit))
  summary(model)

# Generate predicted probability points for each julian day of the year 
  predicted <- data.frame(julian_day = n.earrival.loop$julian_day)
  predicted$cum_norm <- predict(model, newdata = predicted, type = "response")
  plot(predicted$cum_norm~predicted$julian_day)

# Create a new data frame for this species that includes the original data and the predicted probabilities
# set character variables for the plot later
  text_species <- n.earrival.loop[1,6]
  text_cluster <- n.earrival.loop[1,2]

  n.earrival.loop <- cbind(n.earrival.loop, predicted)
  n.earrival.loop <- n.earrival.loop[, -11]
  colnames(n.earrival.loop)[10:11] <- c('data_cum', 'model_cum')
  
# create set of arrival and departure times my various methods, and first and last file dates
row_index <- which.min(abs(predicted$cum_norm - 0.5))
earrive_glm50 <- predicted[row_index,1]
row_index <- which.min(abs(n.earrival.loop$data_cum - 0.5))
earrive_cum50 <- n.earrival.loop[row_index,5]
row_index <- which.min(abs(n.earrival.loop$data_cum - 0.05))
earrive_cum05 <- n.earrival.loop[row_index,5]


#Plot the real data and the predicted data from the model
# p1 <- ggplot() +
#   # geom_point(data=n.arrival, aes(x = julian_day, y = norm), color = "darkgreen", size=0.5) +
#   geom_point(data=n.earrival.loop, aes(x = julian_day, y = data_cum), color = "darkblue") +
#   geom_point(data=n.earrival.loop, aes(x = julian_day, y = model_cum), color = "red") +
#   geom_line(data=n.earrival.loop, aes(x= julian_day, y = model_cum), color = "red") +
#   xlim(c(60,270)) +
#   labs(x = "Day of year", y = "Probability of occurrence") +
#   ggtitle(paste("Example plot to determine acoustic arrival time:",text_species,"at Cluster" ,text_cluster))
# p1

################################
# EBIRD EBIRD repeat the code again to calculate observations per day for one species at each cluster
# create an empty data frame of julian day enable the join with n.arrival dataset
# change the observation days to 1 and the NAs to 0
empty <- as.data.frame(c(1:365))
colnames(empty)[1] <- c('julian_day')
n.efull <- left_join(empty, n.earrival.loop, by = 'julian_day')
n.efull <- n.efull %>% mutate(on_off = if_else(is.na(obs), 0, 1))
length(n.earrival.loop$obs)

# create a start date and range to anchor the logistic model around, start should be close to first observation, end should be after that
# this parameter needs to be tested and standardized
max(n.earrival.loop$julian_day)-min(n.earrival.loop$julian_day)
plot(n.efull$on_off~n.efull$julian_day)
# this code selects julian day of first observation, could also be changed to 5th or 10th etc
first <- which(n.efull$on_off == 1)[1]
start <- n.efull$julian_day[first]
last <- as.integer(max(n.earrival.loop$julian_day))
finish  <- n.efull$julian_day[last]
obs <- length(n.earrival.loop$obs)

# create some arrival thresholds by various methods using full dataset
efirst_obs <- which(n.efull$on_off == 1)[1]
efifth_obs <- which(n.efull$on_off == 1)[5] 
elast_obs <- as.integer(max(n.earrival.loop$julian_day))


# n.efull.range <- subset(n.efull, julian_day>=(start-20) & julian_day<=(start+30))
n.efull.range <- subset(n.efull, julian_day>=(earrive_cum05-30) & julian_day<=(earrive_cum05+30))
plot(n.efull.range$on_off~n.efull.range$julian_day)

# model the arrivals with glm, binomial family and logit link (logistic regression or s-curve)
model <- glm(0 + on_off ~ julian_day, data = n.efull.range, family = binomial(link = logit))
summary(model)

# Generate predicted probability points for each julian day of the year 
predicted_sig <- data.frame(julian_day = n.efull.range$julian_day)
predicted_sig$obs <- predict(model, newdata = predicted_sig, type = "response")
row_index <- which.min(abs(predicted_sig$obs - 0.5))
earrive_sig50 <- predicted_sig[row_index,1]
plot(predicted_sig$obs~predicted_sig$julian_day)
efive_percent <- ceiling(length(n.earrival.loop$obs)/20)
eninetyfive_percent <- ceiling(length(n.earrival.loop$obs)/20*19)
earrive_05 <- n.earrival.loop[(efive_percent), 5]
edepart_95 <- n.earrival.loop[(eninetyfive_percent), 5]

n.ebird.arrival <- data.frame()
n.ebird.arrival[1,1] <- rare.str
n.ebird.arrival[1,2] <- cluster
n.ebird.arrival[1,3] <- earrive_glm50
n.ebird.arrival[1,4] <- earrive_cum50
n.ebird.arrival[1,5] <- earrive_cum05
n.ebird.arrival[1,6] <- earrive_sig50
n.ebird.arrival[1,7] <- earrive_05
n.ebird.arrival[1,8] <- edepart_95
n.ebird.arrival[1,9] <- efifth_obs
n.ebird.arrival[1,10] <- elast_obs
colnames(n.ebird.arrival) <- c('species', 'cluster', 'arrive_glm50', 'arrive_cum50', 'arrive_cum05', 'arrive_sig50', 'arrive_05', 'depart_95', 'fifth_obs', 'last_obs')

n.ebird.dates <- rbind(n.ebird.dates, n.ebird.arrival)

# text_box <- paste(' First observation =', efirst_obs, "\n", 'Fifth observation =', efifth_obs, "\n",'5% observation =', earrive_05, "\n",'0.5 sigmoid model =', earrive_sig50, "\n",
#                   '5% cumulative =', earrive_cum05)
etext_box <- paste(' 5.day =', efifth_obs, "\n",'5.percent.day =', earrive_05, "\n", '5.percent.obs =', earrive_cum05)

p2.e <- ggplot() +
  theme_linedraw() +
  geom_point(data=n.efull, aes(x = julian_day, y = on_off), color = "aquamarine4", size=4, shape='circle') +
  geom_point(data=n.earrival.loop, aes(x = julian_day, y = norm), color = "darkgrey",size=2, shape='square') +
  geom_point(data=n.earrival.loop, aes(x = julian_day, y = data_cum), color = "darkblue",size=4, shape='triangle') +
  
  # geom_point(data=n.earrival.loop, aes(x = julian_day, y = model_cum), color = "darkblue", size=0.7) +
  # geom_line(data=n.earrival.loop, aes(x= julian_day, y = model_cum), color = "darkblue") +
  
  # geom_point(data=predicted_sig, aes(x = julian_day, y = obs), color = "firebrick", size=2, shape='triangle') +
  # geom_line(data=predicted_sig, aes(x= julian_day, y = obs), color = "firebrick") +
  
  # geom_vline(xintercept = earrive_glm50, linetype="solid", color = "darkblue", size=0.5) +
  # geom_vline(xintercept = earrive_cum50, linetype="solid", color = "darkblue", size=0.5) +
  geom_vline(xintercept = earrive_cum05, linetype="dotted", color = "darkblue", size=1.5) +
  # geom_vline(xintercept = earrive_sig50, linetype="dotted", color = "firebrick", size=0.5) +
  geom_vline(xintercept = earrive_05, linetype="dashed", color = "aquamarine4", size=1.5) +
  geom_vline(xintercept = efifth_obs, linetype="dotdash", color = "firebrick", size=1.5) +
  # geom_vline(xintercept = first_detect, linetype="dotted", color = "green", size=1) +
  # geom_vline(xintercept = last_detect, linetype="dotted", color = "green", size=1,) +
  
  xlim(c(90,180)) +
  theme(text=element_text(size=20)) +
  theme(axis.text= element_text(size=20)) +
  labs(x = "Day of year (day 100 = April 10th)", y = ("eBird observations (corrected for effort & normalised)")) +
  # ggtitle(paste("eBird arrival times:",text_species,"at Cluster" ,text_cluster)) +
  geom_text(aes(x = 90, y = 0.25, label = etext_box), size = 6, hjust = 0, vjust = 1) 
# p2.e
# ggsave(paste0("data/norway/arrivals.july.ebird/", migrant_name, "_", cluster, "_ebird2.jpg"), p2.e, width = 16, height = 16, dpi = 300)
   }
#   }
# }


# create a dataframe with the arrival data and the cluster lat/longs
cluster.full <- read_csv('data/norway/cluster.full.csv')
n.ebird.dates <- left_join(n.ebird.dates, cluster.full, by = 'cluster')
n.ebird.dates <- n.ebird.dates[, -11]
# output to csv for analysis and plotting
write_csv(n.ebird.dates, 'data/norway/arrivals.july.ebird/n.ebird.dates2.csv')

##############################################
# code to plot the various arrival methods data as multi species or as individual species

n.ebird.dates <- read_csv('data/norway/arrivals.july.ebird/n.ebird.dates2.csv')
migrants <- c('Willow Warbler', 'Barn Swallow', 'Common Chiffchaff', 'Spotted Flycatcher', 'Common Cuckoo', 'Common Swift', 'European Pied Flycatcher',
              'Greater Whitethroat', 'Eurasian Blackcap')
# migrants <- c('Willow Warbler')
n.ebird.dates <- subset(n.ebird.dates, species %in% migrant)

plot(n.ebird.dates$lat~n.ebird.dates$arrive_cum05)
abline(lm(n.ebird.dates$lat~n.ebird.dates$arrive_cum05))

plot(n.ebird.dates$lat~n.ebird.dates$arrive_05)
abline(lm(n.ebird.dates$lat~n.ebird.dates$arrive_05))
model <- lm(n.ebird.dates$lat~n.ebird.dates$arrive_05+0)
model
summary(model)



p3.e <- ggplot() +
  theme_linedraw() +
  geom_point(data=n.ebird.dates, aes(x = fifth_obs, y = lat), color = "aquamarine3", size=1) +
  geom_point(data=n.ebird.dates, aes(x = arrive_05, y = lat), color = "aquamarine4", size=4) +
  geom_point(data=n.ebird.dates, aes(x = arrive_sig50, y = lat), color = "coral1", size=1) +
  geom_point(data=n.ebird.dates, aes(x = arrive_cum05, y = lat), color = "dodgerblue1", size=1) +
  geom_point(data=n.ebird.dates, aes(x = arrive_cum50, y = lat), color = "dodgerblue3", size=1) +
  geom_point(data=n.ebird.dates, aes(x = arrive_glm50, y = lat), color = "mediumpurple1", size=1) +
  geom_point(data=n.ebird.dates, aes(x = depart_95, y = lat), color = "firebrick", size=1) +
  geom_point(data=n.ebird.dates, aes(x = last_obs, y = lat), color = "firebrick3",size=1) +
  geom_jitter(width = 0.5, height = 1) +
  
  # geom_smooth(data=n.ebird.dates, aes(x = first_obs, y = lat), method = lm, color = "aquamarine3", fill = "aquamarine3", size=1.3, alpha = 0.1, se = FALSE) +
  geom_smooth(data=n.ebird.dates, aes(x = arrive_05, y = lat), method = lm, color = "aquamarine4", fill = "aquamarine4", size=1.3, alpha = 0.1, se = FALSE) +
  # geom_smooth(data=n.ebird.dates, aes(x = arrive_sig50, y = lat), method = lm, color = "coral1", fill = "coral1", size=1.3, alpha = 0.1) +
  # geom_smooth(data=n.ebird.dates, aes(x = arrive_cum05, y = lat), method = lm, color = "dodgerblue1", fill = "dodgerblue1", size=1.3, alpha = 0.1) +
  # geom_smooth(data=n.ebird.dates, aes(x = arrive_cum50, y = lat), method = lm, color = "dodgerblue3", fill = "dodgerblue3", size=1.3, alpha = 0.1) +
  # geom_smooth(data=n.ebird.dates, aes(x = arrive_glm50, y = lat), method = lm, color = "mediumpurple1", fill = "mediumpurple1", size=1.3, alpha = 0.1) +
  # geom_smooth(data=n.ebird.dates, aes(x = depart_95, y = lat), method = lm, color = "firebrick", fill = "firebrick", size=1.3, alpha = 0.1) +
  # geom_smooth(data=n.ebird.dates, aes(x = last_obs, y = lat), method = lm, color = "firebrick3", fill = "firebrick3", size=1.3, alpha = 0.1) +
  
  xlim(c(60,180)) +
  labs(x = "Julian day of the year", y = "Cluster latitude") +
  
  ggtitle(paste("eBird arrival times using various methods: arrival day versus cluster latitudes")) 
  #  geom_text(aes(x = 60, y = 0.75, label = text_box), size = 4, hjust = 0, vjust = 1) 
p3.e

################################################
################################################
