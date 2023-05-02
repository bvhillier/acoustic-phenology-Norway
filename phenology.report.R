# Main R Script for project 1 of MRes BEC
# Phenology of Avian Migration in Northern Europe

# acoustic data is loaded a carpentered in the acoustic.R file
# ebird data is loaded and carpentered in eBird.R

# Install packages (run packages script)

###############################
# START HERE START HERE START HERE START HERE START HERE START HERE START HERE START HERE START HERE START HERE START HERE START HERE START HERE START HERE START HERE
# START HERE START HERE START HERE START HERE START HERE START HERE START HERE START HERE START HERE START HERE START HERE START HERE START HERE START HERE START HERE

# load working dataset to avoid constant data loading
n.birds <- read_csv("data/norway/norway.birds.8.csv")

# Show histogram of confidence values
hist(n.birds$confidence, breaks=80, col='darkgreen', xlab='Confidence', ylab='Frequency', main='Confidence values')

# show table of detections per species in the data set in descending order
n.birds.species <- as.data.frame(table(n.birds$tags))
colnames(n.birds.species) <- c("Species", "Detections")
n.birds.species <- arrange(n.birds.species, desc(Detections))
knitr::kable(n.birds.species, col.names=c("Species","Detections"),caption = "Species Detections")
write_csv(n.birds.species, "data/norway/n.birds.species.8.csv")

# show table of detections per site in the data set
site.nums <- as.data.frame(table(n.birds$site))
site.nums <- arrange(site.nums, desc(Freq))
knitr::kable(site.nums,col.names=c("Sites","No. of detections"),caption = "Site Detections")

# subset for a certain species or group of species or site or cluster
# rare.str <- c("Willow Warbler")
# rare.str <- c("Corn Crake", "Horned Grebe", "Eagle Owl", "Lesser White-fronted Goose", "Black-tailed Godwit", "Ortolan Bunting")
# rare.str <- c("TOV-E: Trondheim 161107")
# rare.str <- c("1_kris")
rare.str <- c("Willow Warbler", 'Common Swift', 'Eurasian Blackcap', 'Barn Swallow', 'Greater Whitethroat', 'Spotted Flycatcher', 
              'European Pied Flycatcher', 'Common Cuckoo', 'Common Chiffchaff') # migrants
n.birds <- subset(n.birds, tags %in% rare.str)
summary(n.birds)
head(n.birds)

# calculate total time span of detections for this subset
min(n.birds$day)
max(n.birds$day)
time_span <- max(n.birds$detected_time)-min(n.birds$detected_time)
time_span
summary(n.birds)

# Show simple histogram of number of detections for this subset by date
hist(n.birds$day, freq=TRUE, breaks="weeks", col='darkred', xlab='Date', ylab='Detections selected species', main='Detections by date')

# create new data frame of just detection date and species
# date.count <- as.data.frame(table(n.birds$date_only),col.names = names(n.birds))
# date.count$Var1 <- as.Date(date.count$Var1)
# colnames(date.count)[1]="date"
# colnames(date.count)[2]="detections"
# date.count

###############################
# plot detections by site/cluster per day/week, shows high variation by site but this is also due to site up time/effort
# still trying to order these plots by latitude, default is alphabetical
# load working dataset to avoid constant data loading
n.birds <- read_csv("data/norway/norway.birds.8.csv")
n.ebird.cluster <- (read_csv('data/ebird/n.ebird.cluster'))
n.list <-read_csv('data/norway/n.list.csv')
n.list$day <- as.Date(n.list$upload_time)

# subset for a certain species or group of species or site or cluster
# rare.str <- c("European Pied Flycatcher")
# rare.str <- c("Corn Crake", "Horned Grebe", "Eagle Owl", "Lesser White-fronted Goose", "Black-tailed Godwit", "Ortolan Bunting")
# rare.str <- c("TOV-E: Trondheim 161107")
# rare.str <- c("1_kris")
# n.birds <- subset(n.birds, tags %in% rare.str)
# n.ebird.cluster <- subset(n.ebird.cluster, common_name %in% rare.str)
summary(n.birds)
head(n.birds)

n.birds$site_order <- reorder(n.birds$site, desc(n.birds$latitude))
p.detect.site <- ggplot(n.birds, aes(x=day, fill=cluster)) + 
  theme_bw() +
  geom_histogram (binwidth=7, color="black", show.legend = FALSE) +
  # ylim(c(0,500)) +
  xlim(c(as.Date("2022-01-01"), as.Date("2022-12-01"))) +
  # geom_smooth(formula=y~x, method='lm', color="black") +
  # theme(legend.position='none') +
  ylab("Total detections") +
  xlab("Date") +
  # theme(text=element_text(size=15)) +
  ggtitle("Total detections per week all sites Norway 2022 ordered by latitude") +
  theme(plot.title = element_text(hjust = 0.0)) +
  scale_fill_brewer(palette="Oranges") +
  facet_wrap(~site_order, ncol=7) 
p.detect.site 
ggsave(paste0("data/norway/p.detect.site.jpg"), p.detect.site, width = 16, height = 8, dpi = 300)

# plot detections by site/cluster per day/week, shows high variation by site but this is also due to site up time/effort
# still trying to order these plots by latitude, default is alphabetical
p.detect.cluster <- ggplot(n.birds, aes(x=day, fill=cluster)) + 
  theme_bw() +
  geom_histogram (binwidth=3, show.legend = FALSE, colour='black') +
  # scale_x_log10(limits=c(10,10000)) +
  # ylim(c(0,500)) +
  xlim(c(as.Date("2022-01-01"), as.Date("2022-12-01"))) +
  # geom_smooth(formula=y~x, method='lm', color="black") +
  # theme(legend.position='none') +
  ylab("Total detections") +
  xlab("Date") +
  theme(text=element_text(size=15)) +
  ggtitle('Detections per week all clusters Norway 2022 ordered by latitude') +
  theme(plot.title = element_text(hjust = 0.0)) +
  scale_fill_brewer(palette="Oranges") +
  facet_wrap(~fct_rev(cluster), drop = FALSE)
p.detect.cluster
ggsave(paste0("data/norway/p.detect.cluster.jpg"), p.detect.cluster, width = 16, height = 8, dpi = 300)

# plot detections AND observations by cluster per day, shows high variation by site but this is also due to site up time/effort
# still trying to order these plots by latitude, default is alphabetical
p.compare.cluster <- ggplot() + 
  theme_bw() +
  geom_histogram (data=n.ebird.cluster, aes(x=observation_date), alpha=0.5, fill='dodgerblue3',size= 0.1, color="black", binwidth=7, show.legend = TRUE) +
  geom_histogram (data=n.birds, aes(x=day), alpha=0.5, fill='red3', size= 0.1, color="black", binwidth=7, show.legend = TRUE) +
  # geom_histogram (data=n.list, aes(x=day), alpha=0.5, fill='darkgreen',color="black", binwidth=7, show.legend = TRUE) +
  # scale_x_log10(limits=c(10,10000)) +
  # ylim(c(0,200)) +
  xlim(c(as.Date("2022-02-01"), as.Date("2022-11-01"))) +
  # geom_smooth(formula=y~x, method='lm', color="black") +
  # theme(legend.position='none') +
  theme(text=element_text(size=20)) +
  theme(strip.text.x = element_text(face = 'bold')) +
  ylab("Detections (red) or observations (blue) per cluster per week") +
  xlab("Date") +
  scale_y_continuous(breaks=seq(0,1500,750),position = "right") +
  theme(strip.background =element_rect(fill="white"))+
  # scale_y_continuous(position = "right") +
  # ggtitle('Acoustic detections (red) and eBird observations (blue) all species and all clusters Norway 2022, scaled per cluster') +
  theme(plot.title = element_text(hjust = 0.0)) +
  facet_wrap(~fct_rev(cluster), drop = FALSE, ncol = 1) #, scales = "free_y")
p.compare.cluster 
ggsave(paste0("data/norway/p.compare.cluster.jpg"), p.compare.cluster, width = 16, height = 8, dpi = 300)

###############################
# SPECIES HEATMAP SPECIES HEATMAP SPECIES HEATMAP SPECIES HEATMAP SPECIES HEATMAP SPECIES HEATMAP SPECIES HEATMAP SPECIES HEATMAP SPECIES HEATMAP  
# SPECIES HEATMAP SPECIES HEATMAP SPECIES HEATMAP SPECIES HEATMAP SPECIES HEATMAP SPECIES HEATMAP SPECIES HEATMAP SPECIES HEATMAP SPECIES HEATMAP  
# use for loop to create data frame with number of species detections per week at each site
# each loop subsets for the site and calculate number of species detections per week
# then transforms from column to row
# adds a row name of the site
# renames columns to label w(1:51)
# binds the new row to the master data frame
# use n.birds data with 0.8 confidence and subsetted for a single species
n.birds <- read_csv("data/norway/norway.birds.8.csv")
p.over50 <- read_csv('data/norway/OVER50_detections_tom_roger_verifications_2022.csv')
p.over50$common_name <- factor(p.over50$common_name, levels = p.over50$common_name)
p.over50 <- p.over50 %>% subset(p.over50$perc_correct >= 50)

# subset for a certain species or rare species or site
# rare.str <- c("Corn Crake", "Horned Grebe", "Eagle Owl", "Lesser White-fronted Goose", "Black-tailed Godwit", "Ortolan Bunting")
# rare.str <- c("Common Cuckoo")
rare.str <- unique(p.over50$common_name)
n.birds <- subset(n.birds, tags %in% rare.str)
summary(n.birds)
unique(n.birds$tags)

# create variable of sites with decreasing latitude using the site.list.lat data frame to use in for loop
site.list.lat <- read_csv('data/norway/site.loc.lat.csv')
str_sites <- c(unique(site.list.lat$site))
str_sites
unique(n.birds$site)

# create empty data frame
n.birds.week <-data.frame()

# For loop which creates a data frame of species/selection per week for each site
for (x in str_sites) {n.site <- subset(n.birds, site==x) 
n.table <- as.data.frame(table(cut(n.site$week,breaks=seq(from=1,to=52))))
n.table<-as.data.frame(t(n.table))
n.table<-n.table[2,]
n.table[1:51] <- lapply(n.table[1:51], as.numeric)
rownames(n.table) <- x
colnames(n.table) <- sprintf("w%s",seq(1:51))
n.birds.week <- rbind(n.birds.week, n.table)
}

summary(n.birds.week)
#convert the row names to a column with sites and then delete row names
n.birds.week$site <- rownames(n.birds.week)
n.birds.week <- n.birds.week %>% relocate (site, .before=1)
rownames(n.birds.week) <- NULL
summary(n.birds.week)

# convert all the character variables to numeric (need to use the unlist function)
# unsure about this code but it works
n.birds.week[2:52] <- as.numeric(unlist(n.birds.week[2:52]))
summary(n.birds.week)
glimpse(n.birds.week)
n.birds.week$site <- as.character(n.birds.week$site)
# n.birds.weeks <- order(desc(n.birds.weeks$site))

# write weekly species detection data ordered by decreasing latitude to a csv
write_csv(n.birds.week, "data/norway/n.CommonCuckoo.week.csv")

###########################
# create heat map of files per site per week
# create a matrix from the data frame (needed for heatmap function) and then reverse the order to get high latitudes at the top
# skip next lines, pheatmap works better
# heat <- as.matrix(n.birds.week[, 2:51])
# heat <- heat[nrow(heat):1,]
# heatmap(heat, Colv = NA, Rowv = NA, scale="none", col = heat.colors(256), labRow=paste("",rev(site.list.lat$site), sep=""))

# try pheatmap function as alternative (be careful to get row data and labeling correct)
# re-order matrix not needed for pheatmap so re-read in the matrix data
n.birds.week <- read_csv('data/norway/n.CommonCuckoo.week.csv')
heat <- as.matrix(n.birds.week[, 2:52])
pheatmap(heat, cluster_cols = FALSE, cluster_rows = FALSE, breaks=c(0,1,Inf), color = rev(hcl.colors(2, "Red-Purple")), border_color = "white", labels_row=(n.birds.week$site))

###############################
# create empty data frame
n.birds.week <-data.frame()

# For loop
for (x in str_sites) {n.site <- subset(n.birds, site==x) 
n.table <- as.data.frame(table(cut(n.site$date_only,breaks=seq(from='2022-01-01',to='2022-12-31'))))
n.table<-as.data.frame(t(n.table))
n.table<-n.table[2,]
n.table[1:364] <- lapply(n.table[1:364], as.numeric)
rownames(n.table) <- x
colnames(n.table) <- sprintf("w%s",seq(1:364))
n.birds.week <- rbind(n.birds.week, n.table)
}

summary(n.birds.week)
#convert the row names to a column with sites and then delete row names
n.birds.week$site <- rownames(n.birds.week)
n.birds.week <- n.birds.week %>% relocate (site, .before=1)
rownames(n.birds.week) <- NULL
summary(n.birds.week)

#convert all the character variables to numeric (need to use the unlist function)
n.birds.week[2:52] <- as.numeric(unlist(n.birds.week[2:52]))
summary(n.birds.week)
glimpse(n.birds.week)
n.birds.week$site <- as.character(n.birds.week$site)
# n.birds.weeks <- order(desc(n.birds.weeks$site))

# write weekly species detection data ordered by decreasing latitude to a csv
write_csv(n.birds.week, "data/norway/n.CommonCuckoo.week.csv")

###########################
# create heat map of files per site per week
# create a matrix from the data frame (needed for heatmap function) and then reverse the order to get high latitudes at the top
# skip next lines, pheatmap works better
# heat <- as.matrix(n.birds.week[, 2:51])
# heat <- heat[nrow(heat):1,]
# heatmap(heat, Colv = NA, Rowv = NA, scale="none", col = heat.colors(256), labRow=paste("",rev(site.list.lat$site), sep=""))

# try pheatmap function as alternative (be careful to get row data and labeling correct)
# re-order matrix not needed for pheatmap so re-read in the matrix data
heat <- as.matrix(n.birds.week[, 2:52])
pheatmap(heat, cluster_cols = FALSE, cluster_rows = FALSE, breaks=c(0,1,Inf), color = rev(hcl.colors(2, "Mint")), border_color = "white", labels_row=(n.birds.week$site))

###########################
n.join <- read_csv("data/norway/norway.birds.8.csv")
c.join <- read_csv('data/norway/clust.files.full.csv')
c.join <- c.join[, c(1,6)]
length(n.join$site)
n.join <- left_join(n.join, c.join, by = 'site')
length(n.join$site)
# move cluster to after site
n.join <- n.join %>% relocate (cluster, .after=site)
n.birds <- n.join
summary(n.birds)

###########################
# END

