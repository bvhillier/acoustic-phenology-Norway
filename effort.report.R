# script to assess variation in up-time (effort) for all recorders in Norway 2022
# run packages.R script to install packages and load libraries

# load effort list data, file with all the audio bugg files recorded at each site throughout 2022
# data downloaded from app.bugg.xyz website filtered for 20222
n.list <- read_csv("data/norway/norway.bugg.audio.list.csv")
summary(n.list)

###########################
# show table of audio files per site ordered by decreasing name in the data set
# and write to a csv file
n.locs <- read_csv("data/norway/site.loc.lat.csv")
site.list<- as.data.frame(table(n.list$site))
colnames(site.list) <- c("sites", "files")
site.list$lat <- (n.locs$lat)
site.list$long <- (n.locs$long)
site.list <- arrange(site.list, desc(sites))
site.list.name <- site.list
knitr::kable(site.list.name,col.names=c("Site","Files", "Lat", "Long"),caption = "Site Effort")

# write the files per site ordered by site name in decreasing order to csv
write_csv(site.list.name, 'data/norway/site.list.name.csv')

###########################
# show table of audio files per site ordered by decreasing latitude in the data set
# and write to a csv file
site.list <- as.data.frame(table(n.list$site))
colnames(site.list) <- c("sites", "files")
site.list <- arrange(site.list, desc(sites))
site.list
site.list$lat <- (n.locs$lat)
site.list$long <- (n.locs$long)
site.list <- arrange(site.list, desc(lat))
site.list
site.list.lat <- site.list
knitr::kable(site.list.lat,col.names=c("Site","Files", "Lat", "Long"),caption = "Site Effort")

# write the files per site ordered by latitude in decreasing order to csv
write_csv(site.list.lat, 'data/norway/site.list.lat.csv')

###########################
# Bar plot of files per site (effort) in decreasing order
site.list$sites <- factor(site.list$sites, levels = site.list$sites[order(site.list$files)])
p.effort.total <-ggplot(data=site.list, aes(x=sites, y=files)) +
  geom_col(colour='red') +
  xlab("Site by latitude") +
  ylab("No. 5 min audio files (effort)") +
  ggtitle("Effort analysis for all sites Norway 2022") +
  coord_flip()
p.effort.total

###########################
# Bar plot of files per site (effort) by latitude
site.list$sites <- factor(site.list$sites, levels = site.list$sites[order(site.list$lat)])
p.effort.lat <-ggplot(data=site.list, aes(x=sites, y=files)) +
  geom_col(colour='green') +
  xlab("Site by total") +
  ylab("No. 5 min audio files (effort)") +
  ggtitle("Effort analysis for all sites Norway 2022") +
  coord_flip()
p.effort.lat

###########################
# Show simple histogram of number of audio files by upload date
n.list$upload_time <- as.POSIXct(n.list$upload_time, format="%d/%m/%Y %H:%M") 
n.list$created_time <- as.POSIXct(n.list$created_time, format="%d/%m/%Y %H:%M") 
n.list$hour_only <- as.ITime(n.list$upload_time, format="%H") 
n.list <- n.list %>% relocate (time_only, .after=upload_time)
n.list$hour_only <- hour(n.list$hour_only)
summary(n.list)
n.list <- n.list[!is.na(n.list$upload_time), ]
hist(n.list$upload_time, freq=TRUE, breaks="days", col='blue', xlab='Date', ylab='Total audio files', main='Total files by upload date')

###########################
# create a calendar week variable from date_only variable THIS IS GOOD ONE
n.list$week <- week(n.list$upload_time)
unique(n.list$week)
n.list <- n.list %>% relocate (week, .after=upload_time)

###########################
# add a cluster variable to the dataset, uses the clust.files.full csv to match cluster to site names
n.join <- n.list
c.join <- read_csv('data/norway/clust.files.full.csv')
c.join <- c.join[, c(1,6)]
length(n.join$site)
n.join <- left_join(n.join, c.join, by = 'site')
length(n.join$site)
# move cluster to after site
n.join <- n.join %>% relocate (cluster, .after=site)
n.list <- n.join
summary(n.list)

# write n.list to csv, this is the key file for effort data
write_csv(n.list, 'data/norway/n.list.csv')
###########################

hist(n.list$time_only)

###########################
# use ggplot facet wrap to show audio files by time at each site
n.list <- read_csv('data/norway/n.list.csv')
n.list$day <- as.Date(n.list$upload_time)
n.list$time_only <- as.POSIXct(n.list$time_only)
n.list$hour <- as.numeric(cut(n.list$time_only, breaks = "1 hour"))
length(unique(n.list$hour))
n.list$site <- str_replace_all(n.list$site,'Finmark', 'Finnmark')

# CANNOT ORDER PLOTS BY LATITUDE, get error even though lengths are actually the same (28)
n.list$site_order <- reorder(n.list$site, desc(n.list$latitude))
p.files.time <- ggplot(n.list, aes(x=hour, fill=cluster)) +
  theme_bw() +
  geom_histogram(binwidth=1, show.legend = TRUE, colour='black', boundary = FALSE) +
  ylab("Total audio files per week") +
  xlab("Hour of day") +
  xlim(c(0,24)) +
  theme(strip.background = element_rect(fill="white"))+
  # ggtitle("Audio files by hour of the day Norway all sites 2022, ordered by latitude") +
  theme(plot.title = element_text(hjust = 0.0)) +
  theme(strip.text.x = element_text(size = 14)) +
  # scale_y_continuous(breaks=seq(0,2500,2500)) +
  # scale_x_continuous(breaks=seq(0,24,6)) +
  theme(text=element_text(size=20)) +
  facet_wrap(~site_order, ncol=7) +
  scale_fill_brewer(palette="Greens") +
  guides(fill = guide_legend(reverse=T))
p.files.time
ggsave(paste0("data/norway/arrivals/p.files.time.jpg"), p.files.time, width = 16, height = 8, dpi = 300)


length(unique(n.list$site))
length(unique(site.list.lat$lat))

###########################
# use ggplot facet wrap to show audio files per week ordered by latitude
# CANNOT ORDER PLOTS BY LATITUDE, get error even though lengths are actually the same (28)
p.files.sites <- ggplot(n.list, aes(x=day, fill=cluster)) +
  theme_bw() +
  geom_histogram (binwidth=7, show.legend = FALSE, colour='black', boundary = FALSE) +
  # geom_density()
  ylab("Total audio files recorded per week") +
  xlab("Date") +
  theme(strip.background = element_rect(fill="white"))+
  # ggtitle("Audio files by week for Norway all sites 2022, ordered by latitude") +
  theme(plot.title = element_text(hjust = 0.0)) +
  theme(text=element_text(size=17)) +
  facet_wrap(~site_order, ncol=7) +
  scale_fill_brewer(palette="Greens")
p.files.sites
ggsave(paste0("data/norway/arrivals/p.files.sites.jpg"), p.files.sites, width = 16, height = 8, dpi = 300)


length(unique(n.list$site))
length(unique(site.list.lat$lat))

###########################
# use ggplot facet wrap to show audio files per week ordered by latitude
# CANNOT ORDER PLOTS BY LATITUDE, get error even though lengths are actually the same (28)
p.files.clusters <- ggplot(n.list, aes(x=day, fill=cluster)) +
  theme_bw() +
  geom_histogram (binwidth=7, show.legend = FALSE, colour='black', boundary = FALSE) +
  # geom_density()
  ylab("Total audio files per cluster per week") +
  xlab("Date") +
  scale_y_continuous(breaks=seq(0,10000,5000)) +
  xlim(c(as.Date("2022-02-01"), as.Date("2022-11-01"))) +
  # ggtitle("Audio files by week for Norway all clusters 2022, ordered by latitude") +
  theme(plot.title = element_text(hjust = 0.0)) +
  theme(text=element_text(size=20)) +
  theme(strip.text.x = element_text(face = 'bold', size = 15)) +
  theme(strip.background =element_rect(fill="white"))+
  facet_wrap(~fct_rev(cluster), ncol=1) +
  scale_fill_brewer(palette="Greens")
p.files.clusters
ggsave(paste0("data/norway/arrivals/p.files.clusters.jpg"), p6, width = 16, height = 8, dpi = 300)


length(unique(n.list$site))
length(unique(site.list.lat$lat))

###########################
# create new data frame with number of files per week at each site using for loop
# create empty data frame
n.list.weeks <-data.frame()

# read in file dataset
n.list <- read_csv('data/norway/n.list.csv')

# create variable of sites with decreasing latitude using the site.list.lat data frame to use in for loop
str_sites <- c(unique(n.list$site))
str_sites

# use for loop to create data frame with number of files per week at each site
# each loop subsets for the site and calculate number of files per week
# then transforms from column to row
# adds a rowname of the site
# renames columns to label 1:51
# binds the new row to the master data frame
for (x in str_sites) {n.site <- subset(n.list, site==x) 
n.table <- as.data.frame(table(cut(n.site$week,breaks=seq(from=1,to=52))))
n.table<-as.data.frame(t(n.table))
n.table<-n.table[2,]
rownames(n.table) <- x
# change column names to w followed by week number
colnames(n.table) <- sprintf("w%s",seq(1:51))
# change week variable to numeric
n.list.weeks$w1 <- as.numeric(n.list.weeks$w1)
n.list.weeks <- rbind(n.list.weeks, n.table)
}

#convert the row names to a column with sites and then delete row names
n.list.weeks$site <- rownames(n.list.weeks)
n.list.weeks <- n.list.weeks %>% relocate (site, .before=1)
rownames(n.list.weeks) <- NULL
summary(n.list.weeks)

#convert all the character variables to numeric (need to use the unlist function)
n.list.weeks[2:52] <- as.numeric(unlist(n.list.weeks[2:52]))
summary(n.list.weeks)
glimpse(n.list.weeks)
n.list.weeks$site <- as.character(n.list.weeks$site)
# n.list.weeks <- order(desc(n.list.weeks$site))

# write effort summary data ordered by decreasing latitude to a csv
write_csv(n.list.weeks, "data/norway/n.effort.csv")

###########################
# create heat map of files per site per week
# create a matrix from the data frame (needed for heatmap function) and then reverse the order to get high latitudes at the top
n.list.weeks <- read_csv("data/norway/n.effort.csv")

heat <- as.matrix(n.list.weeks[, 2:51])
heat <- heat[nrow(heat):1,]
heatmap(heat, Colv = NA, Rowv = NA, scale="none", col = heat.colors(256), labRow=paste("",rev(site.list.lat$site), sep=""))

# try pheatmap function as alternative (be careful to get row data and labeling correct)
# re-order matrix not needed for pheatmap so re-read in the matrix data
heat <- as.matrix(n.list.weeks[, 2:51])
pheatmap(heat, cluster_cols = FALSE, cluster_rows = FALSE, color = rev(hcl.colors(50, "Emrld")), border_color = "white", labels_row=(site.list.lat$site))

ggplot(n.birds, aes(x=date_only)) + 
  geom_histogram (binwidth=7, show.legend = FALSE) +
  # scale_x_log10(limits=c(10,10000)) +
  # ylim(c(0,8)) +
  # geom_smooth(formula=y~x, method='lm', color="black") +
  # theme(legend.position='none') +
  ylab("Total detections") +
  xlab("Date") +
  ggtitle("Detections by date for all Norway sites 2022") +
  theme(plot.title = element_text(hjust = 0.5))

###########################
length(unique(n.birds$audio_link))
length(n.birds$tags)



# END
     