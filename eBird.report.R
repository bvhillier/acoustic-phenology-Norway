# # eBird status download package and access code (not needed for eBird data analysis)
# # if (!requireNamespace("remotes", quietly = TRUE)) {
# #   install.packages("remotes")
# # }
# # remotes::install_github("ebird/ebirdst")
# # library(ebirdst)
# # set_ebirdst_access_key("vu9r9iqobhnv")
# 
# # script to analyse Norway eBird data downloaded from eBird website
# # this bit is for ebird status and trends, not for regular ebd database extraction
# # library(ebirdst)
# # set_ebirdst_access_key("vu9r9iqobhnv")
# # this is for ebd extraction
# # library(auk)
# 
# ###############################
# # code to extract eBird observations at each cluster from the 2022 eBird database
# # uses the box co-ordinates in the cluster csv
# # extract all or specific species
# 
# # NORWAY
# # assign ebd text file, file is accessed but not read in
# input_file <- 'data/eBird/ebd_NO_202201_202212_relJan-2023.txt'
# # assign output ebd text file after filters have been applied
# output_file <- "data/eBird/norway.cluster.out.txt"
# # read in n.clust.full for the for loop
# n.clust.full <- read_csv('data/norway/n.clust.full.csv')
# # create empty data frame for rbinding the eBird data from different clusters
# n.ebird.cluster <- data_frame()
# 
# # for loop goes through the cluster file and extracts eBird data from the assigned cluster radius box (40x40km)
# # adds each cluster to a single file and adds a column variable for the cluster
# for (x in 1:length(n.clust.full$cluster)) {
# # set-up the filters to apply and then write filtered results to the output file
# ebird.filter <- input_file %>% 
#   # 1. reference file
#   auk_ebd() %>% 
#   # 2. define filters
#   # auk_species(species = c("Willow Warbler")) %>% 
#   auk_bbox(bbox = as.numeric(c(n.clust.full[x,5], n.clust.full[x,6], n.clust.full[x,7], n.clust.full[x,8]))) %>% 
#   auk_duration(duration = c(0,300)) %>%
#   auk_distance(distance = c(0,5)) %>%
#   # 3. run filtering
#   auk_filter(file = output_file, overwrite=TRUE) %>% 
#   # 4. read text file into r data frame
#   read_ebd()
#   ebird.filter$cluster <- n.clust.full$cluster[x]
#   n.ebird.cluster <- rbind(n.ebird.cluster, ebird.filter)
# }
# n.ebird.cluster <- n.ebird.cluster %>% relocate (cluster, .after=locality)
# 
# # write filtered ebird data to csv file
# write_csv(n.ebird.cluster, 'data/ebird/n.eBird.cluster')


###############################
n.ebird.cluster <- read_csv('data/ebird/n.eBird.cluster')

# subset for a particular cluster or set of clusters or species
n.ebird.cluster <- subset(n.ebird.cluster, common_name =='Willow Warbler')

# plot total ebird observations by cluster, northern locations have very few observations
ggplot(n.ebird.cluster, aes(x=observation_date, color=cluster)) + 
  geom_histogram (binwidth=1, show.legend = TRUE) +
  # scale_x_log10(limits=c(10,10000)) +
  # ylim(c(0,200)) +
  xlim(c(as.Date("2022-01-01"), as.Date("2022-12-01"))) +
  # geom_smooth(formula=y~x, method='lm', color="black") +
  theme(legend.position='none') +
  ylab("Observations") +
  xlab("Date") +
  ggtitle("Observations") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~fct_rev(cluster), drop = FALSE) # scales = "free_y")

###############################
# show table of detections per species in the data set in descending order
e.common <- as.data.frame(table(ebird.filter$common_name))
e.common <- arrange(e.common, desc(Freq))
length(e.common$Var1)
knitr::kable(e.common,col.names=c("Species","Observations"),caption = "Observations")

# show table of detections per locality in the data set in descending order
e.locality <- as.data.frame(table(ebird.filter$locality))
e.locality <- arrange(e.locality, desc(Freq))
length(e.locality$Var1)
knitr::kable(e.locality, col.names=c("Locality","No. of detections"),caption = "Locality Detections")

###############################
# show table of detections per state in the data set in descending order
e.state <- as.data.frame(table(ebird.filter$state))
e.state <- arrange(e.state, desc(Freq))
length(e.state$Var1)
knitr::kable(e.state, col.names=c("State","eBird detections"),caption = "eBird Detections 2022")

# show table of detections per locality type in the data set in descending order
e.type <- as.data.frame(table(ebird.filter$locality_type))
e.type<- arrange(e.type, desc(Freq))
length(e.type$Var1)
knitr::kable(e.type, col.names=c("State","eBird detections"),caption = "eBird Detections 2022")

###############################
# create file of eBird location in lat/long for plotting on My Maps
# number of unique sites - way too many, need to reduce, try number of observations (common_name) = 50 & 100, or distance from recorder site
length(unique(n.ebird$locality))
length(n.ebird$locality)

###############################
# plot observations by cluster per day/week, shows high variation by site but this is also due to site up time/effort
# still trying to order these plots by latitude, default is alphabetical
# load working dataset to avoid constant data loading
n.birds <- read_csv("data/norway/norway.birds.8.csv")

# subset for a certain species or group of species or site or cluster
rare.str <- c("Willow Warbler")
# rare.str <- c("Corn Crake", "Horned Grebe", "Eagle Owl", "Lesser White-fronted Goose", "Black-tailed Godwit", "Ortolan Bunting")
# rare.str <- c("TOV-E: Trondheim 161107")
# rare.str <- c("1_kris")
n.birds <- subset(n.birds, tags %in% rare.str)
summary(n.birds)
head(n.birds)


ggplot(n.birds, aes(x=week, color=site)) + 
  geom_histogram (binwidth=1, show.legend = FALSE) +
  # scale_x_log10(limits=c(10,10000)) +
  xlim(c(1,51)) +
  # geom_smooth(formula=y~x, method='lm', color="black") +
  # theme(legend.position='none') +
  ylab("Total detections") +
  xlab("Date") +
  ggtitle("Detections Norway 2022") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~site)

# plot detections by site/cluster per day/week, shows high variation by site but this is also due to site up time/effort
# still trying to order these plots by latitude, default is alphabetical
ggplot(n.birds, aes(x=date_only, color=cluster)) + 
  geom_histogram (binwidth=1, show.legend = FALSE) +
  # scale_x_log10(limits=c(10,10000)) +
  # ylim(c(0,200)) +
  xlim(c(as.Date("2022-01-01"), as.Date("2022-07-01"))) +
  # geom_smooth(formula=y~x, method='lm', color="black") +
  # theme(legend.position='none') +
  ylab("Detections") +
  xlab("Date") +
  ggtitle("Detections Norway 2022") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~fct_rev(cluster), drop = FALSE, scales = "free_y")

###########################
# use the effort.R script as template to correct eBird data for effort using simple frequency method
n.ebird.cluster <- read_csv('data/ebird/n.eBird.cluster')


# # this code creates a complete lists (checklist_id) per day variable from data frame of lists made at each cluster
# # this allows correction to be made for effort/uptime
# # create count of lists per day using aggregate function
# # then use inner join to add lists per day variable to n.ebird dataframe
# 
# # create new data frame with counts of observations per day and species
n.ebird.clust.day <- aggregate(list(cluster = n.ebird.cluster$cluster), by = list(day = n.ebird.cluster$observation_date, checklists = n.ebird.cluster$cluster), length)

# rename first column to match desired name
names(n.ebird.clust.day)[1] <- "day"
names(n.ebird.clust.day)[2] <- "cluster"
names(n.ebird.clust.day)[3] <- "checklists"

# # print new data frame
# n.list.clust.day
# # write this file to csv for use later using left_join to add a column to n.birds whenever data and site match
write_csv(n.ebird.clust.day, 'data/ebird/n.ebird.clust.day.csv')




