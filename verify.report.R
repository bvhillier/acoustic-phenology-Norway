# script to analyse the verification of BirdNet detections performed by Tom Roger
# read/load packages from packages.R first

# read in the two files, one with over 50 detections and one with under 50
n.under50 <- read_csv('data/norway/under50.csv')
n.over50 <- read_csv('data/norway/tom_roger_annotations_yr2_2022.csv')

# rename columns to more R friendly
names(n.under50)[2] <- "species"
names(n.over50)[2] <- "species"
names(n.under50)[9] <- "correct"
names(n.over50)[9] <- "correct"

##################################
# for loop goes through the detection file and counts/calculates metrics
p.over50 <- data.frame()
list <- unique(n.over50$species)
list
x=3
for (x in 1:length(unique(n.over50$species))) { 
  p.percent <- subset(n.over50, species==list[x])
  percent <- length(which(p.percent$correct == 'Yes')) / length(p.percent$species) * 100
  percent <- round(percent, 0)
  verify <- sum(!is.na(p.percent$correct))
  count <- as.integer(length(p.percent$species))
  species <- p.percent[1,2]
  inter <- as.data.frame(c(species, count, verify, percent))
  colnames(inter) <- c('common_name', 'num_detects', 'num_assessed', 'perc_correct')
  p.over50 <- rbind(p.over50, inter)
}
p.over50 <- arrange(p.over50, desc(perc_correct), desc(num_assessed))
p.over50
# write output to csv
write_csv(p.over50, 'data/norway/OVER50_detections_tom_roger_verifications_2022.csv')

# p.over50 <- read_csv('data/norway/OVER50_percent_detections_tom_roger_verifications_2022.csv')
# p.over50 <- read_csv('data/norway/UNDER50_detections_tom_roger_verifications_2022.csv')
p.over50 <- read_csv('data/norway/OVER50_detections_tom_roger_verifications_2022.csv')

# plot results
p.over50$common_name <- factor(p.over50$common_name, levels = p.over50$common_name)
p.over50 <- p.over50 %>% subset(p.over50$perc_correct >= 80)
p.over50 <- p.over50 %>% subset(p.over50$perc_correct < 80)
p.over50 <- p.over50 %>% subset(p.over50$num_assessed > 0)
color_scheme <- ifelse(p.over50$common_name %in% c("Willow Warbler", 'Common Swift', 'Eurasian Blackcap', 'Barn Swallow', 'Greater Whitethroat', 'Spotted Flycatcher', 
                                                 'European Pied Flycatcher', 'Common Cuckoo', 'Common Chiffchaff', 'Tree Pipit'), 
                                                 "steelblue4", "seagreen3")
p.verify <- ggplot(data=p.over50, aes(x=common_name, y=perc_correct)) +
  theme_bw() +
  geom_bar(stat="identity", color="white", fill=color_scheme) +
  geom_point(data=p.over50, aes(x=common_name, y=num_assessed), stat="identity", color='black', shape='square', size = 3.0) +
  theme(axis.text.x = element_text(size=20, angle=70,hjust=1)) +
  theme(text=element_text(size=20)) +
  theme(axis.text.y = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  scale_y_continuous(breaks=seq(0,100,by=20), sec.axis = dup_axis()) +
  labs(x = "", y = "Accuracy % (bar), Number of assessments (square)")
  # ggtitle("Verification results for Norway BirdNet detections 2022. Species over 50% correct. Performed by Tom Roger")
p.verify

p.verify <- ggplot(data=p.over50, aes(x=common_name, y=perc_correct)) +
  theme_bw() +
  geom_bar(stat="identity", color="white", fill='seagreen3') +
  geom_point(data=p.over50, aes(x=common_name, y=num_assessed), stat="identity", color='black', shape='square', size = 3.0) +
  theme(axis.text.x = element_text(size=20, angle=70,hjust=1)) +
  theme(text=element_text(size=20)) +
  theme(axis.text.y = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  scale_y_continuous(breaks=seq(0,100,by=20), sec.axis = dup_axis()) +
  labs(x = "", y = "Accuracy % (bar), Number of assessments (square)")
# ggtitle("Verification results for Norway BirdNet detections 2022. Species over 50% correct. Performed by Tom Roger")
p.verify

p.verify <- ggplot(data=p.over50, aes(x=common_name, y=perc_correct)) +
  theme_bw() +
  geom_bar(stat="identity", color="white", fill=color_scheme) +
  geom_point(data=p.over50, aes(x=common_name, y=num_assessed), stat="identity", color='black', shape='square', size = 3.0) +
  theme(axis.text.x = element_text(size=20, angle=70,hjust=1)) +
  theme(text=element_text(size=20)) +
  theme(axis.text.y = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  scale_y_continuous(breaks=seq(0,100,by=20), sec.axis = dup_axis()) +
  labs(x = "", y = "Accuracy % (bar), Number of assessments (square)")
# ggtitle("Verification results for Norway BirdNet detections 2022. Species over 50% correct. Performed by Tom Roger")
p.verify

ggsave(paste0("data/norway/verify_OVER50.jpg"), p.verify, width = 16, height =4, dpi = 300)

##################################
# for loop goes through the detection file and counts/calculates metrics
p.under50 <- data.frame()
list <- unique(n.under50$species)
list
for (x in 1:length(unique(n.under50$species))) { 
  p.percent <- subset(n.under50, species==list[x])
  percent <- length(which(p.percent$correct == 'Yes')) / length(p.percent$species) * 100
  percent <- round(percent, 0)
  verify <- sum(!is.na(p.percent$correct))
  count <- as.integer(length(p.percent$species))
  species <- p.percent[1,2]
  inter <- as.data.frame(c(species, count, verify, percent))
  colnames(inter) <- c('common_name', 'num_detects', 'num_assessed', 'perc_correct')
  p.under50 <- rbind(p.under50, inter)
}
p.under50 <- arrange(p.under50, desc(perc_correct), desc(num_assessed))
p.under50
# write output to csv
write_csv(p.under50, 'data/norway/UNDER50_detections_tom_roger_verifications_2022.csv')



##############################
##############################

# script to analyse the verification of Taiwan detections performed by xxx
# read/load packages from packages.R first

# read in the two files, one with over 50 detections and one with under 50
# t.under50 <- read_csv('data/norway/under50.csv')
t.over50 <- read_csv('data/taiwan/Birdnet_identification_TW_20230224.csv')

# rename columns to more R friendly
# names(n.under50)[2] <- "species"
names(t.over50)[2] <- "species"
# names(n.under50)[9] <- "correct"
names(t.over50)[9] <- "correct"

##################################
# for loop goes through the detection file and counts/calculates metrics
p.over50 <- data.frame()
list <- unique(t.over50$species)
list
x=3
for (x in 1:length(unique(t.over50$species))) { 
  p.percent <- subset(t.over50, species==list[x])
  percent <- length(which(p.percent$correct == 'Yes')) / length(p.percent$species) * 100
  percent <- round(percent, 0)
  verify <- sum(!is.na(p.percent$correct))
  count <- as.integer(length(p.percent$species))
  species <- p.percent[1,2]
  inter <- as.data.frame(c(species, count, verify, percent))
  colnames(inter) <- c('common_name', 'num_detects', 'num_assessed', 'perc_correct')
  p.over50 <- rbind(p.over50, inter)
}
p.over50 <- arrange(p.over50, desc(perc_correct), desc(num_assessed), (common_name))
p.over50

# write output to csv
write_csv(p.over50, 'data/norway/OVER50_detections_taiwan_verifications_2023.csv')

# plot results
p.over50$common_name <- factor(p.over50$common_name, levels = p.over50$common_name)
ggplot(data=p.over50, aes(x=common_name, y=perc_correct)) +
  geom_bar(stat="identity", color="white", fill="firebrick") +
  theme(axis.text.x = element_text(size=10, angle=90,hjust=0.95,vjust=0.2)) +
  scale_y_continuous(breaks=seq(0,100,by=10), sec.axis = dup_axis()) +
  labs(x = "Species", y = "Percentage verifications correct") +
  ggtitle("Verification results for Taiwan BirdNet detections. Performed by Ming-Yuan, 50 detections/verifications per species")


##################################
##################################

