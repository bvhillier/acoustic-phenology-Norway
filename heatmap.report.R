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
# site.list.lat <- read_csv('data/norway/site.loc.lat.csv')
site.list.lat <- read_csv('data/norway/cluster.full.csv')
str_sites <- c(unique(site.list.lat$cluster))
str_sites
unique(n.birds$cluster)

# create empty data frame
n.birds.week <-data.frame()

# For loop which creates a data frame of species/selection per week for each site
for (x in str_sites) {n.site <- subset(n.birds, cluster==x) 
n.table <- as.data.frame(table(cut(n.site$week,breaks=seq(from=1,to=52))))
n.table<-as.data.frame(t(n.table))
n.table<-n.table[2,]
n.table[1:51] <- lapply(n.table[1:51], as.numeric)
rownames(n.table) <- x
colnames(n.table) <- sprintf("W%s",seq(1:51))
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
cluster_labels <- rev(n.birds.week$site)
cluster_labels <- c('Finnmark2', 'Finnmark1', 'Tromsø', 'Trondheim', 'Lillehammer', 'Bergen', 'Østfold', 'Stavanger', 'Kristiansand')
# n.birds.week <- as.data.frame(apply(n.birds.week, 2, rev))

# write weekly species detection data ordered by decreasing latitude to a csv
# write_csv(n.birds.week, "data/norway/n.CommonCuckoo.week.csv")

###########################
# create heat map of files per site per week
# create a matrix from the data frame (needed for heatmap function) and then reverse the order to get high latitudes at the top
# skip next lines, pheatmap works better
# heat <- as.matrix(n.birds.week[, 2:51])
# heat <- heat[nrow(heat):1,]
# heatmap(heat, Colv = NA, Rowv = NA, scale="none", col = heat.colors(256), labRow=paste("",rev(site.list.lat$site), sep=""))

# try pheatmap function as alternative (be careful to get row data and labeling correct)
# re-order matrix not needed for pheatmap so re-read in the matrix data
# n.birds.week <- read_csv('data/norway/n.CommonCuckoo.week.csv')
heat <- as.matrix(n.birds.week[, 2:52])
heat_rev <- heat[nrow(heat):1, ]
p.heatmap.all <- pheatmap(heat_rev, cluster_cols = FALSE, cluster_rows = FALSE, breaks=c(0,1,Inf), color = rev(hcl.colors(2, 'OrRd')), border_color = "lightgray", 
                          labels_row=(cluster_labels),fontsize = 10, angle_col = 45, cellheight=20, cellwidth=20)
p.heatmap.all <- pheatmap(heat_rev, cluster_cols = FALSE, cluster_rows = FALSE, breaks=c(0,1,Inf), color = rev(hcl.colors(2, 'PuBu')), border_color = "lightgray", 
                          labels_row=(cluster_labels),fontsize = 10, angle_col = 45, cellheight=20, cellwidth=20)
p.heatmap.all <- pheatmap(heat_rev, cluster_cols = FALSE, cluster_rows = FALSE, breaks=c(0,50,100,150,200,250,Inf), color = rev(hcl.colors(7, 'Mint')), border_color = "lightgray", 
         labels_row=(cluster_labels),fontsize = 10, angle_col = 45, cellheight=20, cellwidth=20)
p.heatmap.all <- pheatmap(heat_rev, cluster_cols = FALSE, cluster_rows = FALSE, breaks=c(0,100,200,300,400,500,Inf), color = rev(hcl.colors(7, 'Mint')), border_color = "lightgray", 
                          labels_row=(cluster_labels),fontsize = 10, angle_col = 45, cellheight=20, cellwidth=20)
p.heatmap.all
ggsave(paste0("data/norway/p.heatmap.all.jpg"), p.heatmap.all, width = 16, height = 4, dpi = 300)

###############################

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
rare.str <- c('Willow Warbler', 'Common Swift', 'Eurasian Blackcap', 'Greater Whitethroat', 'Spotted Flycatcher', 
  'European Pied Flycatcher', 'Common Cuckoo', 'Common Chiffchaff')
# rare.str <- c("Willow Warbler")
# rare.str <- unique(p.over50$common_name)
n.birds <- subset(n.birds, tags %in% rare.str)
summary(n.birds)
unique(n.birds$tags)

# create variable of sites with decreasing latitude using the site.list.lat data frame to use in for loop
# site.list.lat <- read_csv('data/norway/site.loc.lat.csv')
site.list.lat <- read_csv('data/norway/cluster.full.csv')
str_sites <- c(unique(site.list.lat$cluster))
str_sites
unique(n.birds$cluster)

# create empty data frame
n.birds.week <-data.frame()

# For loop which creates a data frame of species/selection per week for each site
for (x in str_sites) {n.site <- subset(n.birds, cluster==x) 
n.table <- as.data.frame(table(cut(n.site$week,breaks=seq(from=1,to=52))))
n.table<-as.data.frame(t(n.table))
n.table<-n.table[2,]
n.table[1:51] <- lapply(n.table[1:51], as.numeric)
rownames(n.table) <- x
colnames(n.table) <- sprintf("W%s",seq(1:51))
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
cluster_labels <- rev(n.birds.week$site)
cluster_labels <- c('Finnmark2', 'Finnmark1', 'Tromsø', 'Trondheim', 'Lillehammer', 'Bergen', 'Østfold', 'Stavanger', 'Kristiansand')
# n.birds.week <- as.data.frame(apply(n.birds.week, 2, rev))

# write weekly species detection data ordered by decreasing latitude to a csv
# write_csv(n.birds.week, "data/norway/n.CommonCuckoo.week.csv")

###########################
# create heat map of files per site per week
# create a matrix from the data frame (needed for heatmap function) and then reverse the order to get high latitudes at the top
# skip next lines, pheatmap works better
# heat <- as.matrix(n.birds.week[, 2:51])
# heat <- heat[nrow(heat):1,]
# heatmap(heat, Colv = NA, Rowv = NA, scale="none", col = heat.colors(256), labRow=paste("",rev(site.list.lat$site), sep=""))

# try pheatmap function as alternative (be careful to get row data and labeling correct)
# re-order matrix not needed for pheatmap so re-read in the matrix data
# n.birds.week <- read_csv('data/norway/n.CommonCuckoo.week.csv')
heat <- as.matrix(n.birds.week[, 2:52])
heat_rev <- heat[nrow(heat):1, ]
p.heatmap.all <- pheatmap(heat_rev, cluster_cols = FALSE, cluster_rows = FALSE, breaks=c(0,1,10,20,30,40,50, Inf), color = rev(hcl.colors(7, 'Blues')), border_color = "lightgray", 
                          labels_row=(cluster_labels),fontsize = 15, angle_col = 45, cellheight=50, cellwidth=30)
p.heatmap.all <- pheatmap(heat_rev, cluster_cols = FALSE, cluster_rows = FALSE, breaks=c(0,1,Inf), color = rev(hcl.colors(2, 'PuBu')), border_color = "lightgray", 
                          labels_row=(cluster_labels),fontsize = 10, angle_col = 45, cellheight=20, cellwidth=20)
p.heatmap.all <- pheatmap(heat_rev, cluster_cols = FALSE, cluster_rows = FALSE, breaks=c(0,50,100,150,200,250,Inf), color = rev(hcl.colors(7, 'Mint')), border_color = "lightgray", 
                          labels_row=(cluster_labels),fontsize = 10, angle_col = 45, cellheight=20, cellwidth=20)
p.heatmap.all <- pheatmap(heat_rev, cluster_cols = FALSE, cluster_rows = FALSE, breaks=c(0,25,50,75,100,125,Inf), color = rev(hcl.colors(7, 'Mint')), border_color = "lightgray", 
                          labels_row=(cluster_labels),fontsize = 10, angle_col = 45, cellheight=20, cellwidth=20)
p.heatmap.all
ggsave(paste0("data/norway/p.heatmap.all.jpg"), p.heatmap.all, width = 16, height = 4, dpi = 300)
breaks=c(0,10,20,30,40,50, Inf)

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
