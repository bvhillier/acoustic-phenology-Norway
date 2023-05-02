# script to create a locationa overview map
# Load the required libraries
library(ggplot2)
library(maps)

# over complicated code to read in the cluster box cords and reformat to plot them as polygons
n.ebird.cluster <- read_csv('data/ebird/n.eBird.cluster')
boxes <- read_csv('data/norway/n.clust.full.csv')
boxes <- boxes[,-2]
colnames(boxes)[4:7] <- c('lon1','lat1', 'lon3', 'lat3')
# add the other corner points
boxes$lat2 <- boxes$lat3
boxes$lon2 <- boxes$lon1
boxes$lon4 <- boxes$lon3
boxes$lat4 <- boxes$lat1
boxes <- boxes %>% relocate (lon2, .after=lat1)
boxes <- boxes %>% relocate (lat2, .after=lon2)
# remove the unneeded columns
boxes <- boxes[,c(-2,-3)]

cluster_str <- unique(boxes$cluster)
# code to reformat the coords for plotting, probably a much easier way
box_plot <- box_plot_all <- box_plot2<- box_plot3<- box_plot4<- box_plot5 <- data_frame() 
for (x in 1:length(cluster_str)){
    box_plot[1,1] <- boxes[x,1]
    box_plot[1,2] <- boxes[x,2]
    box_plot[1,3] <- boxes[x,3]
    colnames(box_plot) <- c('cluster','lon', 'lat')
    box_plot2[1,1] <- boxes[x,1]
    box_plot2[1,2] <- boxes[x,4]
    box_plot2[1,3] <- boxes[x,5]
    colnames(box_plot2) <- c('cluster','lon', 'lat')
    box_plot3[1,1] <- boxes[x,1]
    box_plot3[1,2] <- boxes[x,6]
    box_plot3[1,3] <- boxes[x,7]
    colnames(box_plot3) <- c('cluster','lon', 'lat')
    box_plot4[1,1] <- boxes[x,1]
    box_plot4[1,2] <- boxes[x,8]
    box_plot4[1,3] <- boxes[x,9]
    colnames(box_plot4) <- c('cluster','lon', 'lat')
    box_plot5 <- rbind(box_plot, box_plot2, box_plot3, box_plot4)
    box_plot_all <- rbind(box_plot_all, box_plot5)
}    

# Create a simple map of Norway etc with ebird boxes, ebird observations and cluster centroids
norway_map <- map_data("world", region = c("Norway"))
# russia_map <- map_data("world", region = c("Russia"))
# russia_map <- subset(russia_map, long >= 2.5 & long <= 32.5 & lat >= 57.5 & lat <= 72.5)
# write_csv(russia_map, 'data/norway/russia_map.csv')
russia_map <- read_csv('data/norway/russia_map.csv')
rest_map <- map_data("world", region = c("Finland", "Sweden"))
russia_map <- subset(russia_map, long >= 2.5 & long <= 32.5 & lat >= 57.5 & lat <= 72.5)
norway_map <- subset(norway_map, long >= 2.5 & long <= 32.5 & lat >= 57.5 & lat <= 72.5)
rest_map <- subset(rest_map, long >= 2.5 & long <= 32.5 & lat >= 57.5 & lat <= 72.5)
n.clust.full <- read_csv('data/norway/n.clust.full.csv')
n.site.name <- read_csv('data/norway/site.loc.name.csv')

# Create the plot
ggplot() +
  theme_bw() +
  # Plot the map of Norway
  # theme(panel.background = element_rect(fill = "lightskyblue", size = 0.5, linetype = "solid")) +
  geom_polygon(data = russia_map, aes(x = long, y = lat, group = group), fill = "white", color = "black", size=0.2) +
  geom_polygon(data = rest_map, aes(x = long, y = lat, group = group), fill = "white", color = "black", size=0.2) +
  geom_polygon(data = norway_map, aes(x = long, y = lat, group = group), color = "darkgreen", size=0.6, fill = 'lightgreen', alpha = 0.1) +
  
  # Plot the boxes
  geom_polygon(data = box_plot_all, aes(x = lon, y = lat, group = cluster), color = "black", fill = 'white', size=0.2) +
  
  # Plot the ebird points
  geom_point(data = n.ebird.cluster, aes(x = longitude, y = latitude), fill = 'blue', colour = 'blue', size = 0.1, ) +
  # Plot the cluster centroid points
  # geom_point(data = n.clust.full, aes(x = long, y = lat), fill = NA, size = 3.0, color = "darkred", shape = 'triangle') +
  geom_point(data = n.site.name, aes(x = long, y = lat), fill = NA, size = 3.0, color = "darkred", shape = 'triangle') +
  theme(axis.title = element_blank()) +
  theme(text=element_text(size=15)) +
  scale_colour_brewer(palette="Greens") +
  scale_y_continuous(limits = c(57.5, 72.5), expand=c(0,0)) +
  scale_x_continuous(limits = c(2.5, 32.5), expand=c(0,0))
  # Add a title
  # ggtitle("eBird observations extracted from a 80x80km box around the weighted centroid of each cluster")
