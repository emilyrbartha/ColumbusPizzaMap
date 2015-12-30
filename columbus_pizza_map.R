# File: columbus_pizza_map.R
# Desc: Creates pizza density map of Columbus, Ohio using Yelp API data
# Date: 12/19/15

library(ggmap)
library(ggplot2)
library(compiler)
library(RColorBrewer)
library(stringr)

# Working directory containing Yelp data
setwd("/Users/emilyrbartha/Dropbox/PizzaMap")

# Download map and record bounding box attributes
columbus <- get_map(location=c(-82.99879,39.99),zoom=12,maptype="watercolor",color="bw")
bb <- attr(columbus, "bb")
box <- bb2bbox(bb)

# Import and clean neighborhood data
neighborhood_data <- read.csv("neighborhood_data.csv")
neighborhood_data$neighborhoods <- as.factor(str_trim(gsub("'","",neighborhood_data$neighborhoods,fixed=TRUE)))
neighborhood_data <- neighborhood_data[neighborhood_data$neighborhoods != "",]
neighborhood_data$neighborhoods <- factor(neighborhood_data$neighborhoods)
neighborhood_labels <- data.frame(labs=levels(neighborhood_data$neighborhoods))

# Jitter neighborhood labels for better plotting
neighborhood_labels$lat <- tapply(neighborhood_data$latitude,neighborhood_data$neighborhoods,mean)
neighborhood_labels$lat <- jitter(neighborhood_labels$lat,200)
neighborhood_labels$long <- tapply(neighborhood_data$longitude,neighborhood_data$neighborhoods,mean)
neighborhood_labels$long <- jitter(neighborhood_labels$long,200)

# Whitehall and Gahanna run off the map, remove them
neighborhood_labels <- neighborhood_labels[!neighborhood_labels$labs %in% c("Whitehall","Gahanna"),]

# Import pizza data and restrict to map bounding box
pizza_data <- read.csv("pizza.csv")
pizza_data_inbounds <- subset(pizza_data,pizza_data$latitude <= box[4] & pizza_data$latitude >= box[2] & pizza_data$longitude <= box[3] & pizza_data$longitude >= box[1])

n_reviews <- pizza_data_inbounds$num_reviews
rating <- pizza_data_inbounds$rating

# Bayesian averaging parameters (can experiment with these)
m <- 3
c <- 5

# Standardized rating takes into account number of reviews via Bayesian average
# See http://fulmicoton.com/posts/bayesian_rating/
bayesian_rating <- ((c*m)+rating*n_reviews)/(c + n_reviews)
bayesian_rating <- ecdf(bayesian_rating)(bayesian_rating)
pizza_where <- data.frame(lat = pizza_data_inbounds$latitude,long = pizza_data_inbounds$longitude)

# Create map grid 
long_range <- c(box[1],box[3])
lat_range <- c(box[2],box[4])
resolution <- 100
points_lat <- seq(lat_range[1],lat_range[2],length.out=resolution)
points_long <- seq(long_range[1],long_range[2],length.out=resolution)

# Populate map grid using distance from restaurants
plot_score <- rep(0,resolution)
plot_long <- rep(0,resolution*resolution)
plot_lat <- rep(0,resolution*resolution)
num_pizza <- rep(0,resolution*resolution)
l <- 1

# Distance threshold controls whether restaurant affects area score
dist_thresh <- 0.015

# Calculate pizza density scores for map grid
for(i in 1:resolution) {
  for(j in 1:resolution) {
    raw_dist <- sqrt((points_long[i]-pizza_where$long)^2 + (points_lat[j]-pizza_where$lat)^2)
    ind <- (raw_dist < dist_thresh) 
    pizza_score <- ind*(bayesian_rating)^2*(1-(raw_dist/dist_thresh))
    plot_score[l] <- sum(pizza_score)
    plot_long[l] <- points_long[i]
    plot_lat[l] <- points_lat[j]
    l <- l + 1
  }
}

# Control plot colors
plot_data <- data.frame(plot_long,plot_lat,plot_score)
plot_data <- plot_data[plot_data$plot_score > 0,]
col_pal <- c("yellow","orange","#EC5800","#F93822","#8B0000")

# Plot map and save results
ggmap(columbus,extent="device") + geom_tile(data=plot_data,aes(x=plot_long,y=plot_lat,fill=factor(cut(plot_score,breaks=5,labels=FALSE))),alpha=0.8) + scale_fill_manual(values=col_pal,name="Pizza Density") + geom_point(data=pizza_where,aes(x=long,y=lat),color="black",alpha=0.5,size=5*(bayesian_rating)^2) + geom_point(data=neighborhood_labels,aes(x=long,y=lat),size=3,colour="green",alpha=0.5) + geom_text(data=neighborhood_labels,aes(x=long,y=lat,label=labs),size=3,angle=30,fontface="bold") + theme(legend.position=c(0.90,0.55))
ggsave("columbus_pizza_map.png")

