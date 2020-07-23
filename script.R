# Ola Tranum Arnegaard
# UC Berkeley, BGA Exchange Fall 2019
# Daily trip distribution analysis of Oslo bicycle-sharing system using PCA and K-means clustering

library(tidyr)
library(plyr)
library(dplyr)
library(lubridate)
library(timeDate)
library(qcc)

# Pre-processing

# Fetching the trip data (This example is for pickups. To calculate the results for drop-offs, simply change the start keyword to end.)
trips_path <- '<path to trip data>'
trips_raw <- read.csv(trips_path, header=TRUE)

# Selecting the desired columns as well as changing the date and time format.
trips_processed_1 <- trips_raw %>% select(start_station_name, started_at) %>% 
  mutate(day = day(started_at), month = month(started_at), isWeekday = isWeekday(started_at), hour = hour(started_at)) %>% 
  select(-started_at)

# Selecting only weekdays, as weekends are not iincluded in the scope of this project.
trips_processed_2 <- trips_processed_1 %>% filter(isWeekday == TRUE) %>% select(-isWeekday)

# Grouping trips for each hour of the day, all days, per station.
trips_processed_3 <- trips_processed_2 %>% group_by(start_station_name, day, month, hour) %>% dplyr::summarize(trips = n())

# Creating a column for each hour, for each day and station.
trips_processed_4 <- trips_processed_3 %>% spread(hour, trips)

# NA’s are swapped with zeros.
trips_processed_4[is.na(trips_processed_4)] <- 0

# Making sure only trips during operating hours
trips_processed_4$`1` <- 0
trips_processed_4$`2` <- 0
trips_processed_4$`3` <- 0
trips_processed_4$`4` <- 0

# Removing unnecessary columns, grouping per station (one row per station) and calculating the mean of each column.
trips_processed_5 <- trips_processed_4[,c(-2, -3)] %>% 
  group_by(start_station_name) %>% 
  dplyr::summarise_all(funs(mean))

# Selecting only the distribution data
mat <- as.matrix(trips_processed_5[,2:25])

# Normalization
trips_normalized <- mat/rowSums(mat)

# Format wrangling
trips_normalized <- as.data.frame(trips_normalized)



# Visualizing a given day
plot_station <- function(y, i){
  x = 0:23
  barplot(height = t(t(y[i,])),
          names.arg = x,
          col="#414a4c", 
          space = 0.25, 
          ylim=c(0,0.4))
}

day <- 10
plot_station(trips_normalized, day)



# PCA
principal_components <- 7
trips_normalized.pca <- prcomp(trips_normalized)
df <- trips_normalized.pca$x[,1:principal_components]

# Pareto Plot
summary <- summary(trips_normalized.pca)
qcc.options(bg.margin = "white")
pareto.chart(summary$importance[2,1:7])



# Elbow method
k.max <- 15
data <- df
wss <- sapply(1:k.max,
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# K-means
k <- 3
kmeans <- kmeans(df, k)



# Reconstruction of Centers
centers = kmeans$centers
means = colMeans(trips_normalized)

Xhat = centers[, 1:principal_components] %*% t(trips_normalized.pca$rotation[, 1:principal_components])

Xhat = scale(Xhat, center = -means, scale = FALSE)

# Showing reconstructed ceters
barplot(Xhat)

# Adding the chosen cluster for each station
station <- trips_processed_5[,1]
station_cluster = data.frame(stations, df$cluster)

# Fetching stations metadata
metadata_path <- '<path to metadata>'
station_lon_lat <- read.csv(metadata_path, header = TRUE) %>% 
  select(start_station_name, start_station_latitude, start_station_longitude)

# Joining the metadata and clusters to a new dataframe.
data_complete <- inner_join(station_cluster, station_lon_lat)

save_path <- "<path to save-file>"
write.csv(data_complete, file = save_path)



# Finding crititcal stations

pickups_path <- "<path to pickup distributions>"
pickups <- read.csv(pickups_path, header = TRUE)

dropoffs_path <- "<path to drop-off distributions>"
dropoffs <- read.csv(dropoffs_path, header = TRUE)

dropoffs <- dropoffs %>% dplyr::rename(DropOffsCluster = df.cluster)
pickups <- pickups %>% dplyr::rename(PickUpsCluster = df.cluster)
joined <- inner_join(pickups, dropoffs)

# Select clusters with unwanted combinations of pickup drop-off distributions
joined <- joined %>% select(-1)
joined <- joined %>% filter((PickUpsCluster == 3 & DropOffsCluster == 3) | (PickUpsCluster == 1 & DropOffsCluster == 2))

save_path <- "<path to save file>"
write.csv(joined, file = save_path)






