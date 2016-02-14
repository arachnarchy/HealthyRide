# first commit
library(circlize)
library(RColorBrewer)
library(ggplot2)
library(ggmap)

rm(list=ls())

# Read data files ---------------------------------------------------------

setwd("~/Documents/R Statistics/DataIncubator/PGH Healthyride")

hr<- read.table(
  "HealthyRideRentals 2015 Q3.csv", header = TRUE, sep = ",", quote = "", comment.char = "", na.strings = "NA", stringsAsFactors = FALSE)
hrStations<- read.table(
  "HealthyRideStations2015.csv", header = TRUE, sep = ",", quote = "", comment.char = "", na.strings = "NA", stringsAsFactors = FALSE)

# Data wrangling ----------------------------------------------------------

index.from <- match(hr$FromStationId, hrStations$StationNum)
index.to <- match(hr$ToStationId, hrStations$StationNum)

hr$from.latitude <- hrStations$Latitude[index.from]
hr$from.longitude <- hrStations$Longitude[index.from]
hr$to.latitude <-  hrStations$Latitude[index.to] 
hr$to.longitude <- hrStations$Longitude[index.to]

mean(hr$TripDuration)/60

# Calculate distance in kilometers between two points
earth.dist <- function (long1, lat1, long2, lat2)
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}

hr$eagle.dist<- earth.dist(hr$from.longitude, hr$from.latitude, hr$to.longitude, hr$to.latitude) * 0.621371 # convert to miles
mean(hr$eagle.dist[hr$eagle.dist != 0], na.rm = TRUE) # mean of trip distance between different stations

# remove NAs
hrNoNa<- na.omit(hr)

# label trip type
index.aa<- hrNoNa$FromStationId == hrNoNa$ToStationId
index.ab<- hrNoNa$FromStationId != hrNoNa$ToStationId
hrNoNa$TripType[index.aa]<- "A-A trip"
hrNoNa$TripType[index.ab]<- "A-B trip"

# insert count per station
hrStations$from.count<- ddply(hr, .(FromStationId), summarize, freq = length(FromStationId))[1:50,2]
hrStations$to.count<- ddply(hr, .(ToStationId), summarize, freq = length(FromStationId))[1:50,2]

# identify source and sink stations
hrStations$net<- hrStations$from.count - hrStations$to.count

# cluster 50 stations into local groups
geo.dist = function(df) {
  require(geosphere)
  d <- function(i,z){         # z[1:2] contain long, lat
    dist <- rep(0,nrow(z))
    dist[i:nrow(z)] <- distHaversine(z[i:nrow(z),c(5,4)],z[i,c(5,4)])
    return(dist)
  }
  dm <- do.call(cbind,lapply(1:nrow(df),d,df))
  return(as.dist(dm))
}

d<- geo.dist(hrStations)
hc <- hclust(d)      # hierarchical clustering
nclust<- 6
rect.hclust(hc, k = nclust)
hrStations$clust <- cutree(hc,k= nclust) # assign 6 clusters

# now add clusters to main data
index.from <- match(hrNoNa$FromStationId, hrStations$StationNum)
index.to <- match(hrNoNa$ToStationId, hrStations$StationNum)

hrNoNa$from.cluster <- hrStations$clust[index.from]
hrNoNa$to.cluster <- hrStations$clust[index.to]

# make matrix of trips from cluster to cluster
cluster_to_cluster<- with(hrNoNa, table(from.cluster, to.cluster))
cluster_to_cluster<- as.data.frame.matrix(cluster_to_cluster)
cluster_to_cluster<- as.matrix(cluster_to_cluster)


# run some stats ----------------------------------------------------------

print(sprintf("Total number of trips recorded in the first three months of Healthy Ride: %s", nrow(hr)))
print(sprintf("Average trip duration was %s min", mean(hr$TripDuration)/60))

print(ddply(
  hrNoNa, .(UserType), summarize, 
  duration = mean(TripDuration) /60,
  sd.duration = sd(TripDuration) /60,
  distance = mean(eagle.dist[TripType == "A-B trip"]),
  sd.distance = sd(eagle.dist[TripType == "A-B trip"]),
  number = length(UserType)
)) #summary stats

print(ddply(hrNoNa, .(TripType), summarize,
            number = length(TripType)))

# which stations lose, which stations get bikes? Cluster 5 gets the most, it's in the center. Clusters 1 and 6 lose the most, they're the most peripheral
print(ddply(hrStations, .(clust), summarize, net = mean(net)))




# plot figures ------------------------------------------------------------

color<- brewer.pal(nclust, "Set1")

# draw chord diagram
plot1a<- chordDiagram(cluster_to_cluster, color)

# get Pittsburgh map
mapPGH <- get_map(location = c(lon = mean(hrStations$Longitude), lat = mean(hrStations$Latitude)), zoom = 13,
                  maptype = "roadmap", scale = 2)

# plot the map with some points on it

plot1b<- ggmap(mapPGH, extent = "device") +
  geom_point(
    data = hrStations[hrStations$clust == 1,], fill = color[1], aes(x = Longitude, y = Latitude), size = 5, shape = 21) + 
  geom_point(
    data = hrStations[hrStations$clust == 2,], fill = color[2], aes(x = Longitude, y = Latitude), size = 5, shape = 21) +
  geom_point(
    data = hrStations[hrStations$clust == 3,], fill = color[3], aes(x = Longitude, y = Latitude), size = 5, shape = 21) +
  geom_point(
    data = hrStations[hrStations$clust == 4,], fill = color[4], aes(x = Longitude, y = Latitude), size = 5, shape = 21) +
  geom_point(
    data = hrStations[hrStations$clust == 5,], fill = color[5], aes(x = Longitude, y = Latitude), size = 5, shape = 21) +
  geom_point(
    data = hrStations[hrStations$clust == 6,], fill = color[6], aes(x = Longitude, y = Latitude), size = 5, shape = 21)

# plot number of pick-ups as interpolated heat map
plot2<- ggmap(mapPGH, extent = "device") +
  geom_density2d(data = hrNoNa, aes(x = from.longitude, y = from.latitude), size = 0.5) + 
  stat_density2d(data = hrNoNa, aes(x = from.longitude, y = from.latitude, fill = ..level..), alpha = 0.5, size = 0.1, geom = 'polygon') + 
  scale_fill_gradient(low = "white", high = "red")

