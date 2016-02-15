library(circlize)
library(RColorBrewer)

# make matrix of trips from one station to the other
station_to_station<- with(hrNoNa, table(FromStationId, ToStationId))[,-37] # works, but is too messy

# cluster 50 stations
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
plot(hc)
nclust<- 6
rect.hclust(hc, k = nclust)
hrStations$clust <- cutree(hc,k= nclust) # assign 10 clusters

# now add clusters to main data
index.from <- match(hrNoNa$FromStationId, hrStations$StationNum)
index.to <- match(hrNoNa$ToStationId, hrStations$StationNum)

hrNoNa$from.cluster <- hrStations$clust[index.from]
hrNoNa$to.cluster <- hrStations$clust[index.to]

# make matrix from cluster to cluster
cluster_to_cluster<- with(hrNoNa, table(from.cluster, to.cluster))
cluster_to_cluster<- as.data.frame.matrix(cluster_to_cluster)
cluster_to_cluster<- as.matrix(cluster_to_cluster)


# draw chord diagram
# chordDiagram(cluster_to_cluster, transparency = 0.4, grid.col = "midnightblue",
#              col = colorRamp2(seq(0, 1, 0.2), brewer.pal(6, "Blues")))
color<- brewer.pal(nclust, "Set1")
chordDiagram(cluster_to_cluster, color)


# give each cluster individual colors
# make another diagram clustered by altitude
# make a map indicating the clusters

