## Preliminary analysis of water inflow data

# Import the water inflow sample data
samples <- read.csv("./water_inflow_data.csv",  stringsAsFactors=FALSE)

# Remove the spaces from the grid references
samples$Grid.reference <- gsub(" ", "", samples$Grid.reference, fixed = TRUE)

# How many site names are there?
length(unique(samples$Comment))

# How many grid references are there?
length(unique(samples$Grid.reference))

# How many pairs of comments and grid references?
sites <- data.frame(OSGrid = samples$Grid.reference, Name = samples$Comment)
sites <- unique(sites)
nrow(sites)

# Give each of the sites a unique id (might come in handy)
sites$Id <- seq.int(nrow(sites))

# Export the sites list so WGS84 can be added to it
write.table(sites, "sites.csv", row.names = FALSE, col.names = TRUE, sep = ",")

## Go to http://gridreferencefinder.com/batchConvert/batchConvert.php to do the geocoding

# Import the geocoded sites
sites <- read.csv("./sites_with_locations.csv", header = TRUE, stringsAsFactors = FALSE)

# Cluster using k-means
km <- kmeans(cbind(sites$X, sites$Y), centers = 3)
# Plot without a background to look at the clusters
plot(sites$X, sites$Y, col = km$cluster, pch = 20)
plot(sites$Lng, sites$Lat, col = km$cluster, pch = 20)

# Get a map of the area and plot sites
library(ggmap)
map_centre <- c(lon = -2.85, lat = 52.80) # chosen by inspection
map <- qmap(map_centre) 
map + geom_point(aes(x=Lng, y=Lat), data=sites, col = km$cluster)


