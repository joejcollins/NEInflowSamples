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

# Export the sites list so WGS84 can be added to it at http://gridreferencefinder.com/batchConvert/batchConvert.php
write.table(sites, "sites.csv", row.names = FALSE, col.names = FALSE, sep = ",")






unique(pairs)


require(rnrfa)

Longitudes <- OSGparse(samples$Grid.reference, CoordSystem = "WGS84")["lon"]
samples <- cbind(samples, Longitudes)

Latitudes <- OSGparse(samples$Grid.reference, CoordSystem = "WGS84")["lat"]
samples <- cbind(samples, Latitudes)

median(samples$lon, na.rm = FALSE)

median(samples$lat, na.rm = FALSE)

## http://yarkerconsulting.com/index.php/blog/15-google-maps-and-r
MyMap <- MapBackground(lat=53.04437, lon=-2.860608, zoom=2)

PlotOnStaticMap(MyMap,samples$lat,samples$lon,cex=tmp+0.8,pch=16,col='black')
