## ----import sites--------------------------------------------------------
samples <- read.csv("./inflow_sample_data.csv",  stringsAsFactors=FALSE)

samples$Grid.reference <- gsub(" ", "", samples$Grid.reference, fixed = TRUE)

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
