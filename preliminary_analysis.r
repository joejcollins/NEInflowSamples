## Preliminary analysis of water inflow data

# Import the water inflow sample data
samples <- read.csv("./water_inflow_data.csv",  stringsAsFactors=FALSE)

# Rename all the measurements for easier reading
rn <- function(old.name, new.name){
  names(samples)[names(samples) == old.name] <<- new.name
}

rn("BOD.5.Day.ATU..mg.l.", "BOD")
rn("Nitrogen...Total.as.N..mg.l.", "Nitrogen")
rn("Phosphorus...Total.as.P..mg.l.", "Phosphorus")
rn("Alkalinity.to.pH.4.5.as.CaCO3..mg.l.", "Alkalinity")
rn("Ammoniacal.Nitrogen.as.N..mg.l.", "Nitrogen.Ammoniacal")
rn("Chloride..mg.l", "Chloride")
rn("Nitrite.as.N..mg.l.", "Nitrite")
rn("Nitrogen...Total.Oxidised.as.N..mg.l.", "Nitrogen.Oxidised")
rn("Orthophosphate..reactive.as.P..mg.l.", "Orthophosphate")
rn("Silicate..reactive.as.SiO2..mg.l.", "Silicate")
rn("Phosphate...Total.as.P..mg.l.", "Phosphate")
rn("Conductivity.at.20C..uS.cm.", "Conductivity")
rn("Turbidity..NTU.", "Turbidity")
rn("Solids..Suspended.at.105.C..mg.l.", "Solids")
rn("Calcium..mg.l.", "Calcium")
rn("Magnesium..mg.l.", "Magnesium")
rn("Potassium..mg.l.", "Potassium")
rn("Sodium..mg.l.", "Sodium")

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

# Zoom in in the Marton cluster of 10
map_centre <- c(lon = -3.045, lat = 52.623) # chosen by inspection
map <- qmap(map_centre, zoom = 15) 
map + geom_point(aes(x=Lng, y=Lat), data=sites, col = km$cluster)


# Remove non numeric from 
scratch <- samples[,c("Phosphorus...Total.as.P..mg.l.")]
scratch <- gsub("[^\\d]", "", scratch, fixed = TRUE)
scratch <- as.numeric(scratch)
hist(scratch)

# Remove non numeric from 
shit <- samples[,c("BOD.5.Day.ATU..mg.l.")]
shit <- gsub("[^\\d]", "", shit, fixed = TRUE)
shit <- as.numeric(shit)
hist(shit, breaks = 50)

colnames(samples)
