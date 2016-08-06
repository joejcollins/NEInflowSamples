## Preliminary analysis of water inflow data

# Import the water inflow sample data
samples <- read.csv("./water_inflow_data.csv",  stringsAsFactors = FALSE)
# I did edit a couple of the dates because R seems to struggle with dates (but then don't we all)

# Rename all the measurements for easier reading
rn <- function(old.name, new.name){
  names(samples)[names(samples) == old.name] <<- new.name
}

rn("Flow.rate..litres.sec.", "Flow")

rn("BOD.5.Day.ATU..mg.l.", "BOD")
rn("Nitrogen...Total.as.N..mg.l.", "Nitrogen.Total")
rn("Phosphorus...Total.as.P..mg.l.", "Phosphorus")
rn("Alkalinity.to.pH.4.5.as.CaCO3..mg.l.", "Alkalinity")
rn("Ammoniacal.Nitrogen.as.N..mg.l.", "Nitrogen.Ammoniacal")
rn("Chloride..mg.l.", "Chloride")
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

# Also remane the comment because it actually contains the site name
rn("Comment", "Name")

# Remove the spaces from the grid references
samples$Grid.reference <- gsub(" ", "", samples$Grid.reference, fixed = TRUE)

# How many site names are there?
length(unique(samples$Name))

# How many grid references are there?
length(unique(samples$Grid.reference))

# How many pairs of comments and grid references?
sites <- data.frame(OSGrid = samples$Grid.reference, Name = samples$Name)
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


# Let's check out the distribution of measurments
# A function to standardised the graphs
graf <- function(column.name){
  scratch <- samples[,c(column.name)]
  scratch <- gsub("[^.0-9]+", "", scratch) # just the numbers and nothing else
  scratch <- as.numeric(scratch)
  title <- paste("Histogram of", column.name, sep = " ")
  hist(main = title, scratch, xlab = column.name, breaks = 20)
}
# Complete graph-arama
graf("BOD")
graf("Nitrogen.Total")             
graf("Phosphorus")           
graf("Alkalinity")          
graf("Nitrogen.Ammoniacal")
graf("Chloride")
graf("Nitrite") 
graf("Nitrogen.Oxidised") 
graf("Orthophosphate")       
graf("Silicate")
graf("Phosphate")             
graf("Conductivity")
graf("Turbidity")
graf("Solids")
graf("Calcium")              
graf("Magnesium")
graf("Potassium")
graf("Sodium")

"
The graphs for calcium, conductivity, silicate, alkalinity look like they might have been censored.
There appears to be a spike at one end of the distribution, like the data might have contained
a bunch of < or > but inspection reveals that silicate contains only one reading with < (<0.200).
"

# Let's find the bad boys
baduns <- function(column.name){
  # The column.name is passed as a string so the column has to be accessed using [] notation
  # rather than directly.
  scratch <- samples[,c("Name", column.name)]
  scratch[,c(column.name)] <- gsub("[^.0-9]+", "", scratch[,c(column.name)]) # just the numbers and nothing else
  scratch[,c(column.name)] <- as.numeric(scratch[,c(column.name)]) # then convert to numeric
  scratch <- aggregate(scratch[,column.name], by = list(scratch$Name), max)
  scratch <- scratch[order(-scratch$x),] # Bad boys at the top
  names(scratch)[names(scratch) == "Group.1"] <- "Name"
  scratch <- head(scratch, n=8) # Calcium etc... have only been recorded for 8 sites.
  print(scratch)
  scratch <- scratch[order(scratch$x),] # reverse the order
  bp <- barplot(scratch$x, xlab=column.name, horiz=TRUE)
  text(0, bp, scratch$Name, cex=1, pos=4)
}

baduns("BOD")
baduns("Nitrogen.Total")             
baduns("Phosphorus")           
baduns("Alkalinity")          
baduns("Nitrogen.Ammoniacal")
baduns("Chloride")
baduns("Nitrite") 
baduns("Nitrogen.Oxidised") 
baduns("Orthophosphate")       
baduns("Silicate")
baduns("Phosphate")             
baduns("Conductivity")
baduns("Turbidity")
baduns("Solids")
baduns("Calcium")              
baduns("Magnesium")
baduns("Potassium")
baduns("Sodium")

# Sites with 9 or more samples.
scratch <- as.data.frame(table(samples$Name))
colnames(scratch) <- c("Name", "Freq")
sites <- merge(sites, scratch, by="Name")
scratch <- sites[sites$Freq >= 8,]
scratch <- merge(scratch, samples, by = "Name")

# The dates are ambiguous so stick in the century using a regex
scratch$Sample.taken <- gsub("(\\d{2}-[A-Z]{3}-)", "\\120", scratch$Sample.taken, perl = TRUE)
scratch$Sample.taken <- strptime(scratch$Sample.taken, "%d-%b-%Y %H:%M")
scratch$Sample.taken <- as.Date(scratch$Sample.taken)
scratch$Sample.taken <- as.POSIXct(scratch$Sample.taken, "%d-%b-%Y")

# The flows include some comments to the effect that there is no flow so these are made into zeros
scratch$Flow <- as.numeric(scratch$Flow)
scratch$Flow[is.na(scratch$Flow)] <- 0

library(ggplot2)
library(scales)

# Let's graph the ones with reasonable amounts of data
timegraph <- function(column.name){
  scratch[,c(column.name)] <- gsub("[^.0-9]+", "", scratch[,c(column.name)]) # just the numbers and nothing else
  scratch[,c(column.name)] <- as.numeric(scratch[,c(column.name)])
  scratch$total <- scratch[,c(column.name)] * scratch$Flow
  ggplot(scratch, aes(x = Sample.taken, y = scratch$total, colour=Name, group=Name)) +
    geom_line() +
    scale_x_datetime(date_breaks = "1 month", date_labels = "%b") +
    xlab("2015-2016") + 
    ylab(paste0(column.name, " * Flow"))
}

timegraph("BOD")
timegraph("Nitrogen.Total")             
timegraph("Phosphorus")           
timegraph("Alkalinity")          
timegraph("Nitrogen.Ammoniacal")
timegraph("Chloride")
timegraph("Nitrite") 
timegraph("Nitrogen.Oxidised") 
timegraph("Orthophosphate")       
timegraph("Silicate")
timegraph("Phosphate")             
timegraph("Conductivity")
timegraph("Turbidity")
timegraph("Solids")
timegraph("Calcium")              
timegraph("Magnesium")
timegraph("Potassium")
timegraph("Sodium")


