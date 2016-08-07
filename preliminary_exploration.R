## ----read in data--------------------------------------------------------
# Import the water inflow sample data
samples <- read.csv("./water_inflow_data.csv",  stringsAsFactors = FALSE)

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

## ----data corrections----------------------------------------------------
# Make the date text consistent so they can be converted more easily to date objects.
samples$Sample.taken <- gsub("Nov", "NOV", samples$Sample.taken, fixed = TRUE)
samples$Sample.taken <- gsub(".", ":", samples$Sample.taken, fixed = TRUE)
samples$Sample.taken <- gsub("01/06/2015", "01-JUN-15", samples$Sample.taken, fixed = TRUE)

# The dates are also ambiguous so stick in the century using a regex and remove the time
# of dat because it is not needed.
samples$Sample.taken <- gsub("(\\d{2}-[A-Z]{3}-)", "\\120", samples$Sample.taken, perl = TRUE)
samples$Sample.taken <- strptime(samples$Sample.taken, "%d-%b-%Y %H:%M")
samples$Sample.taken <- as.Date(samples$Sample.taken)
samples$Sample.taken <- as.POSIXct(samples$Sample.taken, "%d-%b-%Y")

# Remove the spaces from the grid references then correct the defect.
samples$Grid.reference <- gsub(" ", "", samples$Grid.reference, fixed = TRUE)
samples$Grid.reference <- gsub("SJ5051039415", "SJ5651039415", samples$Grid.reference, fixed = TRUE)

## ----export site list----------------------------------------------------
# How many pairs of comments and grid references?
sites <- data.frame(OSGrid = samples$Grid.reference, Name = samples$Name)
sites <- unique(sites)

# Give each of the sites a unique id (might come in handy later)
sites$Id <- seq.int(nrow(sites))

# Export the sites list so WGS84 can be added to it
write.table(sites, "sites.csv", row.names = FALSE, col.names = TRUE, sep = ",")

# Import the geocoded sites
sites <- read.csv("./sites_with_locations.csv", header = TRUE, stringsAsFactors = FALSE)

## ----overview of sites, fig.cap = 'The sites are in three clusters in North Shropshire.'----
# Cluster using Partitioning Around Medoids because k-means includes
# a randomised element so it can mess up periodically
library(cluster)
pam <- pam(cbind(sites$X, sites$Y), 3)
# Get a map of the area and plot sites
library(ggmap)
map_centre <- c(lon = -2.85, lat = 52.80) # chosen by inspection
map <- qmap(map_centre) 
map + geom_point(aes(x=Lng, y=Lat), data=sites, col = pam$clustering, cex = 2)

## ----marton pool cluster, fig.cap="10 sites around Marton Pool."---------
map_centre <- c(lon = -3.045, lat = 52.623) # chosen by inspection
map <- qmap(map_centre, zoom = 15) 
map + geom_point(aes(x=Lng, y=Lat), data=sites, col = pam$clustering, cex = 2)

## ----brown moss cluster, fig.cap="8 sites around Brown Moss."------------
map_centre <- c(lon = -2.653, lat = 52.950) # chosen by inspection
map <- qmap(map_centre, zoom = 15) 
map + geom_point(aes(x=Lng, y=Lat), data=sites, col = pam$clustering, cex = 2)

## ----ellesmere cluster, fig.cap="12 sites near Ellesmere (Whitemere, Colemere and Crosemere)."----
map_centre <- c(lon = -2.850, lat = 52.883) # chosen by inspection
map <- qmap(map_centre, zoom = 13) 
map + geom_point(aes(x=Lng, y=Lat), data=sites, col = pam$clustering, cex = 2)

## ----distributions of measurements---------------------------------------
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

## ----remove less than----------------------------------------------------
removelessthan <- function(column){
  column <- gsub("[^.0-9]+", "", column) # just the numbers and nothing else
  column <- as.numeric(column)
  return(column)
}
samples$BOD <- removelessthan(samples$BOD)
samples$Phosphorus <- removelessthan(samples$Phosphorus)           
samples$Nitrogen.Ammoniacal <- removelessthan(samples$Nitrogen.Ammoniacal)
samples$Nitrite <- removelessthan(samples$Nitrite) 
samples$Nitrogen.Oxidised <- removelessthan(samples$Nitrogen.Oxidised)
samples$Orthophosphate <- removelessthan(samples$Orthophosphate)       
samples$Silicate <- removelessthan(samples$Silicate)
samples$Phosphate <- removelessthan(samples$Phosphate)           
samples$Turbidity <- removelessthan(samples$Turbidity)
samples$Solids <- removelessthan(samples$Solids)

## ----worst offenders-----------------------------------------------------
# Let's find the bad boys
baduns <- function(column.name){
  # The column.name is passed as a string so the column has to be accessed using [] notation
  # rather than directly.
  scratch <- samples[,c("Name", column.name)]
  scratch <- aggregate(scratch[,column.name], by = list(scratch$Name), max)
  scratch <- scratch[order(-scratch$x),] # Bad boys at the top
  names(scratch)[names(scratch) == "Group.1"] <- "Name"
  scratch <- head(scratch, n=8) # Calcium etc... have only been recorded for 8 sites.
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

## ----all sites through the year------------------------------------------
# Sites with 9 or more samples.
scratch <- as.data.frame(table(samples$Name))
colnames(scratch) <- c("Name", "Freq")
sites <- merge(sites, scratch, by="Name") # join the tables
scratch <- sites[sites$Freq >= 8,]
scratch <- merge(scratch, samples, by = "Name")

# The flows include some comments to the effect that there is no flow so these are made into zeros
scratch$Flow <- as.numeric(scratch$Flow)
scratch$Flow[is.na(scratch$Flow)] <- 0

# Let's graph the ones with reasonable amounts of data
library(ggplot2)
library(scales)
timegraph <- function(column.name, df){
  df[,c(column.name)] <- gsub("[^.0-9]+", "", df[,c(column.name)]) # just the numbers and nothing else
  df[,c(column.name)] <- as.numeric(df[,c(column.name)])
  df$total <- df[,c(column.name)] * df$Flow
  # assign the graph to a variable so it can be printed explicitly
  plot <- ggplot(df, aes(x = Sample.taken, y = df$total, colour=Name, group=Name)) +
    geom_line() +
    scale_x_datetime(date_breaks = "1 month", date_labels = "%b") +
    xlab("2015-2016") + 
    ylab(paste0(column.name, " * Flow"))
  print(plot)
}

timegraph("BOD", scratch)

## ----cole mere through the year------------------------------------------
scratchmere <- scratch[startsWith(scratch$Name, "Cole"), ]
alltimegraphs <- function(df){
  timegraph("BOD", df)
  timegraph("Nitrogen.Total", df)             
  timegraph("Phosphorus", df)          
  timegraph("Alkalinity", df)         
  timegraph("Nitrogen.Ammoniacal", df)
  timegraph("Chloride", df)
  timegraph("Nitrite", df)
  timegraph("Nitrogen.Oxidised", df)
  timegraph("Orthophosphate", df)       
  timegraph("Silicate", df)
  timegraph("Phosphate", df)             
  timegraph("Conductivity", df)
  timegraph("Turbidity", df)
  timegraph("Solids", df)
}
alltimegraphs(scratchmere)

## ----crose mere through the year-----------------------------------------
scratchmere <- scratch[startsWith(scratch$Name, "Crose"), ]
alltimegraphs(scratchmere)

## ----marton pool through the year----------------------------------------
scratchmere <- scratch[startsWith(scratch$Name, "Marton"), ]
alltimegraphs(scratchmere)

## ----sweat mere through the year-----------------------------------------
scratchmere <- scratch[startsWith(scratch$Name, "Sweat"), ]
alltimegraphs(scratchmere)

## ----white mere through the year-----------------------------------------
scratchmere <- scratch[startsWith(scratch$Name, "White"), ]
alltimegraphs(scratchmere)

