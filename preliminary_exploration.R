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
samples$Grid.reference <- gsub("SJ5651039415", "SJ5051039415", samples$Grid.reference, fixed = TRUE)

## ----stuff1--------------------------------------------------------------
print("shit")


