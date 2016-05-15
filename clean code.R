rm(list = ls())

################################## LOAD REQUIRED LIBRARIES #########################################

#Mapping libraries:
library(rgeos)
library(maptools)
library(spdep)
library("rgdal")

####################################### IMPORT DATA ################################################

#Get the immunisation data:
setwd("~/Documents/uni/STAT3013/Project/stat-inference")
immunisation_data <- read.csv("1 year fully immunised.csv")

#Get the postcode area data:
setwd("~/Documents/uni/STAT3013/Project/stat-inference/Postcode data/1270055003_poa_2011_aust_shape")
Postcodes_areas <- readOGR(".", "POA_2011_AUST")
#ignore the warning message: "dropping null geometries: 2514, 2515, 2516"

setwd("/Users/jamesbailie/Documents/uni/STAT3013/Project/stat-inference")

################################### FILTER POSTCODES BY NPs ########################################
###Remove postcodes that we do not have immunsiation data on
#List postcodes that have immunisation data:
Postcodes_with_immune_data <- subset(immunisation_data, Number.fully.immunised != "NP")["Postcode"]

##List postcodes that also have postal code area data:
POAs <- as(Postcodes_areas, "data.frame")
POAs <- transform(POAs, POA_CODE = as.numeric(levels(POA_CODE))[POA_CODE])
POAs_with_data <- subset(POAs, POA_CODE %in% t(Postcodes_with_immune_data["Postcode"]))

##Filter postcode area spatial data:
Postcode_areas_with_data <- Postcodes_areas[as.numeric(levels(Postcodes_areas$POA_CODE))[Postcodes_areas$POA_CODE] 
                                            %in% t(Postcodes_with_immune_data["Postcode"]),]

##Fiter immunisation data by NP and whether there is postal code area data:
Postcodes_with_area_and_immune_data <- subset(immunisation_data, Postcode %in% POAs_with_data[,"POA_CODE"])

####################################### DATA EXPLORATION ###########################################

not <- Postcodes_with_area_and_immune_data[,"Number.not.fully.immunised"]
postcode_population <- Postcodes_with_area_and_immune_data[,"Number.of.registered.children"]

sum(not == "NP")
sum(postcode_population == "NP")

total_not_immunised <- sum(as.numeric(levels(not))[not])
population <- sum(as.numeric(levels(postcode_population))[postcode_population])

nation_wide_rate = total_not_immunised/population
nation_wide_rate

##Mean, median 1st and 3rd quartiles of postcode rates:
Postcodes_with_area_and_immune_data["rate_not_immunised"] = as.numeric(levels(not))[not]/as.numeric(levels(postcode_population))[postcode_population]

head(Postcodes_with_area_and_immune_data["rate_not_immunised"])
sum(is.na(Postcodes_with_area_and_immune_data["rate_not_immunised"]))
sum(Postcodes_with_area_and_immune_data["rate_not_immunised"] <= 0 | Postcodes_with_area_and_immune_data["rate_not_immunised"] >= 1)

summary(Postcodes_with_area_and_immune_data["rate_not_immunised"])

##Histogram of rates
hist(x=Postcodes_with_area_and_immune_data[,"rate_not_immunised"], xlim =c(0,0.5), xlab="Fraction of 1 yr. old children not immunised", breaks=75, main="", ylab="Number of postcodes")

################################# CREATE NEIGHBOURHOOD MATRIX ######################################

#Generate IDs and coordinates 
Postcode_areas_with_data_frame <- as(Postcode_areas_with_data, "data.frame")
IDs <- row.names(Postcode_areas_with_data_frame)
coords <- coordinates(Postcode_areas_with_data)

##Create graph of neighbours
#Two postcodes are neighbours iff they share a boundary 
POA_neighbour_graph <- poly2nb(Postcode_areas_with_data, queen=FALSE)

#Have a look at the neighbourhood graph
POA_neighbour_graph
plot(POA_neighbour_graph, coords)

##Create neighbours matrix
#This is a square matrix where rows and columns are indexed by postcodes
#The [i,j] entry is = 1 if and only if postcode i is a neighbour of postcode j (or visa versa)
Postcode_neighbours <- matrix(0, nrow=length(IDs), ncol=length(IDs))
for (i in 1:length(POA_neighbour_graph)) {
  Postcode_neighbours[i,i] <- 1
  for (j in POA_neighbour_graph[i]) {
    Postcode_neighbours[i,j] <- 1
    Postcode_neighbours[j,i] <- 1
  }
}

#Checks
for (i in 1:10) {
  for (j in POA_neighbour_graph[i]) {
    print(paste(paste(as.character(i), " connected to "), as.character(j)))
  }
}
POA_neighbour_graph[1:10]
Postcode_neighbours[1:10,1:10]

#How to get Postcode back from index 1:n:
#If index is i then postcode is POAs_with_data[i,1]
#e.g.
POAs_with_data[2,1]

###################################################################################################

#Data is stored in:
#   - Postcode_neighbours matrix - this is the matrix of neighbours - postcodes i and j are neighbours
#      iff entry [i,j] is 1
#   - Immunisation data - filtered by whether there is immunisation data & postal code area data - 
#      is stored in Postcodes_with_area_and_immune_data

#################################### EXPORT MODEL DATA ############################################

#Assumptions:
#1) Model data results are stored in data.frame 'results'
#'results' has a column 'mean' - this is what we want to map
#'results' has a column 'ID' - this is the index (ID) of the postcode
#That is - postcode_n = 1 means it corresponds to the first row of 
# Postcodes_with_area_and_immune_data
#2) POAs_with_data has a column 'postcode_n' that corresponds to the 'ID' column in 'results

#Get postcode area code:
POAs_with_data["ID"] = POAs_with_data["postcode_n"]
merged <- merge(results, POAs_with_data,by="ID")

#for some reason results has 2 rows for each postcode
#Just take the '.id' = postcode_n row: (ask Mikkel tomorrow)

export <- merged[merged['.id'] == "postcode_n",]

#take the columns we care about
export2 <- cbind(export["POA_CODE"], export["mean"])

#Pad out three digit numbers into 4 digit numbers
export2["POA_CODE"] <- sprintf("%04d", export2[,"POA_CODE"])

#output as .csv file to put into Google Fusion Tables:
write.csv(export2, "results table.csv")
