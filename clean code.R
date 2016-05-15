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

################################### FILTER POSTCODES BY NAs ########################################
#Remove postcodes that we do not have immunsiation data on
#List postcodes that have immunisation data:
Postcodes_with_data <- subset(immunisation_data, Number.fully.immunised != "NP")["Postcode"]

#List postcodes that also have postal code area data:
POAs <- as(Postcodes_areas, "data.frame")
POAs <- transform(POAs, POA_CODE = as.numeric(levels(POA_CODE))[POA_CODE])
POAs_with_data <- subset(POAs, POA_CODE %in% t(Postcodes_with_data["Postcode"]))

##Filter postcode area spatial data:
Postcode_areas_with_data <- Postcodes_areas[as.numeric(levels(Postcodes_areas$POA_CODE))[Postcodes_areas$POA_CODE] 
                                            %in% t(Postcodes_with_data["Postcode"]),]

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

