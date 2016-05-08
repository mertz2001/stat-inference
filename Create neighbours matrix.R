##CREATE NEIGHBOURHOOD MATRIX:

#Load required libraries:
library(rgeos)
library(maptools)
library(spdep)
library("rgdal")

#Read data:
setwd("~/Documents/uni/STAT3013/Project/Extension/Postcode data/1270055003_poa_2011_aust_shape")
Postcodes <- readOGR(".", "POA_2011_AUST")

coords <- coordinates(Postcodes)
IDs <- row.names(as(Postcodes, "data.frame"))
Sy4_nb <- tri2nb(coords, row.names = IDs)
Sy5_nb <- graph2nb(soi.graph(Sy4_nb, coords), row.names = IDs)
Sy6_nb <- graph2nb(gabrielneigh(coords), row.names = IDs)
Sy7_nb <- graph2nb(relativeneigh(coords), row.names = IDs)

#Have a look at (one of) the neighbourhood graph
Sy5_nb
plot(Sy5_nb, coords)

#Create neighbours matrix
Postcode_neighbours <- matrix(0, nrow= length(IDs), ncol = length(IDs))
for (i in 1:length(Sy5_nb)) {
  Postcode_neighbours[i,i] <- 1
  for (j in Sy5_nb[i]) {
    Postcode_neighbours[i,j] <- 1
    Postcode_neighbours[j,i] <- 1
  }
}

#Checks
for (i in 1:10) {
  for (j in Sy5_nb[i]) {
    print(paste(paste(as.character(i), " connected to "), as.character(j)))
  }
}
Sy5_nb[1:10]
Postcode_neighbours[1:10,1:10]


row.names(Postcode_neighbours) <- as(Postcodes, "data.frame")[,"POA_CODE"]
colnames(Postcode_neighbours) <- as(Postcodes, "data.frame")[,"POA_CODE"]


#no discussion of how they choose neighbours
#we should make a logical choice and explain why we choose it

#MIGHT BE MAKING A MISTAKE - NOT ACCOUNTING FOR POSTCODES THAT ARE REMOVED
#Dropping null geometries: 2514, 2515, 2516 - these are the last three!
#So they won't mess up what I've done
#But what are these geometries? Are they POAs? Which POAs?

###Create neighbours matrix - take 2 - filtered by NAs
library(rgeos)
library(maptools)
library(spdep)
library("rgdal")
setwd("~/Documents/uni/STAT3013/Project/stat-inference")
immunisation_data <- read.csv("1 year fully immunised.csv")
head(immunisation_data)

Postcodes_with_data <- subset(immunisation_data, Number.fully.immunised != "NP")["Postcode"]
head(Postcodes_with_data)

setwd("~/Documents/uni/STAT3013/Project/stat-inference/Postcode data/1270055003_poa_2011_aust_shape")
Postcodes <- readOGR(".", "POA_2011_AUST")

#Pull out postcodes with data from the postcode shape file:
POAs <- as(Postcodes, "data.frame")
POAs <- transform(POAs, POA_CODE = as.numeric(levels(POA_CODE))[POA_CODE])
POAs_with_data <- subset(POAs, POA_CODE %in% t(Postcodes_with_data["Postcode"]))
IDs <- row.names(POAs_with_data)
coords <- coordinates(Postcodes)[row.names(coordinates(Postcodes)) %in% t(IDs),]

Sy4_nb <- tri2nb(coords, row.names = IDs)
Sy5_nb <- graph2nb(soi.graph(Sy4_nb, coords), row.names = IDs)
Sy6_nb <- graph2nb(gabrielneigh(coords), row.names = IDs)
Sy7_nb <- graph2nb(relativeneigh(coords), row.names = IDs)

#Have a look at (one of) the neighbourhood graph
Sy5_nb
plot(Sy5_nb, coords)

#Create neighbours matrix
Postcode_neighbours <- matrix(0, nrow= length(IDs), ncol = length(IDs))
for (i in 1:length(Sy5_nb)) {
  Postcode_neighbours[i,i] <- 1
  for (j in Sy5_nb[i]) {
    Postcode_neighbours[i,j] <- 1
    Postcode_neighbours[j,i] <- 1
  }
}

#Checks
for (i in 1:10) {
  for (j in Sy5_nb[i]) {
    print(paste(paste(as.character(i), " connected to "), as.character(j)))
  }
}
Sy5_nb[1:10]
Postcode_neighbours[1:10,1:10]

#Postcode_neighbours is a matrix of neighbours - row and column names are 1:n

#How to get Postcode back from index 1:n:
#If index is i then postcode is POAs_with_data[i,1]
#e.g.
POAs_with_data[2,1]

