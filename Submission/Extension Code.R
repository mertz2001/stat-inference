rm(list = ls())

################################## LOAD REQUIRED LIBRARIES #########################################

#Mapping libraries:
library(rgeos)
library(maptools)
library(spdep)
library("rgdal")
#install.packages("INLA", repos="https://www.math.ntnu.no/inla/R/stable") # Should be installed using this path
library("INLA")
library("plyr")
library("dplyr")
####################################### IMPORT DATA ################################################

#Get the immunisation data:
setwd("/Users/jamesbailie/Documents/uni/STAT3013/Project/stat-inference")
immunisation_data <- read.csv("1 year fully immunised.csv")

#Get the postcode area data:
setwd("/Users/jamesbailie/Documents/uni/STAT3013/Project/stat-inference/Postcode data/1270055003_poa_2011_aust_shape")
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

#################################### RUNNING THE MODELS ###########################################

# Define the Graph/neigborhood matrix
g = Postcode_neighbours

# First Look at the data
summary(immunisation_data)

## Dropping NA's
immunisation = left_join(POAs_with_data, immunisation_data, by=c("POA_CODE"="Postcode"))

# Changing Factors to numeric values
immunisation$Y = as.numeric(levels(immunisation$Number.not.fully.immunised))[immunisation$Number.not.fully.immunised]
immunisation$X = as.numeric(levels(immunisation$Number.of.registered.children))[immunisation$Number.of.registered.children]

# Generating column with postcodes 1 to n corresponding to real postcodes in the neighborhood 
# matrix This is nessesary for the Inla program to collect neighborhood information
POAs_with_data$postcode_n = 1:nrow(POAs_with_data)
immunisation = left_join(POAs_with_data, immunisation, by=c("POA_CODE"))

#Generating extra column of postcodes for the BYM model
immunisation = cbind(immunisation,postcode_struct=immunisation$postcode_n)

# Model 1 ~ iid model specification
formula2 = Y ~ f(postcode_n,model="iid")

# Using Inla to compute the relative risk estimates
result2  =  inla(formula2,family="binomial",data=immunisation,Ntrials=X, 
                 control.compute = list(waic=TRUE, dic=TRUE, cpo=TRUE))

# First look at relative risk ratio
round(result2$summary.fitted.values[1:5,1],4)

# Getting the IID relative risk estimates in dataframe for plotting
iid_results = as.data.frame(t(ldply(result2$summary.fitted.values)[2:1080]))
colnames(iid_results) = c("Mean", "sd", "0.25 quant", "0.5 quant","0.975 quant", "mode")

# Adding column with postcode numbers
iid_results$ID = seq.int(nrow(iid_results))


# Collecting the Exeedence risk estimates from the iid model in a dataframe for plots
a = 0 # Setting the threshold to 1 , log(1) = 0
UHexceedence_iid <- lapply(result2$marginals.random$postcode_n, function(X){
  1-inla.pmarginal(a, X) })
UHexceedence_iid = ldply(UHexceedence_iid)

# Adding column with postcodes numbers
UHexceedence_iid$ID = seq.int(nrow(UHexceedence_iid))


# Model 2 ~ BYM model specification, 2 components - the iid model and the besag model
formula1 = Y ~ f(postcode_struct,model="besag",graph=g) +
  f(postcode_n,model="iid")

# Using Inla to find the relative risk estimates
result1  =  inla(formula1,family="binomial",data=immunisation,Ntrials=X, 
                 control.compute = list(waic=TRUE, dic=TRUE, cpo=TRUE))


BYM_results = as.data.frame(t(ldply(result1$summary.fitted.values)[2:1080]))
colnames(BYM_results) = c("Mean", "sd", "0.25 quant", "0.5 quant","0.975 quant", "mode")

# Adding column with postcode numbers
BYM_results$ID = seq.int(nrow(BYM_results))

# Calculating the exeedence probability (the Posterior spatial risk is greater than 1)
a = 0
UHexceedence_BYM<-lapply(result1$marginals.random$postcode_struct, function(X){
  1-inla.pmarginal(a, X) })
UHexceedence_BYM = ldply(UHexceedence_BYM)

# Adding column with postcode numbers
UHexceedence_BYM$ID = seq.int(nrow(UHexceedence_BYM))

# Comparring DIC and WAIC measures
# Model evaluation parameters
round(rbind(DIC=c(BYM=result1$dic$dic, IID=result2$dic$dic),
            WAIC=c(result1$waic$waic, result2$waic$waic)),2)


#################################### EXPORT MODEL DATA ############################################

#Generate the table we want to export:
export <- POAs_with_data[,c("ID", "POA_CODE")]
export <- merge(export, UHexceedence_iid[, c("ID", "V1")], by="ID")
export <- merge(export, UHexceedence_BYM[, c("ID", "V1")], by="ID")
export <- merge(export, iid_results[, c("ID", "Mean")], by="ID")
export <- merge(export, BYM_results[, c("ID", "Mean")], by="ID")
names(export) <- c("ID", "POA_CODE", "UHexceedence_iid", "UHexceedence_BYM", "RRF_iid", "RRF_BYM")

#Pad out three digit numbers into 4 digit numbers
export["POA_CODE"] <- sprintf("%04d", export[,"POA_CODE"])

#output as .csv file to put into Google Fusion Tables:
write.csv(export, "results table.csv")
