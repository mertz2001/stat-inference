install.packages("INLA", repos="https://www.math.ntnu.no/inla/R/stable")
library("INLA")
library("dplyr")

#Loading neighbor matrix
load("~/GitHub/stat-inference/poscodes.RData")

# Graph matrix
g = Postcode_neighbours

summary(immunisation_data)

## Making a dataset consisting of those without NA's
immunisation = left_join(Postcodes_with_data, immunisation_data, by=c("Postcode"))

# Generating E
immunisation$Y = as.numeric(levels(immunisation$Number.not.fully.immunised))[immunisation$Number.not.fully.immunised]
immunisation$X = as.numeric(levels(immunisation$Number.of.registered.children))[immunisation$Number.of.registered.children]
immunisation$total_not_immunised = sum(immunisation$Y, na.rm = TRUE)
immunisation$total_population = sum(immunisation$X, na.rm = TRUE)
immunisation$A = immunisation$total_not_immunised/immunisation$total_population
immunisation$E = immunisation$A*immunisation$X

# Generating column with postcodes 1 to n corresponding to real postcodes
POAs_with_data$postcode_n = 1:nrow(POAs_with_data)
immunisation = left_join(POAs_with_data, immunisation, by=c("POA_CODE"="Postcode"))

#Generating extra column of postcodes 
immunisation = cbind(immunisation,postcode_struct=immunisation$postcode_n)

# standard BYM model (without covariates)
formula1 = Y ~ f(postcode_struct,model="besag",graph=g) +
  f(postcode_n,model="iid")

result1  =  inla(formula1,family="poisson",data=immunisation,E=E)

#Printing the results in a dataframe - this can be useful when we need to map the data
results = ldply(result1$summary.random)

save.image("~/GitHub/stat-inference/Results_BYM.RData")