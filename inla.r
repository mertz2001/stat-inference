install.packages("INLA", repos="https://www.math.ntnu.no/inla/R/stable")
library("INLA")
library("plyr")
library("dplyr")

#Loading neighbor matrix
load("~/GitHub/stat-inference/poscodes.RData")

# Graph matrix
g = Postcode_neighbours

summary(immunisation_data)

## Dropping NA's
immunisation = left_join(POAs_with_data, immunisation_data, by=c("POA_CODE"="Postcode"))

# Factors as numeric values
immunisation$Y = as.numeric(levels(immunisation$Number.not.fully.immunised))[immunisation$Number.not.fully.immunised]
immunisation$X = as.numeric(levels(immunisation$Number.of.registered.children))[immunisation$Number.of.registered.children]

# Generating column with postcodes 1 to n corresponding to real postcodes
# This is nessesary for the Inla program to collect neighborhood information
POAs_with_data$postcode_n = 1:nrow(POAs_with_data)
immunisation = left_join(POAs_with_data, immunisation, by=c("POA_CODE"))

#Generating extra column of postcodes 
immunisation = cbind(immunisation,postcode_struct=immunisation$postcode_n)

# Model 1 ~ iid model specification
formula2 = Y ~ f(postcode_n,model="iid")

# Using Inla to find the relative risk estimates
result2  =  inla(formula2,family="binomial",data=immunisation,Ntrials=X, 
                 control.compute = list(waic=TRUE, dic=TRUE, cpo=TRUE))

# First look at relative risk ratio
round(result2$summary.fitted.values[1:5,1],4)

# Model evaluation parameters
result2$dic$dic
result2$waic$waic

iid_results = t(ldply(result2$summary.fitted.values)[2:1080])
colnames(iid_results) = c("Mean", "sd", "0.25 quant", "0.5 quant","0.975 quant", "mode")


UHexceedence_iid <- lapply(result2$marginals.random$postcode_struct, function(X){
  1-inla.pmarginal(a, X) })
UHexceedence_iid = ldply(UHexceedence_iid)



# Model 2 ~ BYM model specification, 2 components - the iid model and the besag model
formula1 = Y ~ f(postcode_struct,model="besag",graph=g) +
  f(postcode_n,model="iid")

# Using Inla to find the relative risk estimates
result1  =  inla(formula1,family="binomial",data=immunisation,Ntrials=X, 
                 control.compute = list(waic=TRUE, dic=TRUE, cpo=TRUE))

result1$dic$dic
result1$waic$waic

BYM_results = t(ldply(result1$summary.fitted.values)[2:1080])
colnames(BYM_results) = c("Mean", "sd", "0.25 quant", "0.5 quant","0.975 quant", "mode")

# Calculating the exeedence probability (the Posterior spatial risk is greater than 1)
a = 0
UHexceedence<-lapply(result1$marginals.random$postcode_struct, function(X){
  1-inla.pmarginal(a, X) })
UHexceedence = ldply(UHexceedence)

# Comparring DIC and WAIC measures

#save.image("~/GitHub/stat-inference/Results_BYM.RData")