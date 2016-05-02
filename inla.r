install.packages("INLA", repos="https://www.math.ntnu.no/inla/R/stable")
library("INLA")
data(Germany)
#Germany <- read.csv("C:/Users/Mikkel/Desktop/ANU/Inference/Presentation/R/Germany.txt", sep="")

g = system.file("demodata/germany.graph", package="INLA")
source(system.file("demodata/Bym-map.R", package="INLA"))
summary(Germany)

## just make a duplicated column
Germany = cbind(Germany,region.struct=Germany$region)

Germany$sum = sum(Germany$Y)/sum(Germany$x)
Germany$E_1 = Germany$sum*Germany$x

# standard BYM model (without covariates)
formula1 = Y ~ f(region.struct,model="besag",graph=g) +
  f(region,model="iid")


result1  =  inla(formula1,family="poisson",data=Germany,E=E)

#Printing the results in a dataframe - this can be useful when we need to map the data
df = ldply(result1$summary.random$region.struct$mean)
# with linear covariate
formula2 = Y ~ f(region.struct,model="besag",graph.file=g) +
  f(region,model="iid") + x

result2 =  inla(formula2,family="poisson",data=Germany,E=E)

# with smooth covariate
formula3 = Y ~ f(region.struct,model="besag",graph.file=g) +
  f(region,model="iid") + f(x, model="rw2")

result3 =  inla(formula3,family="poisson",data=Germany,E=E)
inla.pause()

dev.new()
par(mfrow=c(2,2))
Bym.map(result1$summary.random$region.struct$mean)
Bym.map(result2$summary.random$region.struct$mean)
Bym.map(result3$summary.random$region.struct$mean)


## Alternative

prior.iid = c(1,0.01)
prior.besag = c(1,0.001)
initial.iid = 4
initial.besag = 3

formula1.bym = Y ~ f(region, model = "bym", graph.file = g,
                     param = c(prior.iid, prior.besag),
                     initial = c(initial.iid, initial.besag))
result1.bym = inla(formula1.bym,family="poisson",data=Germany,E=E)

Bym.map(result1.bym$summary.random$region.struct$mean)
