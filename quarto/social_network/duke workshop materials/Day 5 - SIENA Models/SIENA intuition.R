# An intuititve introduction to SIENA's actor-oriented approach

library(statnet)
data(sampson)
data(samplk)

# Bring in data on formal role (higher=more status)
statusRaw <- c(5,5,5,3,3,3,2,2,2,2,2,2,1,1,1,1,1,1)

# Let's look at a network, with nodes shaded proportional to their status
# Sampson's data, brothers and their liking relations
g.layout <- gplot(samplike, usearrows=F)  # use the aggregated version of the data

par(mfrow=c(2,2))
gplot(samplk1, vertex.cex=statusRaw**.7, coord=g.layout, main='time 1')
gplot(samplk2, vertex.cex=statusRaw**.7, coord=g.layout, main='time 2')
# with edgelists we can easily calculate changes
gplot(samplk2-samplk1, vertex.cex=statusRaw**.7, coord=g.layout, main='ties added')
gplot(samplk1-samplk2, vertex.cex=statusRaw**.7, coord=g.layout, main='ties dropped')


# Question: How do we get from t1 to t2?
#  What rules might the brothers follow in changing their ties?
#  Keep in mind:
#   Ties are directed
#   Brothers are limited to information on hand (network and status)
#  Types of rules:
#  1. Ego-based ___________________________
#  2. Alter-based ___________________________
#  3. Dyad-based ___________________________
#  4. Network-based ___________________________

# Create a model based on alter's status
# - Brothers prefer ties to higher status alters

# Take the network at t1, allow it to evolve one tie at at time
#  If the network is evolving, which tie changes first?
#  - How do we decide?
#  Let's pick a node at random, and one of its ties.
#  - But again, which tie?
#  Consider all dyads...pick one tie to add (or drop).
#  - Evaluate each dyad based on model
#  - Give a higher "weight" to ties that the model favors
#    - Ties meeting model criteria are more likely to be added or kept
#  - What should we do in case of ties?
#    - Add a small bit of random noise to each weight

# Specify the model
#  Create statistic to help weight each i-j tie based on each rule
#   Alter status
altZ <- function(i,j,x,z) { z[j]*sum(x[,j]) }

#  Additional statistics that could be used
#   Ego status
egoZ <- function(i,j,x,z) { z[i]*sum(x[i,]) }
#   Dissimilarity on status
absZ <- function(i,j,x,z) { abs(z[i]-z[j]) }
#   Dyadic reciprocity
rec <- function(i,j,x) { x[i,j]*x[j,i] }
#   Outdegree
outD <- function(i,j,x) { sum(x[i,]) }

### Model with alter effect only
# Create objects whose values won't change
j <- 0    # initialize only so model object can be created
model <- c(altZ)
( nChanges <- sum(abs( as.sociomatrix(samplk1) - as.sociomatrix(samplk2) )) )
nActors <- 18 
z <- statusRaw

# Run 1. Initialize some values for this run
params <- 2   # set weight of parameter
x <- as.sociomatrix(samplk1) 

# Run the model
for (m in 1:nChanges) {
  # select random actor i
  i <- sample(1:18, 1)
  # what would be the utility of each possible change, alter j
  #  create a vector to store utilities
  # initialize at low value (-9999) to make tie to self unlikely
  util <- rep(-9999,18)  
  for (j in 1:18) {
    if ( i != j ) {
      # calculate utility of changing each tie 
      util[j] <- sum(model[[1]](i,j,x,z) * params, rgamma(1, .1))
      if ( x[i,j] == 1) { util[j] <- -util[j] }  # multiply by -1 if losing tie
      }
    }
  # plot(util[-i])  # which is highest?
  jFlip <- which(util==max(util))
  x[i,jFlip] <- 1-x[i,jFlip]  # flip the tie with greatest utility
}

# Examine the new network in comparison to the time 1 network
par(mfrow=c(1,2))
gplot(samplk1, vertex.cex=statusRaw**.7, coord=g.layout)
gplot(x, vertex.cex=statusRaw**.7, coord=g.layout)
# Lots of ties have been added

# Is this a good model? How can we tell a good model? 
#  A good model should reproduce time 2 network
#  - Especially the effect(s) we modeled

obsStat <- sum(as.sociomatrix(samplk2) %*% z)
simStat <- sum(x %*% z)
( fit <- data.frame( statistic=c('altZ'), obs=c( obsStat), sim=c(simStat) ) )
# The altZ statistic is too high


# Run 2. reduce weight of parameter to 1, then rerun the model
params <- 1   
x <- as.sociomatrix(samplk1)   # re-initialize network
for (m in 1:nChanges) {
  i <- sample(1:18, 1)
  util <- rep(-9999,18)  # initialize at low number to make tie to self unlikely
  for (j in 1:18) {
    if ( i != j ) {
      util[j] <- sum(model[[1]](i,j,x,z) * params, rgamma(1, .1))
      if ( x[i,j] == 1) { util[j] <- -util[j] }  # multiply by -1 if losing tie
      }
    }
  jFlip <- which(util==max(util))
  x[i,jFlip] <- 1-x[i,jFlip]  # flip the tie with greatest utility
}
obsStat <- sum(as.sociomatrix(samplk2) %*% z)
simStat <- sum(x %*% z)
( fit <- data.frame( statistic=c('altZ'), obs=c( obsStat), sim=c(simStat) ) )
# The altZ statistic is too high


# Run 3. Rerun with even lower altZ parameter
params <- .1   
x <- as.sociomatrix(samplk1)   # re-initialize network
for (m in 1:nChanges) {
  i <- sample(1:18, 1)
  util <- rep(-9999,18)  # initialize at low number to make tie to self unlikely
  for (j in 1:18) {
    if ( i != j ) {
      util[j] <- sum(model[[1]](i,j,x,z) * params, rgamma(1, .1))
      if ( x[i,j] == 1) { util[j] <- -util[j] }  # multiply by -1 if losing tie
      }
    }
  jFlip <- which(util==max(util))
  x[i,jFlip] <- 1-x[i,jFlip]  # flip the tie with greatest utility
}
obsStat <- sum(as.sociomatrix(samplk2) %*% z)
simStat <- sum(x %*% z)
( fit <- data.frame( statistic=c('altZ'), obs=c( obsStat), sim=c(simStat) ) )
# The altZ statistic is still too high


# 4. Rerun with even lower altZ parameter
params <- .001   
x <- as.sociomatrix(samplk1)   # re-initialize network
for (m in 1:nChanges) {
  i <- sample(1:18, 1)
  util <- rep(-9999,18)  # initialize at low number to make tie to self unlikely
  for (j in 1:18) {
    if ( i != j ) {
      util[j] <- sum(model[[1]](i,j,x,z) * params, rgamma(1, .1))
      if ( x[i,j] == 1) { util[j] <- -util[j] }  # multiply by -1 if losing tie
      }
    }
  jFlip <- which(util==max(util))
  x[i,jFlip] <- 1-x[i,jFlip]  # flip the tie with greatest utility
}
obsStat <- sum(as.sociomatrix(samplk2) %*% z)
simStat <- sum(x %*% z)
( fit <- data.frame( statistic=c('altZ'), obs=c( obsStat), sim=c(simStat) ) )
# The altZ statistic is still too high


### Why are we so far off? ###








# Model 2. make it costly to have more ties by adding negative outdegree effect
model <- c(altZ, outD)

# initialization
x <- as.sociomatrix(samplk1)   
params <- c(.1, -2)   # set model parameters

# Run the model once
for (m in 1:nChanges) {
  # select random actor i
  i <- sample(1:18, 1)
  # what would be the utility of each possible change, alter j
  #  create a vector to store utilities
  util <- rep(-9999,18)  # initialize at low number to make tie to self unlikely
  for (j in 1:18) {
    if ( i != j ) {
      # calculate utility of changing each tie 
      util[j] <- sum(model[[1]](i,j,x,z) * params[1], model[[2]](i,j,x) * params[2], rgamma(1, .1))
      if ( x[i,j] == 1) { util[j] <- -util[j] }  # multiply by -1 if losing tie
    }
  }
  jFlip <- which(util==max(util))
  x[i,jFlip] <- 1-x[i,jFlip]  # flip the tie with greatest utility
}

# Calculate the observed and simulated t2 statistic
obsStat <- sum(as.sociomatrix(samplk2) %*% z)
simStat <- sum(x %*% z)
( fit2 <- data.frame(
  statistic=c('density','altZ'),
  obs=c(sum(as.sociomatrix(samplk2)), obsStat),
  sim=c(sum(x), simStat)
) )
# Simulated statistics are too low


# Create a function to do a parameter sweep
#   Also track fit of outdegree distribution
siena09 <- function(x, params) {
  for (m in 1:nChanges) {
    i <- sample(1:18, 1)
    util <- rep(-9999,18)  # initialize at low number to make tie to self unlikely
    for (j in 1:18) {
      if ( i != j ) {
   	   util[j] <- sum(model[[1]](i,j,x,z) * params[1], model[[2]](i,j,x) * params[2], rgamma(1, .1))
       if ( x[i,j] == 1) { util[j] <- -util[j] }  # multiply by -1 if losing tie
      }
    }
    jFlip <- which(util==max(util))
    x[i,jFlip] <- 1-x[i,jFlip]  # flip the tie with greatest utility
  }
  obsStat <- sum(as.sociomatrix(samplk2) %*% z)
  simStat <- sum(x %*% z)
  ( fit <- data.frame(
    statistic=c('outdegree','altZ'),
    obs=c(sum(as.sociomatrix(samplk2)), obsStat),
    sim=c(sum(x), simStat)
  ) )
  return(fit)
}

#  Begin the parameter sweep
altZ_val <- seq(-2,2,.5)  # values for altZ parameter
outD_val <- seq(-2,2,.5)  # values for outD parameter
pred_altZ <- matrix(0, nrow=length(altZ_val), ncol=length(outD_val))
pred_outD <- matrix(0, nrow=length(altZ_val), ncol=length(outD_val))
for (a in 1:length(altZ_val)) {
  for (b in 1:length(outD_val)) {
    temp <- siena09(
      x <- as.sociomatrix(samplk1),   
      params <- c(altZ_val[a],outD_val[b]) 
    )
    pred_altZ[a,b] <- temp[2,3]
    pred_outD[a,b] <- temp[1,3]
    }
  }
# transfrom fits into deviations from observed
#  Rows are altZ values
#  Columns are outD values
dev_altZ <- abs(pred_altZ-temp[2,2])
dev_outD <- abs(pred_outD-temp[1,2])
# standardize scores
devZ_altZ <- (dev_altZ-mean(dev_altZ)) / sd(dev_altZ)
devZ_outD <- (dev_outD-mean(dev_outD)) / sd(dev_outD)
sd(devZ_altZ)
sd(devZ_outD)
devZ <- devZ_altZ + devZ_outD

# Plot separate, then the sum
#   Region with lowest deviation is best fitting (for that measure)
dev.off()
filled.contour(y=altZ_val, x=outD_val, z=devZ_altZ, zlim=c(min(devZ_altZ),max(devZ_altZ)), nlevels=50, axes=TRUE, 
               color.palette=colorRampPalette(c("white","yellow", "green", "blue")),
               main = "Deviation from Observed: Alter Status",
               ylab = "alter Z", xlab = "outdegree" )

filled.contour(y=altZ_val, x=outD_val, z=devZ_outD, zlim=c(min(devZ_outD),max(devZ_outD)), nlevels=50, axes=TRUE, 
               color.palette=colorRampPalette(c("white","yellow", "green", "blue")),
               main = "Deviation from Observed: Outdegree",
               ylab = "alter Z", xlab = "outdegree" )

filled.contour(y=altZ_val, x=outD_val, z=devZ, zlim=c(min(devZ),max(devZ)), nlevels=50, axes=TRUE, 
               color.palette=colorRampPalette(c("white","yellow", "green", "blue")),
               main = "Deviation from Observed: Outdeg + Alter Z",
               ylab = "alter Z", xlab = "outdegree" )



 

#  Run a true SIENA model
library(RSiena)
liking <- sienaDependent(array(c(
  as.sociomatrix(samplk1), as.sociomatrix(samplk2)), dim=c(18,18,2) ))
status <- coCovar(statusRaw, center=F)
sampNet <- sienaDataCreate(liking, status)

myeff <- getEffects(sampNet)
myeff <- setEffect(myeff, recip, initialValue=0, fix=T)
#myeff <- includeEffects(myeff, egoX, interaction1="status", name="liking")
myeff <- includeEffects(myeff, altX, interaction1="status", name="liking")
#myeff <- includeEffects(myeff, simX, interaction1="status", name="liking")

modelOptions <- sienaAlgorithmCreate(
  projname='siena intuition', doubleAveraging=0, diagonalize=.2)  

myResults <- siena07(modelOptions, data=sampNet, effects= myeff, batch=FALSE)
summary(myResults)
