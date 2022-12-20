# SIENA tutorial using R for the Social Networks and Health 
#   workshop at Duke University on May 18, 2018
# Written by David R. Schaefer
# Adapted from tutorial by Christian Steglich
#   http://www.gmw.rug.nl/~steglich/workshops/Groningen2015.htm

##############################################
# Contents:
#     (1) preparation
#     (2) inspect data
#     (3) prepare objects for RSiena
#     (4) model specification
#     (5) model estimation
#     (6) goodness of fit
#     (7) time heterogeneity
#     (8) interpretation
#     (9) simulations
##############################################

# ==========================
# (1) preparatory steps
# ==========================

# Install the RSiena package
#install.packages("RSiena") # latest is (1.2-11)
# Sometimes there is a slightly newer version available via R-Forge 
# It requires compilation for macs. But if you insist, un-hash and run
# install.packages("RSiena", repos="http://R-Forge.R-project.org") 
# Answer "y" to the question of whether you want to install from source

# Optionally, install the RSienaTest package (i.e., beta version) 
# install.packages("RSienaTest", repos="http://R-Forge.R-project.org")

# Load RSiena commands and sna to the R namespace
library(RSiena)
library(sna)
# install.packages("lattice")
library(lattice)  # for plotting

# To check your version 
packageVersion("RSiena")

# Set working directory to where you want to store output files:
#setwd("/Users/davidschaefer/Google Drive 2/UCI/Teaching/Duke/lab/output/")

# Read in some data
# This data will complement data that comes with the RSiena package
temp <- tempfile()
download.file("http://www.stats.ox.ac.uk/~snijders/siena/s50_data.zip",temp)
s50d <- as.matrix(read.table(unz(temp, "s50-drugs.dat")))
s50t <- as.matrix(read.table(unz(temp, "s50-sport.dat")))
s50f <- as.matrix(read.table(unz(temp, "s50-familyevent.dat")))
unlink(temp)

# Create a series of functions that will help interpret model performance & output 
# Run from here to line ~215
igraphNetworkExtraction <- function(i, data, sims, period, groupName, varName){
     require(igraph)
     dimsOfDepVar<- attr(data[[groupName]]$depvars[[varName]], "netdims")
     missings <- is.na(data[[groupName]]$depvars[[varName]][,,period]) |
                 is.na(data[[groupName]]$depvars[[varName]][,,period+1])
     if (is.null(i)) {
   # sienaGOF wants the observation:
       original <- data[[groupName]]$depvars[[varName]][,,period+1]
       original[missings] <- 0
       returnValue <- graph.adjacency(original)
     }
     else
     {
       missings <- graph.adjacency(missings)
   #sienaGOF wants the i-th simulation:
       returnValue <- graph.difference(
       graph.empty(dimsOfDepVar) +
           edges(t(sims[[i]][[groupName]][[varName]][[period]][,1:2])),
                missings)
     }
     returnValue
   }
   
GeodesicDistribution <- function (i, data, sims, period, groupName,
                           varName, levls=c(1:5,Inf), cumulative=TRUE, ...) {
     x <- networkExtraction(i, data, sims, period, groupName, varName)
     require(sna)
     a <- sna::geodist(symmetrize(x))$gdist
     if (cumulative)
     {
       gdi <- sapply(levls, function(i){ sum(a<=i) })
     }
	 else
     {
       gdi <- sapply(levls, function(i){ sum(a==i) })
     }
     names(gdi) <- as.character(levls)
     gdi
   }

   # Holland and Leinhardt Triad Census; see ?sna::triad.census.
   TriadCensus <- function(i, data, sims, wave, groupName, varName, levls=1:16){
       unloadNamespace("igraph") # to avoid package clashes
       require(sna)
       require(network)
       x <- networkExtraction(i, data, sims, wave, groupName, varName)
	   if (network.edgecount(x) <= 0){x <- symmetrize(x)}
       # because else triad.census(x) will lead to an error
       tc <- sna::triad.census(x)[1,levls]
       # names are transferred automatically
       tc
   }

  # Distribution of Bonacich eigenvalue centrality; see ?igraph::evcent.
  EigenvalueDistribution <- function (i, data, sims, period, groupName, varName,
                           levls=c(seq(0,1,by=0.125)), cumulative=TRUE){
     require(igraph)
     x <- igraphNetworkExtraction(i, data, sims, period, groupName, varName)
     a <- igraph::evcent(x)$vector
     a[is.na(a)] <- Inf
     lel <- length(levls)
     if (cumulative)
     {
       cdi <- sapply(2:lel, function(i){sum(a<=levls[i])})
     }
     else
     {
       cdi <- sapply(2:lel, function(i){
                     sum(a<=levls[i]) - sum(a <= levls[i-1])})
     }
     names(cdi) <- as.character(levls[2:lel])
     cdi
    }
    
MoranGeary <- function(i, data, sims, wave, groupName, varName, levls=1:2){
	#unloadNamespace("igraph") # to avoid package clashes
	require(sna)
	require(network)
	x <- as.sociomatrix(networkExtraction(i, data, sims, wave, groupName, varName[1]))
	z <- behaviorExtraction(i,data,sims,wave,groupName,varName[2])
	n <- length(z)
	z.ave <- mean(z,na.rm=TRUE)
	numerator <- n*sum(x*outer(z-z.ave,z-z.ave),na.rm=TRUE)
	denominator <- sum(x,na.rm=TRUE)*sum((z-z.ave)^2,na.rm=TRUE)
	res <- numerator/denominator
	numerator <- (n-1)*sum(x*(outer(z,z,FUN='-')^2),na.rm=TRUE)
	denominator <- 2*sum(x,na.rm=TRUE)*sum((z-z.ave)^2,na.rm=TRUE)
	res[2] <- numerator/denominator
	names(res) <- c("Moran","Geary")
	return(res)
}

Moran123 <- function(i, data, sims, wave, groupName, varName, levls=1){
	#unloadNamespace("igraph") # to avoid package clashes
	require(sna)
	require(network)
	x <- as.sociomatrix(networkExtraction(i, data, sims, wave, groupName, varName[1]))
	z <- behaviorExtraction(i,data,sims,wave,groupName,varName[2])
	# handle missing data [not checked if this makes sense]:
	x[is.na(x)] <- 0
	z[is.na(z)] <- mean(z,na.rm=TRUE)
	res <- nacf(x,z,lag.max=3,typ="moran")[2:4]
	names(res) <- c("d=1","d=2","d=3")
	return(res)
}

Geary123 <- function(i, data, sims, wave, groupName, varName, levls=1){
	#unloadNamespace("igraph") # to avoid package clashes
	require(sna)
	require(network)
	x <- as.sociomatrix(networkExtraction(i, data, sims, wave, groupName, varName[1]))
	z <- behaviorExtraction(i,data,sims,wave,groupName,varName[2])
	# handle missing data [not checked if this makes sense]:
	x[is.na(x)] <- 0
	z[is.na(z)] <- mean(z,na.rm=TRUE)
	res <- nacf(x,z,lag.max=5,typ="geary")[2:4]
	names(res) <- c("d=1","d=2","d=3")
	return(res)
}

EgoAlterTable <- function(i, data, sims, wave, groupName, varName, levls=1){
	#unloadNamespace("igraph") # to avoid package clashes
	#require(sna)
	#require(network)
	x <- as.sociomatrix(networkExtraction(i, data, sims, wave, groupName, varName[1]))
	z <- behaviorExtraction(i,data,sims,wave,groupName,varName[2])
	res <- matrix(0,nr=5,nc=5)
	for (ego in 1:5) {
	for (alt in 1:5) {
		thesum <- sum(x[z==ego,z==alt],na.rm=TRUE)
		if (thesum>0) {
			res[ego,alt] <- thesum
		}
	}}
	thenames <- paste('e',col(res),'a',row(res),sep='')
	res <- c(t(res))
	names(res) <- thenames
	return(res)
}

outTable <- function(x) {
	coef <- abs(x$theta)
	coefPretty <- sprintf("%.3f", round(coef,3))
	se <- diag(x$covtheta)**.5
	sePretty <- sprintf("%.3f", round(se,3))
	pval <- 2*pnorm(-abs(coef/se))
	symp <- symnum(pval, corr = FALSE,
               cutpoints = c(0,  .001,.01,.05, .1, 1),
               symbols = c("***","**","*","."," "))
    convPretty <- sprintf("%.3f", round(abs(x$tconv),3))
    out1 <- noquote(cbind(
		Function = x$effects[[1]], 
		Effect = x$effects[[2]], 
		Coef = coefPretty, 
		StEr = sePretty, 
		Sig = symp, 
		Conv = convPretty))
	out2 <- paste("Maximum Convergence Ratio:", round(x$tconv.max,3))
	return(list(out1,out2))
}

# ======================
# (2) inspect data
# ======================

# We'll use the s50 data, which are part of the RSiena package
?s50
# Longitudinal data on networks, alcohol use and smoking
# For more info and data: http://www.stats.ox.ac.uk/~snijders/siena/s50_data.htm
# and http://www.stats.ox.ac.uk/~snijders/siena/PearsonSteglichSnijders2006.pdf

# Alcohol use is in an object named 's50a'
dim(s50a)  # 50 students (rows), 3 time points (columns)
head(s50a, 10)  # first 10 rows of data
apply(s50a,FUN=table,MARGIN=2,useNA='always')  # freq. distribution by wave
# Alcohol use is already coded as ordinal - GOOD! (required for dependent behaviors)

# Is there sufficient change in alcohol use?
# Examine correlations and discrete changes
cor(data.frame(s50a))
table(s50a[,1],s50a[,2],useNA='always')
table(s50a[,2],s50a[,3],useNA='always')
# & total:
( totalChange <- table(s50a[,1],s50a[,3],useNA='always') ) 
sum(diag(totalChange)) / sum(totalChange)  # 50% at same level t1 & t3

# Perform the same steps for smoking
dim(s50s)  # 50 students (rows), 3 time points (columns)
head(s50s, 10)  # first 10 rows of data
apply(s50s,FUN=table,MARGIN=2,useNA='always')  # freq. distribution by wave
# Examine change across time
cor(data.frame(s50s))
table(s50s[,1],s50s[,2],useNA='always')
table(s50s[,2],s50s[,3],useNA='always')
( totalChange <- table(s50s[,1],s50s[,3],useNA='always') ) 
sum(diag(totalChange)) / sum(totalChange)    # 68% stable t1 to t3

# Examine the networks:
s501  # t1 network - it's a matrix
s50list <- list(s501,s502,s503)  
lapply(s50list, dim)   # each network has the same number of actors as needed
lapply(s50list, function(x) table(x, useNA='always'))  
# 113 ties at t1, 116 at t2, 122 at t3, none missing

# Because the networks are the same dimension we can stack them in an array
# This will make some things more efficient later
s50array <- array(c(s501,s502,s503),dim=c(dim(s501),3))
dim(s50array)  # 3 50x50 adjacency matrices layered atop one another

# Network change between subsequent observations:
(tab1to2 <- table(s501,s502,useNA='always') )
# 2328 0's and 57 1's remained stable; 
# 59 0's became 1's; 56 1's became 0's
# Measure stability using the Jaccard index (script only works if no missing ties)
# Jaccard should be minimum of .2 for RSiena
tab1to2[2,2] / (sum(tab1to2)-tab1to2[1,1])   # jaccard=.33

(tab2to3 <- table(s502,s503,useNA='always') )
tab2to3[2,2] / (sum(tab2to3)-tab2to3[1,1])   # jaccard=.38

# Create colors to shade nodes by substance use
color5 <- c('white','gray90','slategray','slateblue3','slateblue4')  # light to dark 
color3 <- c('white','red3','darkred')  # light to dark 

# Create and save a layout based on union of 3 networks
# We will use the same layout to graph the network at each wave
# Rerun the g.layout until it is pleasing
g.layout <- gplot(apply(s50array, c(1,2), max), usearrows=F)  

# Plot each network with nodes shaded by accompanying alcohol use
par(mfrow=c(1,3))
for (i in 1:3) {
	gplot(s50array[,,i], vertex.col=color5[s50a[,i]], arrowhead.cex=.75, edge.col='gray70', coord=g.layout, 	
	      vertex.cex=1.5, main=paste('Drinking t',i,sep="") )
	}
# Same plot but with nodes shaded by accompanying smoking
par(mfrow=c(1,3))
for (i in 1:3) {
  gplot(s50array[,,i], vertex.col=color3[s50s[,i]], arrowhead.cex=.75, edge.col='gray70', coord=g.layout, 	
        vertex.cex=1.5, main=paste('Smoking t',i,sep="") )
}
# Plot both smoking and alcohol (to shade nodes with both behaviors see https://osf.io/5u59k/ )
par(mfcol=c(2,3))
for (i in 1:3) {
  gplot(s50array[,,i], vertex.col=color5[s50a[,i]], arrowhead.cex=.75, edge.col='gray70', coord=g.layout, 	
        vertex.cex=1.5, main=paste('Drinking t',i,sep="") )
  gplot(s50array[,,i], vertex.col=color3[s50s[,i]], arrowhead.cex=.75, edge.col='gray70', coord=g.layout, 	
        vertex.cex=1.5, main=paste('Smoking t',i,sep="") )
}

# ================================
# (3) prepare objects for RSiena
# ================================

# Networks must be stored as a matrix (not an edgelist);
#   Each matrix is the same size;
#   Matrices are ultimately layered to form an array
# Convert network to "dependent network variable" (RSiena object):
friends <- sienaDependent(s50array)

# Behaviors to be modeled as an outcome must also be a matrix
#   where rows are actors and columns are time points
class(s50a)  # alcohol use is already a matrix
# Convert behavior to "dependent behavior variable" (RSiena object):
drinking <- sienaDependent(s50a,type="behavior")
smoking <- sienaDependent(s50s,type="behavior")

# Attributes are identified independently
# Examine the family event data
head(s50f)
# There is one fewer column than waves of network observations
#  * t1 value is used to predict t1-t2 transition
#  * t2 value is used to predict t2-t3 transition
# Treat family event is an exogenously changing covariate
familyEvent <- varCovar(s50f)
# Stable attributes would be treated as constant covariates ('coCovar')

# Bind data together for Siena analysis:
myDataDrinkSmoke <- sienaDataCreate(friends,drinking,smoking,familyEvent)
myDataDrinkSmoke

# We could also have no behaviors, just a network or a network and some attributes
myData <- sienaDataCreate(friends)
myData
myData <- sienaDataCreate(friends, familyEvent)
myData

# Any dependent objects specified during the binding step will be
#   included in the model by default. It is wise to only list
#   networks or behaviors you intend to model.

# Always write a descriptive summary file (a good check of coding):
print01Report(myDataDrinkSmoke, modelname='SIENA drink smoke 180518')

# ================================================
# (4) model specification / selection of effects
# ================================================

# create a model specification object for the data:
myEffects <- getEffects(myDataDrinkSmoke)
myEffects
# note that some effects are included by default

# Inspect all the possible effects:
effectsDocumentation(myEffects)  # writes an html file to working directory
# Note how column "name" has three names (scroll way down),
#   one for each function in the model
# For a fuller look at what the effects object click on it (in RStudio) or
#   uncomment the following (DO NOT run this in RStudio or risk freezing your machine)
#fix(myEffects)

# specify the model 
# drinking function: effects where drinking is the *outcome*

# Test whether drinking is 'contagious' (i.e., peer influence)
myEffects <- includeEffects(myEffects, avSim,
	interaction1="friends", name="drinking")
	# instead of 'avSim' could use 'totSim', 'avAlt', or 'totAlt'

# Test whether being lonely leads to drinking
myEffects <- includeEffects(myEffects, isolate,
	interaction1="friends", name="drinking")
	# instead of 'isolate' could use 'indegree' 

# network function: effects where friends is the *outcome* and drinking a predictor
# Test whether drinking helps one make friends
myEffects <- includeEffects(myEffects, egoX,
	interaction1="drinking", name="friends")

# Test whether drinking enhances (or reduces) popularity
myEffects <- includeEffects(myEffects, altX,
	interaction1="drinking", name="friends")

# Test whether actors select friends based on similar drinking level
#   (i.e., selection homophily)
myEffects <- includeEffects(myEffects, simX,
	interaction1="drinking", name="friends")
# instead of 'simX' could use 'egoXaltX' 

# Add structural effects as control variables:
myEffects <- includeEffects(myEffects, recip, transTrip,
	name="friends")

# Control for selection on family event
myEffects <- includeEffects(myEffects, egoX, altX, sameX,
    interaction1='familyEvent', name="friends")

# Control for effect of family event on drinking
myEffects <- includeEffects(myEffects, effFrom,
    interaction1='familyEvent', name="drinking")

# Test whether an interaction between reciprocity and transitivity should be included
myEffects <- setEffect(myEffects, transRecTrip,
    include=T, fix=T, test=T)
# See Schweinberger (2012) for more info. on this score test
# and Block (2014) for why this interaction is important

# Full model specification looks like this now:
myEffects
# Note that transitive reciprocated triplets is fixed (at 0) and tested for inclusion

# Add comparable effects for smoking (with more efficient code)
myEffects <- includeEffects(myEffects, avSim, interaction1="friends", name="smoking")
myEffects <- includeEffects(myEffects, isolate, interaction1="friends", name="smoking")
myEffects <- includeEffects(myEffects, egoX, interaction1="smoking", name="friends")
myEffects <- includeEffects(myEffects, altX, interaction1="smoking", name="friends")
myEffects <- includeEffects(myEffects, simX, interaction1="smoking", name="friends")
myEffects <- includeEffects(myEffects, effFrom, interaction1='familyEvent', name="smoking")

# A benefit of modeling multiple behaviors is testing their effects on one another
# Does drinking behavior affect smoking behavior?
myEffects <- includeEffects(myEffects, effFrom, interaction1='drinking', name="smoking")
# Does smoking behavior affect drinking behavior?
myEffects <- includeEffects(myEffects, effFrom, interaction1='smoking', name="drinking")


# ====================
# (5) model estimation
# ====================

# Create an object with settings to tune the estimation algorithm:
# Using the same project name as above will append the results to that file
modelOptions <- sienaAlgorithmCreate(
	projname='SIENA drink smoke 180518', MaxDegree=c(friends=6),  
	doubleAveraging=0, diagonalize=.2, seed=786840)  # the seed is for the lab only

# Estimate the model, you can decrease estimation time by 
#   using more processors with the options commented out below
myResults <- siena07(modelOptions, data=myDataDrinkSmoke,
	effects= myEffects, batch=FALSE, verbose=TRUE
    )    # if multiple processors desired, comment out this line and run the next
#    , initC=T, useCluster=TRUE, nbrNodes=3) # replace "3" with your number of processors-1
myResults           # brief report of results

# If 'Overall maximum convergence ratio' is greater than .25 rerun the model
#   and use previous estimates as starting values (prevAns option)
# myAttempt2 <- siena07(modelOptions, data=myDataDrink, effects=myEffects, prevAns=myResults)
# To identify troublesome effects, look for high 'Convergence t-ratios' (>.1) 

# Note, no p-values are reported; dividing the estimate by its standard error gives
#   a t-ratio that is approximatley normally distributed
#  (t-ratios > 2 are significant at alpha=.05)
#  But we can also create a function to table this with conventional stars
outTable(myResults)

# Examine fuller report of results (shows score tests)
summary(myResults)  

# The score test indicates the simulated count of transitive reciprocated triplets is off
# (null hypothesis is that the simulated count = observed count)
# Let's add this effect to our effects object and rerun the model
myEffects <- includeEffects(myEffects, transRecTrip,name="friends")

myResults2 <- siena07(modelOptions, data=myDataDrinkSmoke,
	effects= myEffects, batch=FALSE, verbose=F,
	prevAns=myResults, returnDeps=T # specify starting values; return simulated nets (for GOF)
	)  
	
outTable(myResults2)  # examine results; good covergence; transRecTrip is significant

myRes <- myResults2   # for the following steps, assign final estimates to "myRes"
# This allows us to use the same script without changing the results object name each time


# ====================
# (6) goodness of fit
# ====================

# Calculate the fit with respect to the indegree distribution.
# By specifying "verbose=TRUE" we get information on the screen telling us
#   how far calculations have progressed.
# (You may see a note about "method with signature CsparseMatrix# (etc.)
#   which you can ignore.)

# indegree
( gof.id <- sienaGOF(myRes, verbose=TRUE, varName="friends", IndegreeDistribution,
     join=T, cumulative=F) )
     
plot(gof.id)  # looks great!

# outdegree
( gof.od <- sienaGOF(myRes, verbose=TRUE, varName="friends", OutdegreeDistribution,
     join=T, cumulative=F) )
plot(gof.od)  # good

# More GOF functions (OPTIONAL)
#   Running these requires the functions specified above
#   WARNING: some of these take a long time (may want to skip during lab)

# geodesic distances
( gof.gd <- sienaGOF(myRes, verbose=TRUE, varName="friends", GeodesicDistribution,
     join=T, cumulative=F) )
plot(gof.gd)
# On the low side. There are too many short distances, not enough separation into components

# triad census
(gof.tc <- sienaGOF(myRes, verbose=TRUE, varName="friends", TriadCensus, join=T) )
plot(gof.tc, scale=TRUE, center=TRUE)
# Really good fit

# eigenvector centralities
( gof.ev <- sienaGOF(myRes, verbose=TRUE, varName="friends", EigenvalueDistribution,
     join=T, cumulative=F) )
plot(gof.ev)
# Need fewer nodes with lowest eigenvalue centrality

# GOF for drinking
# goodness of fit overall drinking distribution
( gof.behaviourD <- sienaGOF(myRes,BehaviorDistribution,
	verbose=TRUE,join=TRUE,varName="drinking") )
plot(gof.behaviourD) # looks very good!

# two spatial autocorrelation coefficients
( gof.MoranGearyD <- sienaGOF(myRes,MoranGeary,
	verbose=TRUE,join=FALSE,varName=c("friends","drinking")) )
plot(gof.MoranGearyD,period=1) # acceptable
plot(gof.MoranGearyD,period=2) # acceptable

# Moran's I autocorrelation at a distance: 
( gof.MoranD <- sienaGOF(myRes,Moran123,
	verbose=TRUE,join=TRUE,varName=c("friends","drinking")) )
plot(gof.MoranD) # acceptable
# note that coefficients are summed across the two periods!

# same for Geary's c
( gof.GearyD <- sienaGOF(myRes,Geary123,
	verbose=TRUE,join=TRUE,varName=c("friends","drinking")) )
plot(gof.GearyD)	# acceptable
# note that coefficients are summed up over periods!

# ego-by-alter table for alcohol scores
( gof.EgoAlterTableD <- sienaGOF(myRes,EgoAlterTable,
	verbose=TRUE,join=TRUE,varName=c("friends","drinking")) )
plot(gof.EgoAlterTableD,center=TRUE,scale=TRUE)
# Good

# GOF for smoking
# goodness of fit overall smoking distribution
( gof.behaviourS <- sienaGOF(myRes,BehaviorDistribution,
                verbose=TRUE,join=TRUE,varName="smoking") )
plot(gof.behaviourS) # looks very good!

# two spatial autocorrelation coefficients
( gof.MoranGearyS <- sienaGOF(myRes,MoranGeary,
                verbose=TRUE,join=FALSE,varName=c("friends","smoking")) )
plot(gof.MoranGearyS,period=1) # acceptable
plot(gof.MoranGearyS,period=2) # acceptable

# Moran's I autocorrelation at a distance: 
( gof.MoranS <- sienaGOF(myRes,Moran123,
                verbose=TRUE,join=TRUE,varName=c("friends","smoking")) )
plot(gof.MoranS) # acceptable
# model predicts too much homophily among connected actors, but not enough at distance 3

# same for Geary's c
( gof.GearyS <- sienaGOF(myRes,Geary123,
                verbose=TRUE,join=TRUE,varName=c("friends","smoking")) )
plot(gof.GearyS)	# acceptable
# Geary's C leads to the opposite inference

# ego-by-alter table for alcohol scores
( gof.EgoAlterTableS <- sienaGOF(myRes,EgoAlterTable,
                verbose=TRUE,join=TRUE,varName=c("friends","smoking")) )
plot(gof.EgoAlterTableS,center=TRUE,scale=TRUE)	


# ======================
# (7) time heterogeneity
# ======================

# test whether estimates differ from wave 1 to 2 vs. wave 2 to 3
tt <- sienaTimeTest(myRes)
summary(tt)
# see "Joint significance test", null hypothesis is time homogeneity 
# p > .05 indicates no time heterogeneity


# ======================
# (8) Interpretation
# ======================

# Ego-Alter Selection Table
# Interpret the effects of drinking on friend selection
# First define a function that incorporates the relevant part
#   of the evaluation function, dependent on the parameters b1, b2, b3,
#   the overall average v_av, the similarity average sim_av,
#   and the range ran_v
obj_n <- function(vi, vj){
  b1*(vi-v_av) + b2*(vj-v_av) + b3*(1 - abs(vi-vj)/ran_v - sim_av)
  }

# Fill in the values of the parameter estimates and the averages (from the descriptive output)
v_av <- 3.113  # average drink 
sim_av <- 0.6744  # average similarity
ran_v <- 4   # range of values
b1 <- .0714  # drink ego
b2 <- -.0545  # drink alter
b3 <- .98  # drink similarity

vv <- c(1, 2, 3, 4, 5)  # Define the values of v for which the table is to be given
sel_tab <- outer(vv, vv, obj_n)  # calculate the table

round(sel_tab,3)  # display the table: rows are egos, columns are alters
# Cells indicate the contribution to the network function of
#   each type of dyad. rows are ego drinking (1-5); columns are alter drinking (1-5)

# Plot the predicted contribution to the network function
graphics.off()
levelplot(sel_tab,col.regions=colorRampPalette(c("white","yellow", "green", "blue")), 
	scales=list(col='white'), main = "Contribution to the Network Function",
	xlab = "Ego's Attribute Value", ylab = "Alter's Attribute Value" )

# Another plot (more approprite for continuous attributes)
filled.contour(x=vv, y=vv, z=sel_tab, zlim=c(min(sel_tab),max(sel_tab)), nlevels=50, axes=TRUE, 
	color.palette=colorRampPalette(c("white","yellow", "green", "blue")),
	main = "Contribution to the Network Function",
	xlab = "Ego Attribute Value", ylab = "Alter Attribute Value" )

# Peer Influence Table
# Define part of evaluation function
# 	Note: this will differ depending on measure of peer influence used!
obj_b <- function(vi, vj){
  b1*(vi-v_av) + b2*(vi-v_av)^2 + b3*(1 - abs(vi-vj)/ran_v - sim_av)
  }

# Fill in the values of the parameter estimates and the averages
v_av <- 3.113
sim_av <- 0.6983
b1 <- 0.42  # linear
b2 <- -0.02  # quad
b3 <- 4.68  # behavior: average similarity
vv <- c(1, 2, 3, 4, 5)

( beh_tab <- outer(vv, vv, obj_b) )  # calculate the table

# Plot the predicted contribution to the behavior function
levelplot(beh_tab,col.regions=colorRampPalette(c("white","yellow", "green", "blue")), 
	scales=list(col='white'), main = "Contribution to the Behavior Function",
	xlab = "Ego's Prospective Attribute Value", ylab = "Alter's Attribute Value" )

# Rows are alters' given value; columns are ego's prospective behavior
# When ego's alters have the behavior in a given row, 
#	  ego is drawn to the column with the highest positive value  


# Tangent: how do things look different with avAlt measure of influence?
#myEffects <- setEffect(myEffects,avSim,interaction1="friends",name="drinking", include=F)
#myEffects <- setEffect(myEffects,avAlt,interaction1="friends",name="drinking", include=T)
#myResults3 <- siena07(modelOptions, data=myDataDrinkSmoke, effects=myEffects)  

#obj_b2 <- function(vi, vj){b1*(vi-v_av) + b2*(vi-v_av)^2 + b3*((vi-v_av)*(vj-v_av))}
#v_av <- 3.113
#sim_av <- 0.6983
#b1 <- 1.297  # linear
#b2 <- -0.6397  # quad
#b3 <- 1.4135  # behavior: average alter
#vv <- c(1, 2, 3, 4, 5) 
#( sel_tab <- outer(vv, vv, obj_b2) ) # calculate the table  
  
# plot the predicted contribution to the behavior function
#levelplot(sel_tab,col.regions=colorRampPalette(c("white","yellow", "green", "blue")), 
#	scales=list(col='white'), main = "Contribution to the Behavior Function",
#	xlab = "Ego's Prospective Attribute Value", ylab = "Alter's Attribute Value" )


# ======================
# (9) Simulations
# ======================

#  Simulations to evaluate an intervention or manipulating model parameters
#  Let's adjust the strength of peer influence

# Reduce data to only two time points (1 & 3), which makes things simpler
friends2 <- sienaDependent(s50array[,,c(1,3)])
drinking2 <- sienaDependent(s50a[,c(1,3)],type="behavior")
smokingEx <- coCovar(s50s[,1])
myDataSim <- sienaDataCreate(friends2,drinking2,smokingEx)
myDataSim

simEffects <- getEffects(myDataSim)
simEffects <- includeEffects(simEffects,avSim,interaction1="friends2",name="drinking2")
simEffects <- includeEffects(simEffects,isolate,interaction1="friends2",name="drinking2")
simEffects <- includeEffects(simEffects,egoX,interaction1="drinking2",name="friends2")
simEffects <- includeEffects(simEffects,altX,interaction1="drinking2",name="friends2")
simEffects <- includeEffects(simEffects,simX,interaction1="drinking2",name="friends2")
simEffects <- includeEffects(simEffects, recip, transTrip,name="friends2")
simEffects <- includeEffects(simEffects, egoX, altX, sameX,interaction1='smokingEx', name="friends2")
simEffects <- includeEffects(simEffects, effFrom,interaction1='familyEvent', name="drinking2")
simEffects <- includeEffects(simEffects, transRecTrip,name="friends2")

# Fit model with new two-wave data specification
modelOptionsSim1 <- sienaAlgorithmCreate(projname='lab180518sim',
	nsub=5, MaxDegree=c(friends2=6),seed=786840)  # the seed is for the lab only
myResultsObs <- siena07(modelOptionsSim1, data=myDataSim,
	effects=simEffects, returnDeps=T) 
myResultsObs

# Set initial values for simulations based on estimated model 
simEffects$initialValue[simEffects$include==T] <- myResultsObs$theta
simEffects$fix[simEffects$include==T] <- TRUE
simEffects

# Manipulate strength of peer influence to be much larger
simEffectsHiPI <- simEffects
simEffectsHiPI <- setEffect(simEffectsHiPI, avSim, interaction1='friends2', name='drinking2',
    initialValue=12, fix=T)  
simEffectsHiPI

# Make peer influence much lower
simEffectsLoPI <- simEffects
simEffectsLoPI <- setEffect(simEffectsLoPI, avSim, interaction1='friends2', name='drinking2',
    initialValue=.5, fix=T)  
simEffectsLoPI

# Run the simulation
# 
nIter <- 50
modelOptionsSim2 <- sienaAlgorithmCreate(projname='lab180518sim', MaxDegree=c(friends2=6),
	nsub=0,     # no phase 2 iterations (because parameter values are fixed)
	n3=nIter,   # number of iterations (i.e., number of independent runs)
	simOnly=T,  # skips calculation of covariance matrix
	seed=4)     # the seed is for the lab only
myResultsSimHiPI <- siena07(modelOptionsSim2, data=myDataSim,
	effects=simEffectsHiPI, returnDeps=T, returnChains=T)
myResultsSimLoPI <- siena07(modelOptionsSim2, data=myDataSim,
	effects=simEffectsLoPI, returnDeps=T, returnChains=T)

# Extract mean drinking from simulation runs
#   Create vectors to store means
meanSimO <- rep(0,1000)      # means from sims using observed level of peer influence
meanSimHiPI <- rep(0,nIter)  # means for high peer influence condition
meanSimLoPI <- rep(0,nIter)  # means for low peer influence condition
#   Extract from simulations using estimated parameters
for (m in 1:1000) {
	meanSimO[m] <- mean( myResultsObs$sims[[m]][[1]]$drinking[[1]] )
	}
#   Extract from simulations that manipulate peer influence
for (m in 1:nIter) {
	meanSimHiPI[m] <- mean( myResultsSimHiPI$sims[[m]][[1]]$drinking2[[1]] )
	meanSimLoPI[m] <- mean( myResultsSimLoPI$sims[[m]][[1]]$drinking2[[1]] )
	}
meanSimHiPI
meanSimLoPI
# To see what we did, here is the mean of 50th simulation run: 3.14
mean( InitSim$sims[[50]][[1]]$drinking2[[1]] )

# Observed drinking at each wave, t1: 2.88, t2: 3.1, t3: 3.36
colMeans(s50a, na.rm=T)  
meanObs <- mean(s50a[,3])  # store observed mean drinking at time 3

# Plot the results
toPlot <- rbind(
	data.frame(cond="Observed",mean=meanSimO),
	data.frame(cond="High",mean=meanSimHiPI),
	data.frame(cond="Low",mean=meanSimLoPI) )
boxplot(mean ~ cond, data=toPlot, main="Mean Drinking", xlab="Value of Peer Influence",
	ylab="Mean Drinking")
abline(h=meanObs)

# Examine simulated drinking change in greater detail
#InitSim <- myResultsSimHiPI
#nstudents <- dim(s50a)[1]
#Zs5 <- array(NA, dim=c(nstudents,1))  # record each student's
#Zb5 <- array (0, dim=c(nIter,1))
#for (m in 1: nIter) {
#  for (n in 1:nstudents) {
#    Zs5[n,1] <- InitSim$sims[[m]][[1]]$drinking2[[1]][[n]]  
#    }
#  Zb5[m] <-colSums(Zs5, na.rm=T)/nstudents
#  }
#Zb5   # average drinking at end, for each iteration

# Plot mean simulated drinking over time
simName <- myResultsSimHiPI    # look at hi PI simulations first
chainNum <- 50
datChain <- t(matrix(unlist(simName$chain[[chainNum]][[1]][[1]]), 
    nc=length(simName$chain[[chainNum]][[1]][[1]]))) 
datBehavChain <- datChain[datChain[,2]=="1",]    # condense to behavior change opportunities
( nBehavChanges <- dim(datBehavChain)[1] )       # number of behavior change opportunities
meanSeqB <- nBehavChanges + 1    # only record behavior changes
meanSeqB[1] <- mean(s50a[,1])   # set t1 drinking mean to observed level
for (i in 1:nBehavChanges) {
	meanSeqB[i+1] <- meanSeqB[i] + (as.numeric(datBehavChain[i,6])/50)
	}
plot(x=1:length(meanSeqB), y=meanSeqB, type="l", ylab='drinking mean', xlab='time',
	main='mean drinking over time')	

# plot mean simulated drinking over time, with NETWORK change opportunities included (a better approach)
simName <- myResultsSimHiPI    # look at hi PI simulations first
chainNum <- 50
datChain <- t(matrix(unlist(simName$chain[[chainNum]][[1]][[1]]), 
      nc=length(simName$chain[[chainNum]][[1]][[1]]))) 
( nChanges <- dim(datChain)[1] )
meanSeqF <- nChanges + 1         # full sequence, whether behavior changes or not
meanSeqF[1] <- mean(s50a[,1])   # set t1 drinking mean to observed level
for (i in 1:nChanges) {
	if (datChain[i,2]=="0") { 
		meanSeqF[i+1] <- meanSeqF[i]
		}
	if (datChain[i,2]=="1") { 
		meanSeqF[i+1] <- meanSeqF[i] + (as.numeric(datChain[i,6])/50)
		}
	}
plot(x=1:length(meanSeqF), y=meanSeqF, type="l", 	ylab='drinking mean', xlab='micro step',
	main='mean drinking over time')	

# Compare the two approaches
par(mfrow=c(2,1))
plot(x=1:length(meanSeqB), y=meanSeqB, type="l", ylab='drinking mean', xlab='time',
     main='excluding network changes')	
plot(x=1:length(meanSeqF), y=meanSeqF, type="l", ylab='drinking mean', xlab='micro step',
     main='including network changes')	

# Extract mean drinking for all 50 chains, with network change opportunities included
simChanges <- rep(0, nIter)
for (i in 1: nIter) {simChanges[i] <- length(simName$chain[[i]][[1]][[1]]) }
maxChanges <- max(simChanges)

seqs <- matrix(meanSimHiPI, nr= nIter, nc=maxChanges+1)  # fill matrix with final mean
seqs[,1] <- mean(s50a[,1])  # set t1 drinking mean to observed level
for (i in 1: nIter) {
  datChain <- t(matrix(unlist(simName$chain[[i]][[1]][[1]]), nc=length(simName$chain[[i]][[1]][[1]]))) 
  for (j in 1: simChanges[i]) {
	if (datChain[j,2]=="0") { 
		seqs[i,j+1] <- seqs[i,j]
		}
	if (datChain[j,2]=="1") { 
		seqs[i,j+1] <- seqs[i,j] + (as.numeric(datChain[j,6])/50)
		}
	}
  }

# plot the first 25 iterations
dev.off()
plot(x=1:dim(seqs)[2], y=seqs[1,], ylim=c(min(seqs),max(seqs)),type="l",
	ylab='drinking mean', xlab='micro step',
	main='mean drinking over time')	
for (i in 1:25) { lines(x=1:dim(seqs)[2], y=seqs[i,], col=colors()[i*10]) }
# not very pretty, so let's try another approach

# plot one iteration using loess curve
micros <- 1:dim(seqs)[2]
lo1 <- loess(seqs[1,] ~ micros)
l1 <- predict(lo1, micros)
plot(x=1:dim(seqs)[2], y=seqs[1,], ylim=c(min(seqs),max(seqs)),type="l", 
	ylab='drinking mean', xlab='micro step',
	main='mean drinking over time')	
lines(x=micros, y=l1, col='blue')

# plot multiple iterations using loess curves
plot(x=1:dim(seqs)[2], y=seqs[1,], ylim=c(min(seqs),max(seqs)),type="l", 
	ylab='drinking mean', xlab='micro step', col='white',
	main='mean drinking over time')	
for (i in 1:25) {
	lines(x=micros, y=predict(loess(seqs[i,] ~ micros), micros), col=colors()[i*10])
	}


