### Peer Influence & Diffusion lab 
### Instructor: Jaemin Lee, Duke University
### Resources: Statnet Team, Cheng-Jun Wang, Katya Ognyanova, Labs from James Moody & Jake Fisher, and my own
### Overview
# 1. SI(Susceptible-Infected) model
# 2. SIS Endemic using EpiModel
# 3. Peer influence model for opinions
# 4. Network autocorrelation regression
# 5. QAP regression

####################################################################################
### Part 1. STATIC NETWORKS - A Simple SI Simulation on a Toy Graph from Scratch####
####################################################################################
library(igraph)

# Make a toy tree-like network with 50 nodes
size=50 
g = graph.tree(size, children = 2, mode="undirected")
plot(g)

# Initialize the diffusers
seeds_num = 1 # Let's begin with 1 seed for simplicity
set.seed(2016); diffusers = sample(1:vcount(g),seeds_num) # Randomly choose the seed
diffusers # Who is it?
g[[diffusers,]] # Who's connected to it?
nearest_neighbors<-unlist(g[[diffusers,]])

p=0.4 # let's say dyadic transmission probability is .4
sample(c(1,0),length(nearest_neighbors),replace=T, prob=c(p,1-p)) # Draw 1 or 0 based on p
is.infected<-sample(c(1,0),length(nearest_neighbors),replace=T, prob=c(p,1-p))
is.infected
new.infected<-nearest_neighbors[which(is.infected==1)] # Get IDs for newly infected nodes among neighbors
diffusers<-c(diffusers,new.infected) # Update diffusers
diffusers
# Visualize it
V(g)$color = "white"
V(g)$color[V(g)%in%diffusers] = "red"
plot(g)

## Repeat this procedure by making a loop

# function for updating the spreaders
update_diffusers = function(diffusers){
  nearest_neighbors = unlist(ego(g, 1, diffusers))
  nearest_neighbors = setdiff(nearest_neighbors, diffusers)
  n=length(nearest_neighbors)
  is.infected=sample(c(1,0),n,replace=T, prob=c(p,1-p))
  new_infected = nearest_neighbors[which(is.infected==1)]
  diffusers = unique(c(diffusers, new_infected))
  return(diffusers)
}

infected=list() # Initialize a list to record who's infected
infected[[1]] = sample(1:vcount(g),seeds_num) # First seed


# Start the contagion processes

i = 1
while(length(infected[[i]]) < size){ 
  infected[[i+1]] = update_diffusers(infected[[i]]) %>% sort()
  print(paste("Tick:",i,"Infected:",length(infected[[i]])))
  i = i + 1
}

## Visualizations
# Diffusion curve
num_cum = lapply(1:i, function(x) length(infected[[x]])) %>% unlist()
p_cum = num_cum/max(num_cum)
time = 1:i
plot(main="Proportion infected",p_cum~time, type = "b") # type "b" -- plotting Both lines and points


# Network visualization

l<-layout.fruchterman.reingold(g) # to fix coordinates of nodes

# Snapshot 
par(mfrow=c(2,2), mar=c(1,1,1,1)) # combine multiple plots
plot(g, main="Day 1", vertex.color=ifelse(V(g)%in%infected[[1]],"tomato","cadetblue3"), layout=l, vertex.label=NA)
plot(g, main="Day 8", vertex.color=ifelse(V(g)%in%infected[[8]],"tomato","cadetblue3"), layout=l, vertex.label=NA)
plot(g, main="Day 16", vertex.color=ifelse(V(g)%in%infected[[16]],"tomato","cadetblue3"), layout=l, vertex.label=NA)
plot(g, main=paste("Day",i), vertex.color=ifelse(V(g)%in%infected[[i]],"tomato","cadetblue3"), layout=l, vertex.label=NA)

dev.off() # clear plot deck..

## Let's go dynamic with a GIF animation

## ..Turns out that some computers have trouble in executing the following lines 
## that use R package and its functions to combine images and create a GIF animation.
## Try as you like by uncommenting them. Instead, we will try an alternative way for today.

# install.packages("animation")
# library(animation)
# 
# saveGIF({
#   # start the plot
#   m = 1
#   while(m <= length(infected)){
#     V(g)$color = "cadetblue3"
#     V(g)$color[V(g)%in%infected[[m]]] = "tomato"
#     day<-paste("Day",m)
#     plot(g, edge.color="gray85", vertex.frame.color="white", vertex.size=10, edge.width=3, layout =l)
#     title(day, cex.main=2.5, col.main="dodgerblue4")
#     m = m + 1}
# },
# interval = .8, ani.width = 640, ani.height = 640, movie.name="endemic_animation.gif")


## The alternative is to save each image slice to a file then combine them in a freely available web-based software
m = 1
while(m <= length(infected)){ # Repeat making plots
  V(g)$color = "cadetblue3"
  V(g)$color[V(g)%in%infected[[m]]] = "tomato"
  day<-paste("Day",m)
  plot(g, edge.color="gray85", vertex.frame.color="white", vertex.size=10, edge.width=3, layout =l)
  title(day, cex.main=2.5, col.main="dodgerblue4")
  dev.copy(png,filename=paste("day",m,".png"))  # This function saves a plot to your working directory
  dev.off()
  m = m + 1
}
## Then you can upload your slices to, for example, https://giphy.com/create/gifmaker to make a combined gif pic.

##################################################
### PART 2: DYNAMIC NETWORKS - EpiModel (S-I-S)###
##################################################

# Model information: http://www.epimodel.org/
# Tutorial: http://statnet.github.io/sb/t1.html
install.packages("EpiModel",dependencies = TRUE)
install.packages("ndtv")
library("EpiModel")
library("ndtv")

# SIS Epidemic: Susceptible-Infected-Susceptible (SIS) epidemic in a closed population.
# Example: a bacterial sexually transmitted infection such as Gonorrhea.
# Persons infected from sexual contact with an infected partner
# & recovery from infection either through natural clearance or through antibiotic treatment.

# What EpiModel does: Network-based epidemic model
# - Estimate networks using ERGMS with "target statistics"
# - Continue with dynamic network changes (i.e. romantic relationship)
# - epidemic processes on that network

## 1. Network model estimation
N=50 # a network of 50 homogeneous nodes, with no edges between them at the start
nw <- network.initialize(n = N, directed = FALSE)

formation <- ~edges+concurrent # Formula: Our sexual networks will be formed by two parameters: how dense / how frequent people cheat on
mean.edge<-1 # mean edge = 1 in our population
mean.con<-.22 # 22% concurrency = overlapping sexual partnerships
target.stats<-c(mean.edge*(N/2),mean.con*N) # Target statistics based on our specification

# Another parameter: duration. Homogeneous probability of dissolution -- partnerships last avg. 10 time steps
coef.diss <- dissolution_coefs(dissolution = ~offset(edges), duration = 10) 

est <- netest(nw, formation, target.stats, coef.diss, verbose = FALSE) # Network model estimation based on edge, concurrency, duration parameters.

## 2. Epidemic simulation
## Epidemeic model parameters
# 1. Infection probability = the risk of transmission given contact with an infected person. 
# 2. Act rate = the number of sexual acts between that occur within a partnership each time unit
# Overall frequency of acts per person per unit time is a function of the incidence rate of partnerships 
# and this act parameter.
# 3. Recovery rate = the speed at which infected persons become susceptible again. 
param <- param.net(inf.prob = 0.8, act.rate = 2, rec.rate=0.01)
init <- init.net(i.num = 2) # seed
control <- control.net(type = "SIS", nsteps = 30, nsims = 1, verbose = FALSE)
sim <- netsim(est, param, init, control)
plot(sim) # See the diffusion curve

# Dynamic network visualization
nw <- get_network(sim)
nw <- color_tea(nw, verbose = FALSE)

slice.par <- list(start = 1, end = 30, interval = 1, 
                  aggregate.dur = 1, rule = "any")
render.par <- list(tween.frames = 10, show.time = FALSE)
plot.par <- list(mar = c(0, 0, 0, 0))

compute.animation(nw, slice.par = slice.par)

render.d3movie(
  nw, 
  render.par = render.par, 
  plot.par = plot.par,
  vertex.cex = 1.5, 
  vertex.col = "ndtvcol", 
  edge.col = "darkgrey", 
  vertex.border = "lightgrey",
  displaylabels = FALSE,
  #filename = paste0(getwd(), "/movie.html")
)

## 3. How much does concurrency matter?
# We conduct simulations, varying concurrency by 3 levels
N=500
nw <- network.initialize(n = N, directed = FALSE)
formation <- ~edges
mean.edge<-0.7
target.stats<-mean.edge*(N/2)
coef.diss <- dissolution_coefs(dissolution = ~offset(edges), duration = 100) 
est <- netest(nw, formation, target.stats, coef.diss, verbose = FALSE)
param <- param.net(inf.prob = 0.5, act.rate = 2, rec.rate=0.01)
init <- init.net(i.num = 10)
control <- control.net(type = "SIS", nsteps = 500, nsims = 5, verbose = FALSE)
sim <- netsim(est, param, init, control)
#plot(sim)

formation.concur <- ~edges+concurrent
mean.con<-.15
target.stats<-c(mean.edge*(N/2),mean.con*N)
est <- netest(nw, formation.concur, target.stats, coef.diss, verbose = FALSE)
sim.concur.1 <- netsim(est, param, init, control)
#plot(sim.concur.1)

mean.con<-.20
target.stats<-c(mean.edge*(N/2),mean.con*N)
est <- netest(nw, formation.concur, target.stats, coef.diss, verbose = FALSE)
sim.concur.2 <- netsim(est, param, init, control)
#plot(sim.concur.2)

par(mfrow=c(1,3))
plot(sim)
plot(sim.concur.1)
plot(sim.concur.2)

####################################################
### PART 3: Peer Influence and Opinion Dyanamics ###
####################################################
library(igraph)
library(ggplot2)

op<-matrix(rnorm(100*2,0,2),100,2) # Initialize random opinions on two issues, mean=0 s.d=2
plot(op)

## Generate a network
graph<- sample_smallworld(dim=2, size=10, nei=1, p=0.1) # Watts & Strogatz small-world in this case (N=100)
# For erdos-renyi graph..
# graph <- erdos.renyi.game(100, 0.2,directed = FALSE, loops = FALSE)
plot(graph, vertex.size=6, vertex.label=NA, layout=layout_in_circle)
plot(graph, vertex.label=NA)

graph.adj<-get.adjacency(graph) %>% as.matrix # Need an adjacency mat

## Construct weight matrix
wgtmat<-apply(graph.adj,1,function(x) x/sum(x)) # Normalized by row sums

## A Function for basic Friedkin model
FRIEDKIN<-function(a,wmat,y,t){ 
  # a = alpha (peer influence coefficient)
  # wmat = weight matrix (just connected alters each with equal weights in this case)
  # y = initial opinion
  # t = no. iteration
  Y<-list(y)
  for(i in 1:t){
    Y[[i+1]]<-a*(wgtmat%*%Y[[i]])+(1-a)*y
    # My opinion at t+1 is a function of my initial opinion (t=1) and connected peers' opinions (t)
  }
  return(Y)
}

# See how alpha changes opinion dynamics
pim.high<-FRIEDKIN(0.8,wgtmat,op,10)
pim.mid<-FRIEDKIN(0.5,wgtmat,op,10)
pim.low<-FRIEDKIN(0.3,wgtmat,op,10)

plot(pim.high[[1]]); plot(pim.high[[2]])

# Store mean distance of to
results.df<-rbind(
  cbind(0.8,sapply(pim.high, function(x)mean(dist(x)))),
  cbind(0.5,sapply(pim.mid,function(x)mean(dist(x)))),
  cbind(0.3,sapply(pim.low,function(x)mean(dist(x))))
) %>% as.data.frame
results.df$t<-rep(1:11,3)

ggplot(results.df,aes(t,V2,colour=factor(V1)))+geom_line()+geom_point()+ylab("Mean Op. Distance")+ labs(colour='Alpha') 


#############################################
### PART 4: Network Autocorrelation Model ###
#############################################

# Probably should install this package if you haven't already:
install.packges("numDeriv")
# load R packages 
library(statnet)
library(network)
library(sna)
library(numDeriv)
# Load Add Health datasets
load(url("http://www.soc.duke.edu/~jl489/s_edge.Rda")) #Edge-level data
load(url("http://www.soc.duke.edu/~jl489/s_nodes.Rda")) #Node-level data
load(url("http://www.soc.duke.edu/~jl489/s_dycov.Rda")) #Dyad covariate data

#create a network object from the Node names of edgelist
s_friends <- network(s_edge[,c("N1" ,"N2" )])
#Now create the node attributes 
names(s_nodes)
s_friends %v% "FEMALE" <- s_nodes$FEMALE
s_friends %v% "NID" <- s_nodes$NID
s_friends %v% "GRADE" <- s_nodes$GRADE
s_friends %v% "FIGHT" <- s_nodes$FIGHT #Physical fight frequency 
s_friends %v% "WHITE" <- s_nodes$WHITE
list.vertex.attributes(s_friends)

#Now assign edge attributes
head(s_edge)
set.edge.attribute(s_friends,"n1" ,s_edge[,1])
set.edge.attribute(s_friends,"n2" ,s_edge[,2])
set.edge.attribute(s_friends,"rnorm" ,s_edge[,3]) #normalized edge values

#Now assign dyad values 
s_clubs <- network(s_dycov[,c("N1" ,"N2" )])
set.edge.attribute(s_clubs,"n1" ,s_dycov[,1])
set.edge.attribute(s_clubs,"n2" ,s_dycov[,2])
set.edge.attribute(s_clubs,"ovlpec" ,s_dycov[,3])

#select the node-level covariates to model
x<-as.matrix(s_nodes[c("FEMALE","WHITE","GRADE")]) 
summary(x)
#create the weight matrix from the network object
#here i created a row-normalized version of the adj. matrix to use as W
w1<-as.sociomatrix(s_friends,attrname="rnorm")
fights<-as.matrix(s_nodes["FIGHTS"])
summary(fights)
class(fights)
class(x)
class(w1)
#simple model w. row-weighted peer effects
pim1<-lnam(fights,x,w1)
summary(pim1)
#add a second net effect on the disturbances, here from club overlap
clbs<-as.sociomatrix(s_clubs,attrname="ovlpec")
pim2<-lnam(fights,x,w1,clbs)
summary(pim2)

##########################
### PART 5: QAP Model ####
##########################
## Quadratic Assignment Procedure (QAP)


# Permutation tests - If we randomly shuffle
# the rows and columns of the adjacency matrix, how often would a correlation 
# as large as the one we observed occur?

# This is the idea behind the Quadratic Assignment Procedure (QAP) used to test
# the significance of correlations between networks. 

## FIRST EXAMPLE:
# We'll use the Padgett florentine marriage & business ties dataset:

load(url("http://www.soc.duke.edu/~jl489/flo.Rda"))

# The data contains two network objects - one with marital and another one
# with business relations between Florentine families.

flobusiness; flomarriage 


# Compute the network correlation:

gcor(flomarriage,flobusiness)

# Test the correlation using qaptest:
# qaptest(A.List.Of.Networks, Some.Network.Function, reps=100)
# where reps is the number of graphs to be generated.

# Out of a 100 permutations, how many had >= or <= correlation than the observed value?
# (the parameters g1=1 g2=2 tell the test to compare the first & second element of
# the list of networks that is the first argument of qaptest)

flo.qap <- qaptest(list(flomarriage,flobusiness), gcor, g1=1, g2=2, reps=100)
flo.qap
plot(flo.qap)


#=============================================================#

## SECOND EXAMPLE:
# correlation between smoking behavior and friendships in Add Health data

load(url("http://www.soc.duke.edu/~jl489/add_health_cmty1.Rdata"))
add.health

# To begin, we construct a square matrix where the (i, j) cell is a dummy
# variable indicating whether i and j have the same smoking behavior (i.e.,
# they both smoke, or the they both don't smoke.)  To construct that, we use the
# outer command (like an outer product)
smoking <- outer(
  add.health %v% "PSMOKES",
  add.health %v% "PSMOKES",
  FUN = "=="
)
dim(smoking)

# We compare that with the matrix of friendships:
friends <- as.matrix(add.health)
head(friends)

# The command for QAP tests is netlm or netlogit (depending on whether you use
# a linear or logistic regression).  Here we use logistic:
(qap.out <- netlogit(y = smoking, x = friends, nullhyp = "qap", reps = 100))

# You can add additional covariates by entering them as an array.  Let's
# add same-grade:
grade <- outer(
  add.health %v% "grade",
  add.health %v% "grade",
  FUN = "=="
)

# Make a 3 dimensional array -- the abind command from the abind package also
# does this.
X <- array(, dim = c(2, nrow(grade), ncol(grade)))
X[1, , ] <- friends
X[2, , ] <- grade

(qap2 <- netlogit(y = smoking, x = X, nullhyp = "qap", reps = 100))

