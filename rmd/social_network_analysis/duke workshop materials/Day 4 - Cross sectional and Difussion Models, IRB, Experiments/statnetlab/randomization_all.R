#this program demonstrates some of the network randomization bits and 
#example models.

#Moody, 5.16.2018; 

#clear everything to start...
rm(list = ls())
gc()

#load basic data manipulation bits
library(dplyr); 
library(readr);
library(magrittr)     #Supports pipe (%>%) commands that allow you to perform multiple operations with one statement
library(tidyr)        #Additional functions for manipulating data
library(ggplot2)

#load the data - this is the same code as before
#first build the edgelist & nodelist info
setwd("C:/SNH18wd")
AHS_Base <- read_csv ('ahs_wpvar.csv',
                      col_names = TRUE);
AHS_adjlist <- AHS_Base %>%
  select(ego_nid, mfnid_1:mfnid_5, ffnid_1:ffnid_5, grade, sex, PSMOKES,commcnt) %>%
  filter(commcnt==1);

AHS_Edges <- AHS_adjlist %>%
  rename( id = `ego_nid`,
          gender = `sex`) %>%
  gather(Alter_Label, Target, mfnid_1:mfnid_5, ffnid_1:ffnid_5, na.rm = TRUE)

AHS_Edges=AHS_Edges %>% filter (Target != 99999);
AHS_Edges=AHS_Edges %>%select(id, Target);

#install.packages("statnet")
library(statnet)
g=as.network(AHS_Edges)
g %v% "grade" <- AHS_adjlist$grade
g %v% "sex" <- AHS_adjlist$sex
g %v% "smokes" <- AHS_adjlist$PSMOKES

# get degree scores
outdegree <- degree(g, cmode = "outdegree")
indegree<- degree(g, cmode = "indegree")
g %v% "indegree" <- indegree
g %v% "outdegree" <- outdegree
g %v% "degree" <- degree(g)
plot(g, vertex.col="grade")
plot(g, vertex.col="sex")

#how transitive is our network?
obs_tran=gtrans(g)
obs_tran

#distribution of dyads
dyads=dyad.census(g)
dyads

#to simulate...first initialize an empty vector
simsize=500;
ran_tran=vector(length=simsize)

#loop for the simulation...
j=1
while(j<(simsize+1)){
  r=rguman(1,nv=71,mut=dyads[1],asym=dyads[2],null=dyads[3])
  ran_tran[j]=gtrans(r)
  if (is.na(ran_tran[j])) next else j=j+1  #skip bad sim cases
}

dt=as.data.frame(ran_tran)
ggplot(dt,aes(x=ran_tran))+geom_histogram()+
  geom_vline(xintercept=obs_tran,
             linetype=1,
             color="red")


#perhaps the clustering observed is due to general categorical mixing, 
#by grade or sex?
grades=as.character(g %v% "grade")
grdmix=mixingmatrix(g, "grade")
grdmix


#should be able to do this with the RGNMIX function, but that's 
#not working, so let's do the same thing by hand. 
#here's my failed code for the build in function, 
################
#to simulate...first initialize an empty vector
#simsize=500;
#ran_tran=vector(length=simsize)

#loop for the simulation...
#j=1
#while(j<(simsize+1)){
#  ran_g<-rgnmix(1,grades,grdmix$matrix,method="exact",return.as.edgelist=TRUE)
#  ran_tran[j]=gtrans(as.network(ran_g))
#  j=j+1
#  if (is.na(ran_tran[j])) next else j=j+1  #skip bad sim cases
#}

#dt=as.data.frame(ran_tran)
#ggplot(dt,aes(x=ran_tran))+geom_histogram()+
#  geom_vline(xintercept=obs_tran,
#             linetype=1,
#             color="red")
#now do it by hand.  This code steals directly from some stuff
#jake fisher put together in 2017 session.



#1. get a probability matrix from the mixing matrix...need a denominator
c=as.matrix(table(grades))
cells=c%*%t(c)
mixprob=grdmix$matrix/cells
mixprob
#note this is technically not quite right...as we should subtractk
#1 from the diagonal...but I'm being lazy...

# 2. Now create an empty matrix:
prob.matrix <- matrix(NA, ncol = network.size(g),
                      nrow = network.size(g))

# 3. ...fill in the values based on the mixing matrix.  Going to 
#so get the categories...
grade.values <- sort(unique(grades))

# The general idea is to do this...
#prob.matrix[which(grades == 7), which(grades == 7)] <- tie.probs[1, 1]

# ...but that's going to require 36 lines of code.  Let's write a loop instead.
for (i in 1:nrow(mixprob)) {
  for (j in 1:ncol(mixprob)) {
    prob.matrix[which(grades == grade.values[i]), 
                which(grades == grade.values[j])] <- mixprob[i, j]
  }
}

#now we have a matrix where each ij cell is the probabilty of a 
#tie based on mixing...so just do random bernulli draws from that, 
#using the same code strucure as above...

simsize=500;
ran_tran=vector(length=simsize)

#loop for the simulation...
j=1
while(j<(simsize+1)){
  r=rgraph(n = network.size(g), tprob = prob.matrix)
  ran_tran[j]=gtrans(r)
  if (is.na(ran_tran[j])) next else j=j+1  #skip bad sim cases
}

dt=as.data.frame(ran_tran)
ggplot(dt,aes(x=ran_tran))+geom_histogram()+
  geom_vline(xintercept=obs_tran,
             linetype=1,
             color="red")

#could do the same thing by Sex, but the plot we had suggested it's 
#not strongly clustered by sex...but you get the idea...
#also, would be more efficient to create a sample of graphs, store them
#then you can test a bunch of stats agains the same set...but transitivyt
#is cheap to calculate...so using that...

plot(hist(ran_tran),xlim = c(0, .5))
abline(v = obs_tran, col = "red")

# Now we pass both of those to the function in igraph
#one quick test...
r=igraph::sample_degseq(out.deg = outdegree, in.deg = indegree,method = "simple.no.multiple")
igraph::transitivity(r)
igraph::plot.igraph(r)

#same loop bit...could just write a function for this...
simsize=50;
ran_tran=vector(length=simsize)

#loop for the simulation...note I use the igraph transitivity function
j=1
while(j<(simsize+1)){
  r=igraph::sample_degseq(out.deg = outdegree, in.deg = indegree,method = "simple.no.multiple")
  ran_tran[j]=igraph::transitivity(r)
  if (is.na(ran_tran[j])) next else j=j+1  #skip bad sim cases
}

dt=as.data.frame(ran_tran)
ggplot(dt,aes(x=ran_tran))+geom_histogram()+
  geom_vline(xintercept=obs_tran,
             linetype=1,
             color="red")

#again...not even close.  But, all this is binary; let's build a model
#to look at this with multiple conditionals.

####################
# ERGM 
####################

#ergm code adapted from Jeff Smith's tutorial.
#clean house...
rm(c,cells, dt, dyads, grdmix,prob.matrix,r)
rm(grade.values,grades,i,j,mixprob,obs_tran,rant_tran,simsize)

#super simple model.. 
mod.rand<-ergm(g~edges)  
summary(mod.rand)  

#interpretation of model coefficients: 
#the log-odds of any tie existing is -2.7275
#probability: exp(-2.7275)/(1+exp(-2.7275))=.0613
exp(-2.7275)/(1+exp(-2.7275))
#which compares to density as..
gden(g)

#How good is the model?
plot(gof(mod.rand))

#add some covariates...
mod.homoph<-ergm(g~edges+nodematch("sex")+nodematch("grade")
                 +nodematch('smokes'))  
summary(mod.homoph)
mod.gof=gof(mod.homoph)
plot(mod.gof)



#add some simple volume bitscovariates...
mod.p1<-ergm(g~edges+nodematch("sex")+nodematch("grade")
             +nodematch('smokes')+sender+receiver)  
summary(mod.p1)
mod.gof=gof(mod.p1)
plot(mod.gof)
#model predicts better, but huge BIC cost!  Simplify?



g %v% "indegree" <- indegree
g %v% "outdegree" <- outdegree

mod.p2<-ergm(g~edges+nodematch("sex")+nodematch("grade")
             +nodematch('smokes')+nodeicov("indegree")+nodeocov("outdegree")
             +mutual)
summary(mod.p2)
mod.gof=gof(mod.p2)
plot(mod.gof)

mod.gendmut<-ergm(g~edges+nodematch("sex")+nodematch("grade")
                  +nodematch('smokes')+nodeicov("indegree")+nodeocov("outdegree")
                  +mutual(same="sex",diff=TRUE))
summary(mod.gendmut)


#let's try a transitivity term...
# mod.tran<-ergm(g~edges+nodematch("sex")+nodematch("grade")
#                   +nodematch('smokes')+nodeicov("indegree")+nodeocov("outdegree")
#                                +mutual+triangle)
# summary(mod.gendmut)

#turns out that won't run...lets cheat!
mod.tran<-ergm(g~edges+nodematch("sex")+nodematch("grade")
               +nodematch('smokes')+nodeicov("indegree")+nodeocov("outdegree")
               +mutual+triangle, control=control.ergm(MCMLE.maxit=2))
summary(mod.tran)
mcmc.diagnostics(mod.tran)
test=simulate.ergm(mod.tran,1)
plot(test)


mod.gwesp<-ergm(g~edges+nodematch("sex")+nodematch("grade")
                +nodematch('smokes')+nodeicov("indegree")+nodeocov("outdegree")
                +mutual+gwesp)
summary(mod.gwesp)
mcmc.diagnostics(mod.gwesp)
test=simulate.ergm(mod.gwesp,1)
plot(test)
plot(gof(mod.gwesp))

################################################################
#latent space models.  These models allow us to fit a network without 
#having to specify the actual tie formation patterns...
#code cribbed from Jake Fisher
#################################################################

# The most user-friendly latent space model software is in the latentnet package
# (more recent models are provided by the amen package).
#install.packages("latentnet")

library(latentnet)
start.time <- Sys.time()
latent.fit <- ergmm(g ~ euclidean(d = 2))
runtime=Sys.time()-start.time;
runtime;

summary(latent.fit)
plot(latent.fit)
mcmc.diagnostics(latent.fit)
plot(gof(latent.fit))

# Can add additional dimensions...
start.time <- Sys.time()
latent.fit <- ergmm(g ~ euclidean(d = 3))
runtime=Sys.time()-start.time;
runtime;
plot(gof(latent.fit))

# ... latent groups ...
start.time <- Sys.time()
latent.fit <- ergmm(g ~ euclidean(d = 2, G = 5))
runtime=Sys.time()-start.time;
runtime;
plot(latent.fit)
plot(gof(latent.fit))

# ... or homophily effects
start.time <- Sys.time()
latent.fit <- ergmm(g ~ nodematch("grade") + nodematch("sex")+euclidean(d = 2))
runtime=Sys.time()-start.time;
runtime
summary(latent.fit)






