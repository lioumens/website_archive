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
#load the data
#first build the edgelist & nodelist info
setwd("C:/jwm/SNH18wd")
AHS_Base <- read_csv ('ahs_wpvar.csv',
                      col_names = TRUE);
AHS_adjlist <- AHS_Base %>%
  select(ego_nid, mfnid_1:mfnid_5, ffnid_1:ffnid_5, grade, sex, commcnt) %>%
  filter(commcnt==1);

AHS_Edges <- AHS_adjlist %>%
  rename( id = `ego_nid`,
          gender = `sex`) %>%
  gather(Alter_Label, Target, mfnid_1:mfnid_5, ffnid_1:ffnid_5, na.rm = TRUE)

AHS_Edges=AHS_Edges %>% filter (Target != 99999);
AHS_Edges=AHS_Edges %>%select(id, Target);

library("statnet")
g=as.network(AHS_Edges)
g %v% "grade" <- AHS_adjlist$grade
g %v% "sex" <- AHS_adjlist$sex
g %v% "degree" <- degree(g)


#Lets just do some randomization checks...compare our observed to a real.
tri=triad.census(g) #The full triad census as an 16-element vector 

#how transitive is our network?
obs_tran=gtrans(g)

#distribution of dyads
dyads=dyad.census(g)
dyads

#to simulate...first initialize an empty vector
ran_tran=vector(length=500)

j=1
while(j<501){
  r=rguman(1,nv=71,mut=dyads[1],asym=dyads[2],null=dyads[3])
  ran_tran[j]=gtrans(r)
  if (is.na(ran_tran[j])) next else j=j+1  #skip bad sim cases
  }

dt=as.data.frame(ran_tran)

ggplot(dt,aes(x=ran_tran))+geom_histogram()+
    geom_vline(xintercept=obs_tran,
           linetype=1,
           color="red")

#Draw a highly reciprocal network
g<-rguman(1,15,mut=0.25,asym=0.05,null=0.7)

#Test transitivity against size, density, and the dyad census
cug.test(g,gtrans,cmode="size")
cug.test(g,gtrans,cmode="edges")
cug.test(g,gtrans,cmode="dyad.census")



summarise(dt,mean(r.p.t))

p=length(r.p.t[r.p.t>=Pt])/1000

p

r.triad=triad.census(r)