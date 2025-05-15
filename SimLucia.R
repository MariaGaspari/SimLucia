#
## Modeller: Maria Gaspari
### Latest Update: May 2025

#### Description: SimLucia is a spatial microsimulation model for the development of residential building exposure including key structural and economic dimensions of vulnerability. 
##### A demo of the SimLucia model with "some" microdata is presented here to make the user familiar with the methodology.

##### Method: Iterative Proportional Fitting Spatial Microsimulation 


# Packages required
install.packages("ipfp")
install.packages("tityverse")
install.packages("plyr")
install.packages("ggpubr")
# Load libraries
library(plyr)
library(dplyr)
library(ggpubr)
library(ipfp)
library(ggplot2)
library(gridExtra)

# Set the directory to the folder this code belongs to
setwd("/Users/mariagaspari/Desktop/UCL_Thesis/SimLucia/GIT")

#Load Household MICROdata (100 x 6) |  Anonymised household-level data
indD<-read.table("MicrodataSample.csv",header=TRUE,sep = ",", strip.white = TRUE)  
sapply(indD,class)                      # check the classes of the variables
#
#
#Load CENSUS aggregate Data (12 x 29) | Aggregate data from the 2010 national population and housing census in Saint Lucia
consV<-read.table("Census_HHLD.csv",header=TRUE,sep = ",", strip.white = TRUE)
consV<-consV[,c(2:29)]
sapply(consV,class)  

N_ind<-nrow(indD)      #number of individuals/households (100)
N_zones<-nrow(consV)   #number of zones  (12)
N_cons<-ncol(consV)    #number of categories (subconstraints)  (28)


# Check totals for each constraint
sum(consV[,1:8])                              # Number of Households (Census)
rowSums(consV[,1:8])                          # 1.Ownership of Dwelling 
rowSums(consV[,1:8])-rowSums(consV[,9:14])    # 2.Land Ownership
rowSums(consV[,1:8])-rowSums(consV[,15:21])   # 3.External Wall Material
rowSums(consV[,1:8])-rowSums(consV[,22:28])   # 4.Number of Rooms


# For EMPTY CELL VALUE == 0, set to 0.00001   (internal validation check)
consV [consV== 0]  <- 0.00001 # checked, OK
#
##
#Flatten the individual level data set (i.e., increase its width so each column becomes a category containing 1:presence or 0:absence)
             #  boolean matrix with 100 x 28 = 2,800 elements

IndCat<-matrix(0,nrow=N_ind, ncol=N_cons)
#
# Constraint 1| h1_5 (ownership of dwelling)
IndCat[which(indD$h1_5==2),1]<-1      # owned (without mortgage)               
IndCat[which(indD$h1_5==1),2]<-1      # owned (with mortgage)             
IndCat[which(indD$h1_5==4),3]<-1      # rented-unfurnished
IndCat[which(indD$h1_5==3),4]<-1      # rented-furnished
IndCat[which(indD$h1_5==6),5]<-1      # provided rent-free
IndCat[which(indD$h1_5==5),6]<-1      # leased to own
IndCat[which(indD$h1_5==7),7]<-1      # squatted
IndCat[which(indD$h1_5==88),8]<-1     # other
#
# Constraint 2| h1_4 (land ownership)
IndCat[which(indD$h1_4==1),9]<-1       # owned with title     
IndCat[which(indD$h1_4==2),9]<-1       # family owned    
IndCat[which(indD$h1_4==3),11]<-1      # rents the land
IndCat[which(indD$h1_4==4),10]<-1      # leases the land
IndCat[which(indD$h1_4==5),13]<-1      # squatting
IndCat[which(indD$h1_4==6),12]<-1      # not owned
IndCat[which(indD$h1_4==88),14]<-1     # other / don't know
#
# Constraint 3| h1_2 (external wall material)
IndCat[which(indD$h1_2==1),15]<-1       # wood/timber               
IndCat[which(indD$h1_2==2),16]<-1       # concrete/concrete blocks
IndCat[which(indD$h1_2==3),17]<-1       # wood & concrete
IndCat[which(indD$h1_2==4),18]<-1       # brick/blocks 
IndCat[which(indD$h1_2==5),18]<-1       # stone (zero in the sample)
IndCat[which(indD$h1_2==6),19]<-1       # plywood
IndCat[which(indD$h1_2==7),20]<-1       # makeshift
IndCat[which(indD$h1_2==88),21]<-1      # other/ don't know
#
# Constraint 4| h1_14 (number of rooms)
IndCat[which(indD$h1_14==1),22]<-1            # 1 rooms
IndCat[which(indD$h1_14==2),23]<-1            # 2 rooms
IndCat[which(indD$h1_14==3),24]<-1            # 3 rooms
IndCat[which(indD$h1_14==4),25]<-1            # 4 rooms
IndCat[which(indD$h1_14==5),26]<-1            # 5 rooms
IndCat[which(indD$h1_14==6),27]<-1            # 6 rooms
IndCat[which(indD$h1_14>6),28]<-1             # 7 to 14 rooms
#
colnames(IndCat)<-colnames(consV)                         # match individual and aggregate level data names
length(which(IndCat!=1))+sum(IndCat)==28*100              # check
colSums(IndCat)                                         
IndCat                                                    # view the aggregate version of ind

## Create weights - individual HHLDs x zones (100x12)
weights0 <- matrix(data = 1, nrow = N_ind, ncol = N_zones)  
weights1 <-weights2 <-weights3 <-weights4 <-weights0
dim(weights0)

category.labels<-colnames(consV[,])

# Convert survey data into aggregates to compare with census
indAgg0<-array(dim=c(N_zones,N_cons))                             # zones x constraints (12x28)
indAgg1<-indAgg2<-indAgg3<-indAgg4<-indAgg0

#indAgg <- colSums(IndCat) 
#indAgg0<-t(apply(consV, 1, function(x) 1 * indAgg))
colnames(indAgg0)<-colnames(consV)


for (i in 1:N_zones) {
  indAgg0[i, ] <- colSums(IndCat * weights0[, i])}
indAgg0
consV
plot(as.vector(as.matrix(consV)), as.vector(as.matrix(indAgg0[,1:28])), xlab = "Constraints", 
     ylab = "Model output")
abline(a = 0, b = 1)     #add equality line
COR0=cor(as.vector(as.matrix(consV)), as.vector(as.matrix(indAgg0[,1:28])))


#### CONSTRAINT 1: Ownership of Dwelling (h1_5)

for(j in 1:N_zones){
  for(i in 1:8){
    index <- IndCat[, i] == 1    #indicate the individual households where a category of the constraint is present (TRUE==1)
    weights1[index, j] <- weights0[index, j] * consV[j, i] / indAgg0[j, i] }}
print(weights1)

# Convert weights back into aggregate values for each zone
for(i in 1:N_zones){
  indAgg1[i, ] <- colSums(IndCat * weights1[, i])}
# 
plot(as.vector(as.matrix(consV)), as.vector(as.matrix(indAgg1)), xlab = "Constraints", 
     ylab = "Model output")
abline(a = 0, b = 1)
COR1=cor(as.vector(as.matrix(consV)), as.vector(as.matrix(indAgg1)))

#### CONSTRAINT 2: Land Ownership (h1_4)

for(j in 1:N_zones){
  for(i in 1:14){
    index <- IndCat[, i] == 1    #indicate the individual households where a category of the constraint is present (TRUE==1)
    weights2[index, j] <- weights1[index, j] * consV[j, i] / indAgg1[j, i] }}
print(weights2)

# Convert weights back into aggregate values for each zone
for(i in 1:N_zones){
  indAgg2[i, ] <- colSums(IndCat * weights2[, i])}
# 
plot(as.vector(as.matrix(consV)), as.vector(as.matrix(indAgg2)), xlab = "Constraints", 
     ylab = "Model output")
abline(a = 0, b = 1)
COR2=cor(as.vector(as.matrix(consV)), as.vector(as.matrix(indAgg2)))

#### CONSTRAINT 3: External Wall Material (h1_2)

for(j in 1:N_zones){
  for(i in 1:21){
    index <- IndCat[, i] == 1
    weights3[index, j] <- weights2[index, j] * consV[j , i] / indAgg2[j, i]}}

for(i in 1:N_zones){
  indAgg3[i, ] <- colSums(IndCat * weights3[, i])}

plot(as.vector(as.matrix(consV)), as.vector(as.matrix(indAgg3)), xlab = "Constraints", 
     ylab = "Model output")
abline(a = 0, b = 1)
COR3=cor(as.vector(as.matrix(consV)), as.vector(as.matrix(indAgg3)))

#### CONSTRAINT 4: Number of Rooms (h1_14)

for(j in 1:N_zones){
  for(i in 1:28){
    index <- IndCat[, i] == 1
    weights4[index, j] <- weights3[index, j] * consV[j , i] / indAgg3[j, i]}}

for(i in 1:N_zones){
  indAgg4[i, ] <- colSums(IndCat * weights4[, i])}

plot(as.vector(as.matrix(consV)), as.vector(as.matrix(indAgg4)), xlab = "Constraints", 
     ylab = "Model output")
abline(a = 0, b = 1)
COR4=cor(as.vector(as.matrix(consV)), as.vector(as.matrix(indAgg4)))


#############     Iterate the above process n times    ###############

library(ipfp) # load ipfp library after install.packages("ipfp")

consV <- apply(consV, 2, as.numeric) # to 1d numeric data type

weights<- weights0 # create a copy of the weights object
for(i in 1:N_zones){
  weights[,i] <- ipfp(consV[i,], t(IndCat), rep(1,N_ind), maxit = 10)}

indAgg <- t(apply(weights, 2, function(x) colSums(x * IndCat)))
colnames(indAgg) <- colnames(consV)   # make the column names equal

plot(as.vector(as.matrix(consV)), as.vector(as.matrix(indAgg)), xlab = "Constraints", 
     ylab = "Model output")
abline(a = 0, b = 1)

COR.10it=cor(as.vector(as.matrix(consV)), as.vector(as.matrix(indAgg)))

Constraint = c(rep("Ownership of Dwelling", 8*nrow(consV)),rep("Land Ownership", 6*nrow(consV)), 
               rep("Wall Material", 7*nrow(consV)),rep("Number of Rooms", 7*nrow(consV)))
p10<-qplot(as.vector(t(t(consV))), as.vector(t(t(indAgg[,1:28]))), color = Constraint, alpha=I(1))+
  xlab("Census Households") +
  ylab("Simulated Households")+geom_abline(intercept =0, slope=1) +xlim(0,20000)+ylim(0,20000)+
  theme(axis.title=element_text(size="14"),
        legend.text=element_text(size="14"), axis.text=element_text(size="11"),
        legend.title=element_text(size="14"))+
  scale_color_discrete(breaks=c("Ownership of Dwelling","Land Ownership","Wall Material","Number of Rooms"))


#####################  EXPANSION WITHOUT INTEGERAZATION  #######################
setwd("/Users/mariagaspari/Desktop/UCL_Thesis/SimLucia/GIT")
weights_F<-c(weights)
indD_12times<-do.call("rbind",replicate(12,indD,simplify=FALSE))
FINAL_t<-data.frame(weights_F,indD_12times,zone = rep(1:nrow(consV), rep(100,12)))
View(FINAL_t)
NoHhlds<-sum(FINAL_t$weights_F)

write.table(FINAL_t,'Results_SimLucia.xls',sep='\t')


