## A code for modeling the ecolutionary dynamics of müllerian mimicry species 
# when there are supergeneralists (SG) species 

#Changing my directory
setwd("/Users/irinabb/Dropbox/Mestrado/Am_Nat/codigos_git/mimicry_rings/")

## Parameters
S <- 50 #number of species 
p <- 0.3 #strength of habitat selection 
n <- 10 #number of habitats
h <- 0.25 #heritability 
alfa <- 2 #crontoling trait matching 

##Variables 
z <-  runif(50,0,1) #trait value for each specie
z <- as.vector(z)
z0 <- z

##Importing the occurence matrix in a perfect modular cenario 
M_mod  <- read.table(file="MOD.txt", header=T, row.names=1) 
M <- M_mod

## Randomly choosing sp to be SG
sg <- sample(1:50, 5) #five species among the 50 

## Adding the occurences in all habitats in our occurence matrix 
for(a in 1:5){
  M_mod[sg[a],] <- 1
}

##Builiding Matrix
H  <- diag(h,50,50)
P <-  diag(p,50,50)
I <- diag(1,50,50)

##Theta (Habitat Optimum) values 
teta <- runif(10,0,1)

#inclugin the optimum for SG = mean of all optimuns 
teta.sp <- numeric(50)
for (i in 1:S)
{
  teta.sp[i] <- rowSums(M_mod[i,]*teta)/rowSums(M_mod)[i]  
} 

##Calculating the abundance of species
f_oc <- matrix((sample(c(1:10), 500, replace=T)), nrow=50, ncol=10) #it's a relative frequency
f_oc<- f_oc*M_mod #matching with the occurence matrix

ab <- f_oc
ab <- as.matrix(ab)
ab <- prop.table(ab,2) #standardizing by column 

f_oc <- as.matrix(f_oc)
ab <- ab%*%t(f_oc)
ab <- prop.table(ab,1) #standardizing by row

##Building the Trait Matching Matrix 
dif.z <- matrix(nrow=50,ncol=50)

# Trait difference among species 
for (i in 1:50)
{
  for (j in 1:50)
  {
    dif.z[i,j] <- (z[i]-z[j])^2
  }
}

# Calculating the effect of trait matching 
mat <- exp(-alfa*dif.z)
#diag(mat) <- 0 #in case I want to remove the effect of a specie in itself 

# The effect of trait matching for only species that co-occur
A <- ifelse(ab, 1, 0)
mat <- mat*A
mat <- mat/rowSums(mat)

## Building the Q matrix (the strength of mimetic relation) = effect
#of abundance + trait matching 
Q.abund <- ab*mat
Q.abund <- Q.abund/rowSums(Q.abund)

### Runing the simulation - calculating the final trait (z) after many generations
## (For each generation I need to recalculate the Q matrix)

size <- c(1:1000)
fenotipo <- matrix(nrow = length(size),ncol=50)

for (t in 1:1000)
{ 
  id <- t
  z <- z-H%*%z + (H%*%P)%*%teta.sp + H%*%(I-P)%*%(Q.abund)%*%z 
  for (i in 1:50)
  {
    for (j in 1:50)
    {
      dif.z[i,j] <- (z[i]-z[j])^2
    }
  }
  mat <- exp(-alfa*dif.z)
  #diag(mat) <- 0 #the effect of a species in itself
  mat <- mat*A #for only species that co-occur 
  mat <- mat/rowSums(mat)
  Q.abund <- ab*mat
  Q.abund <- Q.abund/rowSums(Q.abund)
  fenotipo[t,] <- c(z)
}

z.sg.5 <- z

}