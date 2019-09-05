library('rjags')

bevholt = function(S, prod, cap) {
  S/(1/prod+S/cap)
}
# OS need to put in model as well put in same place (ocean survival)
# hatchery fish or harvest rate
#process model
set.seed(123)
N <- 100
Phas <- sample(rep(seq(0, .9, .1), rep(N/10,10)))
HR <- sample(rep(seq(0, .63, .07), rep(N/10,10)))
S <- numeric(N)
S[1] <- 1000
cap <- 1200
prod <- 2.5
for (i in 1:(N-1)){
  S[i+1] <- rlnorm(1, log(bevholt(S[i], prod, cap)*(1-HR[i])/(1-Phas[i])), .1)
}

#observation model
Sobs <- numeric(N)
for (i in 1:N) {
  Sobs[i] <- rlnorm(1, log(S[i]), .15)
}
par(mfrow=c(2,1))
plot(Sobs[-100],Sobs[-1]*(1-Phas[-100])/(1-HR[-100]), xlab="spawner", ylab="recruitment", main="Processed Data", ylim = c(0,2000))
plot(Sobs[-100],Sobs[-1], xlab="spawner", ylab="recruitment", main="Observed Data")

#generating model
initValFunc <- function() {
  list(prod=1, cap=600)
}

modelLoc <- "C:/Work/NOAA/models/jags/realistic.bug"
jagsDat <- list(Sobs=Sobs, Nyears=N, Phas=Phas, HR=HR)
m1 <- jags.model(modelLoc, data=jagsDat, initValFunc, n.chains=3, n.adapt=5000)
s1 <- jags.samples(m1,n.iter=10000,variable.names=c("prod","cap", "tau", "S"))
prodLi <- s1$prod
capLi <- s1$cap
sigmaLi <- sqrt(1/s1$tau)
last <- s1$S
U <- 1-1/sqrt(prodLi)
median(U)
midP <- median(prodLi)
midC <- median(capLi)
midP
midC
par(mfrow=c(2,2))
hist(U)
hist(capLi)
hist(prodLi)
plot(capLi~prodLi)

par(mfrow = c(1,1))
plot(Sobs[-100],Sobs[-1]*(1-Phas[-100])/(1-HR[-100]), ylim = c(0,2000))
SS <- 0:10000
Ndraws <- 100
for (i in 1:Ndraws) {
  draw <- sample(1:length(prodLi), 1)
  lines(SS, bevholt(SS, prodLi[draw], capLi[draw]), type = "l", col = 2, lwd = .5)  
}
lines(SS, bevholt(SS, midP, midC), type = "l", col = 1, lwd = 4)  
lines(SS, SS)

############################################################# STAN
library("rstan") # observe startup messages
options(mc.cores = parallel::detectCores()) #RAM to estimate model in parallel
rstan_options(auto_write = TRUE) #automaticasally save a bare version of compiled Stan program
#Sys.setenv(LOCAL_CPPFLAGS = '-march=native')
verbose=TRUE # error dumping

modelLoc <- "C:/Work/NOAA/models/stan/realistic.stan"
stanDat <- list(Sobs=Sobs, Nyears=N, Phas=Phas, HR=HR)
fit <- stan(modelLoc, data = stanDat, init=initValFunc, iter=11000, warmup=1000, chains=3, seed=483892929)
samp <- extract(fit)
prodLi <- samp$pro
capLi <- samp$cap
hist(capLi)
hist(prodLi)
plot(capLi~prodLi)
median(samp$pro)
median(samp$cap)

############################################################# Simple Jags
modelLoc <- "C:/Work/NOAA/models/jags/BH.bug"
jagsDat <- list(S=Sobs[-100], R=Sobs[-1]*(1-Phas[-100])/(1-HR[-100]), Nyears=N)
m2 <- jags.model(modelLoc, data=jagsDat, initValFunc, n.chains=3, n.adapt=5000)
s2 <- jags.samples(m2,n.iter=10000,variable.names=c("prod","cap"))

############################################################## Altering error
set.seed(123)
par(mfrow = c(2,2))
N <- 100
Phas <- sample(rep(seq(0, .9, .1), rep(N/10,10)))
HR <- sample(rep(seq(0, .63, .07), rep(N/10,10)))
S <- numeric(N)
PErr <- seq(.1, .7, .1)
dataState <- numeric(28)
dataCompStat <- matrix(1:(30000*14),30000,14)
modelLoc <- "C:/Work/NOAA/models/jags/realistic.bug"
for (j in 1:1) {
  S[1] <- 1000
  cap <- 1200
  prod <- 2.5
  for (i in 1:(N-1)){
    S[i+1] <- rlnorm(1, log(bevholt(S[i], prod, cap)*(1-HR[i])/(1-Phas[i])), PErr[j])
  }
  
  #observation model
  Sobs <- numeric(N)
  for (i in 1:N) {
    Sobs[i] <- rlnorm(1, log(S[i]), .15)
  }
  jagsDat <- list(Sobs=Sobs, Nyears=N, Phas=Phas, HR=HR)
  m1 <- jags.model(modelLoc, data=jagsDat, initValFunc, n.chains=3, n.adapt=5000)
  s1 <- jags.samples(m1,n.iter=10000,variable.names=c("prod","cap", "tau", "S"))
  prodLi <- s1$prod
  capLi <- s1$cap
  dataCompStat[1:30000,2*j] <- capLi
  dataCompStat[1:30000,2*j-1] <- prodLi
  dataState[4*j] <- sd(capLi)
  dataState[4*j-1] <- sd(prodLi)
  dataState[4*j-2] <- median(capLi)
  dataState[4*j-3] <- median(prodLi) 
  sigmaLi <- sqrt(1/s1$tau)
  U <- 1-1/sqrt(prodLi)
  plot(Sobs[-100],Sobs[-1]*(1-Phas[-100])/(1-HR[-100]), ylim = c(100,1800), xlab="spawner", ylab="recruitment", main=paste("State Model Process Error =",PErr[j]))
  SS <- 0:10000
  Ndraws <- 100
  for (i in 1:Ndraws) {
    draw <- sample(1:length(prodLi), 1)
    lines(SS, bevholt(SS, prodLi[draw], capLi[draw]), type = "l", col = 2, lwd = .5)  
  }
  lines(SS, bevholt(SS, median(prodLi), median(capLi)), type = "l", col = 1, lwd = 4)  
  lines(SS, SS)
}
tState <- matrix(dataState,ncol=7,byrow=FALSE)
colnames(tState) <- c("1","2","3","4","5","6","7")
rownames(tState) <- c("Prod Median", "Cap Median", "Prod sd", "Cap sd")
tState <- as.table(tState)

dataSimple <- numeric(28)
modelLoc <- "C:/Work/NOAA/models/jags/BH.bug"
for (j in 1:7) {
  S[1] <- 1000
  cap <- 1200
  prod <- 2.5
  for (i in 1:(N-1)){
    S[i+1] <- rlnorm(1, log(bevholt(S[i], prod, cap)*(1-HR[i])/(1-Phas[i])), PErr[j])
  }
  
  #observation model
  Sobs <- numeric(N)
  for (i in 1:N) {
    Sobs[i] <- rlnorm(1, log(S[i]), .15)
  }
  jagsDat <- list(S=Sobs[-100], R=Sobs[-1]*(1-Phas[-100])/(1-HR[-100]), Nyears=N)
  m2 <- jags.model(modelLoc, data=jagsDat, initValFunc, n.chains=3, n.adapt=5000)
  s2 <- jags.samples(m2,n.iter=10000,variable.names=c("prod","cap"))
  prodLi <- s2$prod
  capLi <- s2$cap
  dataSimple[4*j] <- sd(capLi)
  dataSimple[4*j-1] <- sd(prodLi)
  dataSimple[4*j-2] <- median(capLi)
  dataSimple[4*j-3] <- median(prodLi) 
  sigmaLi <- sqrt(1/s2$tau)
  U <- 1-1/sqrt(prodLi)
  plot(Sobs[-100],Sobs[-1]*(1-Phas[-100])/(1-HR[-100]), ylim = c(100,1800), xlab="spawner", ylab="recruitment", main=paste("Simple Model Process Error =",PErr[j]))
  SS <- 0:10000
  Ndraws <- 100
  for (i in 1:Ndraws) {
    draw <- sample(1:length(prodLi), 1)
    lines(SS, bevholt(SS, prodLi[draw], capLi[draw]), type = "l", col = 2, lwd = .5)  
  }
  lines(SS, bevholt(SS, median(prodLi), median(capLi)), type = "l", col = 1, lwd = 4)  
  lines(SS, SS)
}
tSimple <- matrix(dataSimple,ncol=7,byrow=FALSE)
colnames(tSimple) <- c("1","2","3","4","5","6","7")
rownames(tSimple) <- c("Prod Median", "Cap Median", "Prod sd", "Cap sd")
tSimple <- as.table(tSimple)

