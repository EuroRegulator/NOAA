library('rjags')

bevholt = function(S, prod, cap) {
  S/(1/prod+S/cap)
}
# OS need to put in model as well put in same place (ocean survival)
# hatchery fish or harvest rate
#process model
set.seed(123)
N <- 100
Pha <- sample(rep(seq(0, .9, .1), rep(N/10,10)))
HR <- sample(rep(seq(0, .63, .07), rep(N/10,10)))
S <- numeric(N)
S[1] <- 1000
cap <- 1200
prod <- 2.5
for (i in 1:(N-1)){
  S[i+1] <- rlnorm(1, log(bevholt(S[i], prod, cap)*(1-HR[i])/(1-Pha[i])), .1)
}

#observation model
Sobs <- numeric(N)
for (i in 1:N) {
  Sobs[i] <- rlnorm(1, log(S[i]), .15)
}
plot(Sobs[-100],Sobs[-1]*(1-Pha[-100])/(1-HR[-100]))
#plot(Sobs[-100],Sobs[-1])

#generating model
initValFunc <- function() {
  list(prod=1, cap=600)
}

jagsDat <- list(Sobs=Sobs, Nyears=100, Pha=Pha, HR=HR)
m1 <- jags.model("realistic.bug", data=jagsDat, initValFunc, n.chains=3, n.adapt=5000)
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
plot(Sobs[-100],Sobs[-1]*(1-Pha[-100])/(1-HR[-100]), ylim = c(0,2000))
SS <- 0:10000
Ndraws <- 20
for (i in 1:Ndraws) {
  draw <- sample(1:length(prodLi), 1)
  lines(SS, bevholt(SS, prodLi[draw], capLi[draw]), type = "l", col = 2, lwd = 1)  
}
lines(SS, bevholt(SS, midP, midC), type = "l", col = 1, lwd = 4)  
lines(SS, SS)

############################################################# STAN
library("rstan") # observe startup messages
options(mc.cores = parallel::detectCores()) #RAM to estimate model in parallel
rstan_options(auto_write = TRUE) #automaticasally save a bare version of compiled Stan program
#Sys.setenv(LOCAL_CPPFLAGS = '-march=native')
verbose=TRUE # error dumping

stanDat <- list(Sobs=Sobs, Nyears=100, Pha=Pha, HR=HR)
fit <- stan("realistic.stan", data = stanDat,  iter=100, warmup=10, chains=1, seed=483892929)
samp <- extract(fit)


