library('rjags')

bevholt = function(S, prod, cap) {
  S/(1/prod+S/cap)
}
# OS need to put in model as well put in same place (ocean survival)
# hatchery fish or harvest rate
#process model
N <- 100
OS <- rep(c(.2,.4,.8,.6,1), rep(N/5,5))
S <- numeric(N)
S[1] <- 1000
cap <- 1200
prod <- 2.5
for (i in 1:(N-1)){
  S[i+1] <- rlnorm(1, log(bevholt(S[i], prod, cap)*OS[i]), 0.1)
}

#observation model
Sobs <- numeric(N)
for (i in 1:N) {
  Sobs[i] <- rlnorm(1, log(S[i]), .15)
}
plot(S[-100],S[-1]/OS[-1])

#generating model
initValFunc <- function() {
  list(prod=1, cap=600)
}

jagsDat <- list(Sobs=Sobs, Nyears=100, OS=OS)
m1 <- jags.model("stateB.bug", data=jagsDat, initValFunc, n.chains=3, n.adapt=5000)
s1 <- jags.samples(m1,n.iter=10000,variable.names=c("prod","cap", "tau", "S"))
prodLi <- s1$prod
capLi <- s1$cap
sigmaLi <- sqrt(1/s1$tau)
last <- s1$S
median(prodLi)
median(capLi)

stanDat <- list(Sobs=Sobs, Nyears=100, OS=OS)
fit <- stan("stateB.stan", data = stanDat,  iter=1200, warmup=500, chains=1, seed=483892929, refresh=1200)
samp <- extract(fit)


#############################################################################


Ndraws <- 5
for (i in 1:Ndraws) {
  draw <- sample(1:length(prodLi), 1)
  Sp <- numeric(N)
  Sp[1] <- last[draw*100]
  for (yr in 1:(N-1)){
    Sp[yr+1] <- rlnorm(1, log(bevholt(Sp[yr], prodLi[draw], capLi[draw])), sigmaLi[draw])
  }
  if (i == 1) {
    plot(Sp, type = "l", col = i, ylim = c(0, 1500), lwd = 3)  
  } else {
    lines(Sp, type = "l", col = i)
  }
}

for (i in 1:Ndraws) {
  draw <- sample(1:length(prodLi), 1)
  Sp <- numeric(N)
  Sp[1] <- 1000
  for (yr in 1:(N-1)){
    Sp[yr+1] <- rlnorm(1, log(bevholt(Sp[yr], prodLi[draw], capLi[draw])), sigmaLi[draw])
  }
  if (i == 1) {
    plot(Sp, type = "l", col = i, ylim = c(0, 1500), lwd = 3)  
  } else {
    lines(Sp, type = "l", col = i)
  }
}

  
  
  
  
  
  