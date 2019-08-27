library('rjags')

initValFunc <- function() {
  list(prod=1, cap=10)
}

bevholt = function(S, a, b) {
  S/(1/a+S/b)
  #(a*S)/(b+S)
}


pinks <- read.csv("PSalmonAK.csv", header=T)
jagsDat <- list(R=pinks$ret, S=pinks$esc, Nyears=30)
m1 <- jags.model("BH.bug", data=jagsDat, initValFunc, n.chains=3, n.adapt=1000)
s1 <- jags.samples(m1,n.iter=10000,variable.names=c("prod","cap"))
plot(pinks$ret ~ pinks$esc, data=pinks, ylim=c(0,30000), xlim=c(0,10000))
params <- c("prod", "cap")
samp2 <- coda.samples(m1, params, n.iter=1000)
curve(bevholt(x, samp2[[1]][2], samp2[[1]][1]), add=T)
curve(bevholt(x, median(s1$prod), median(s1$cap)), add=T)

for(samp in sample(1:10000, 30)) curve(bevholt(x, s1$prod[samp], s1$cap[samp]), add=T)
quantile(s1$prod,prob=c(0.025,0.5,0.975))

sVals <- 1:10000

set.seed(200)
xs <- seq(.1, 5000,length.out=199)
ys <- numeric(length(xs))
for(i in 1:length(xs)) {
  ys[i] <- bevholt(xs[i], 1, 100) + rnorm(1, 0, 10)
}
plot(ys ~ xs)
jagsDat <- list(R=ys, S=xs, Nyears=length(xs))
m2 <- jags.model("BH.bug", data=jagsDat, initValFunc, n.chains=3, n.adapt=1000)
s2 <- jags.samples(m2,n.iter=10000,variable.names=c("prod","cap", "sigma"))
#params <- c("prod", "cap")
#samp1 <- jags.samples(m2, params, n.iter=1000)
#samp2 <- coda.samples(m2, params, n.iter=1000)

#mcmc <- as.mcmc(m2) 
#lm1_mcmc_combi <- as.mcmc(rbind(mcmc[[1]], mcmc[[2]], mcmc[[3]]))

#curve(bevholt(x, samp2[[1]][2], samp2[[1]][1]), add=T)