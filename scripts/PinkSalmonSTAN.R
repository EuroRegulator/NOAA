library("rstan") # observe startup messages
options(mc.cores = parallel::detectCores()) #RAM to estimate your model in parallel
rstan_options(auto_write = TRUE) #automatically save a bare version of a compiled Stan program to the hard disk so that it does not need to be recompiled (unless you change it).
#Sys.setenv(LOCAL_CPPFLAGS = '-march=native')
verbose=TRUE # error dumping

bevholt = function(S, prod, cap) {
  S/(1/prod+S/cap)
}

set.seed(200)
xs <- seq(.1, 5000,length.out=199)
ys <- numeric(length(xs))
for(i in 1:length(xs)) {
  ys[i] <- bevholt(xs[i], 1, 100) + rnorm(1, 0, 10)
}
plot(ys ~ xs)
stanDat <- list(Nyears=length(xs), R=ys, S=xs)
fit <- stan("BH.stan", data = stanDat,  iter=1200, warmup=500, chains=1, seed=483892929, refresh=1200)
samp <- extract(fit)
curve(bevholt(x, mean(samp$pro), mean(samp$cap)), add=T)

pinks <- read.csv("PSalmonAK.csv", header=T)
stanDat <- list(R=pinks$ret, S=pinks$esc, Nyears=30)
fit <- stan("BH.stan", data = stanDat,  iter=1200, warmup=500, chains=1, seed=483892929, refresh=1200)
plot(pinks$ret ~ pinks$esc, data=pinks, ylim=c(0,30000), xlim=c(0,10000))
samp <- extract(fit)
curve(bevholt(x, mean(samp$pro), mean(samp$cap)), add=T)
mean(samp$pro)
mean(samp$cap)