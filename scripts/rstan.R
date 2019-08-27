library("rstan") # observe startup messages
options(mc.cores = parallel::detectCores()) #RAM to estimate your model in parallel
rstan_options(auto_write = TRUE) #automatically save a bare version of a compiled Stan program to the hard disk so that it does not need to be recompiled (unless you change it).
Sys.setenv(LOCAL_CPPFLAGS = '-march=native')
verbose=TRUE # error dumping


bevholt = function(S, a, b) {
  S/(1/a+S/b)
  #(a*S)/(b+S)
}

clrs <- c("black","red","blue")
S <- seq(0.1,5000,length.out=199)
a <- 1
b <- c(0.001,0.002,0.007)
R <- matrix(nrow=length(S),ncol=length(b))
for(i in 1:length(b)) {
  R[,i] <- bevholt(S, a, b[i])
}
plot(R[,1]~S,type="l",lwd=2,xlab="Spawners",ylab="Recruits",ylim=c(0,1000))
lines(R[,2]~S,lwd=2,col=clrs[2])
lines(R[,3]~S,lwd=2,col=clrs[3])

set.seed(200)
xs <- seq(.1, 5000,length.out=199)
ys <- numeric(length(xs))
for(i in 1:length(xs)) {
  ys[i] <- bevholt(xs[i], 1, .007) + rnorm(1, 10, 30)
}
plot(ys ~ xs)
m<-nls(ys~bevholt(xs,a,b), start=c(a=1,b=1))
lines(x, predict(m), col="red")

pinks <- read.csv("PSalmonAK.csv", header=T)
plot(pinks$ret ~ pinks$esc, data=pinks)
maxY <- max(pinks$ret)/2
m<-nls(pinks$ret~bevholt(pinks$esc,a,b), data=pinks, start=c(a=2.5,b=15000))
lines(x, predict(m), col="red")
