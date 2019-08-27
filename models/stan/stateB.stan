data {
  int<lower=1> Nyears;    // number of years        
  vector[Nyears] Sobs;
  vector[Nyears] OS;
}

parameters {
  real<lower=0> pro;                
  real<lower=0> cap;          
  real<lower=0> tau;      // error SD
}

model {
  vector[Nyears] S;
  S[1] ~ lognormal_lpdf(0,0.0001);
  for (i in 1:(Nyears-1)) {
    S[i+1] ~ lognormal_lpdf(log((S[i]/(1/pro + S[i]/cap))*OS[i]), tau);
  }
  
  for (i in 1:Nyears) {
    Sobs[i] ~ lognormal_lpdf(log(S[i]),1/(.15*.15));
  }
  pro ~ lognormal_lpdf(log(3), 0.01) ;  
  cap ~ lognormal_lpdf(log(15000), 0.001);
  tau ~ gamma(0.001, 0.001);
}
