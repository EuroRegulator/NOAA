
data {
  int<lower=0> Nyears;
  vector[Nyears] Sobs;
  vector[Nyears] Phas;
  vector[Nyears] HR;
}

parameters {
  real<lower=0> pro;                
  real<lower=0> cap;  
  real<lower=0> S[Nyears];
  real<lower=0> sigma;
}

model {  
  pro ~ lognormal(log(3), 1);  
  cap ~ lognormal(log(15000), 1);
  sigma ~ normal(0,1);
  
  S[1] ~ lognormal(log(1000), .001);
  for(i in 1:(Nyears-1)) { 
    S[i+1] ~ lognormal(log((S[i]/(1/pro + S[i]/cap))*(1/(1-Phas[i]))*(1-HR[i])), sigma);
  }
  
  Sobs ~ lognormal(log(S),.15);
}

