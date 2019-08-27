
data {
  int<lower=0> Nyears;
  vector[Nyears] Sobs;
  vector[Nyears] Pha;
  vector[Nyears] HR;
}

parameters {
  real<lower=0> pro;                
  real<lower=0> cap;  
  real<lower=0> sigma;
  vector<lower=0>[Nyears] S_raw;
}

transformed parameters {
  real S[Nyears];
  S[1] = 1000;
  for (i in 1:(Nyears-1)) {
    S[i+1] = (S[i]/(1/pro + S[i]/cap))*(1/(1-Pha[i]))*(1-HR[i]) * exp(sigma* S_raw[i]);
    //print("i = ", i, ", pro=", pro, " cap=", cap," Si+1= ", S[i+1]);
  }
}

model {  
  pro ~ lognormal(log(3), 1);  
  cap ~ lognormal(log(15000), 1);
  S_raw ~ normal(0,1);
  sigma ~ normal(0,1);
  
  Sobs ~ lognormal(log(S),.15);
}

