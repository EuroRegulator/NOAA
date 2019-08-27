data {
  int<lower=1> Nyears;    // number of years 
  vector[Nyears] R;          
  vector[Nyears] S;              
}

parameters {
  real<lower=0> pro;                
  real<lower=0> cap;          
  real<lower=0> sigma;      // error SD
}

model {
  pro ~ normal(0, 1);
  cap ~ uniform(0, 30000); //(0, 100) for simulated data 
  for (i in 1:Nyears) {
      R[i] ~ lognormal(log(S[i]/(1/pro + S[i]/cap)), sigma);
  }
}
