data {
  int<lower=0> N;
  vector[N] drug_use;
  vector[N] Bilirubin;
  vector[N] platelet;
  array[N] int survival;
}

parameters {
  real k_drug_use;
  real k_Bilirubin;
  real k_platelet;
}



model {
  k_drug_use ~ normal(-2, 1);
  k_Bilirubin ~ normal(-2, 1);
  k_platelet ~ normal(-2, 1);
  

  for (n in 1:N) {
    survival[n] ~ bernoulli_logit(k_drug_use * drug_use[n] + k_Bilirubin * Bilirubin[n] + k_platelet * platelet[n]);
  }
}


