data {
  int<lower=0> N;
  vector[N] drug_use;
  vector[N] Bilirubin;
  vector[N] platelet;
  vector[N] endema;
  vector[N] Ascites;
  vector[N] Spiders;
  array[N] int survival;
}

parameters {
  real k_drug_use;
  real k_Bilirubin;
  real k_platelet;
  real k_endema_type_1;
  real k_endema_type_2;
  real k_endema_type_3;
  real k_Ascites;
  real k_Spiders;
}

transformed parameters{
  vector<lower=0, upper=1>[N] p;
  for (n in 1:N){
    if (endema[n] == 1){
     p[n] = inv_logit(k_drug_use * drug_use[n] + k_Bilirubin*Bilirubin[n] + k_platelet*platelet[n]+k_Ascites*Ascites[n]+k_Spiders*Spiders[n]+k_endema_type_1*endema[n]); 
    }
    if (endema[n] == 2){
     p[n] = inv_logit(k_drug_use * drug_use[n] + k_Bilirubin*Bilirubin[n] + k_platelet*platelet[n]+k_Ascites*Ascites[n]+k_Spiders*Spiders[n]+k_endema_type_2*endema[n]); 
    }
    if (endema[n] == 3){
     p[n] = inv_logit(k_drug_use * drug_use[n] + k_Bilirubin*Bilirubin[n] + k_platelet*platelet[n]+k_Ascites*Ascites[n]+k_Spiders*Spiders[n]+k_endema_type_3*endema[n]); 
    }
  }
}

model {
  k_drug_use ~ normal(-2,1);
  k_Bilirubin ~ normal(-2,1);
  k_platelet ~ normal(-2,1);
  k_endema_type_1 ~ normal(2,1);
  k_endema_type_2 ~ normal(0,1);
  k_endema_type_3 ~ normal(-2,1);
  
  for (n in 1:N){
    survival[n] ~ bernoulli(p[n]);
  }
}