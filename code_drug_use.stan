
data {
  int<lower=0> N;
  vector[N] drug_use;
  vector[N] Bilirubin;
  vector[N] platelet;
  vector[N] edema;
  vector[N] Ascites;
  vector[N] Spiders;
  array[N] int survival;
}
parameters {
  real k_drug_use;
  real k_Bilirubin;
  real k_platelet;
  real k_edema_type_1;
  real k_edema_type_2;
  real k_edema_type_3;
  real k_Ascites;
  real k_Spiders;
}
model {
  k_drug_use ~ normal(-2,1);
  k_Bilirubin ~ normal(-2,1);
  k_platelet ~ normal(-2,1);
  k_edema_type_1 ~ normal(2,1);
  k_edema_type_2 ~ normal(0,1);
  k_edema_type_3 ~ normal(-2,1);
  
  for (n in 1:N){
    real mu;
    if (edema[n] == 1){
      mu = k_drug_use * drug_use[n] + k_Bilirubin*Bilirubin[n] + k_platelet*platelet[n] + k_Ascites*Ascites[n] + k_Spiders*Spiders[n] + k_edema_type_1;
    } else if (edema[n] == 2){
      mu = k_drug_use * drug_use[n] + k_Bilirubin*Bilirubin[n] + k_platelet*platelet[n] + k_Ascites*Ascites[n] + k_Spiders*Spiders[n] + k_edema_type_2;
    } else {
      mu = k_drug_use * drug_use[n] + k_Bilirubin*Bilirubin[n] + k_platelet*platelet[n] + k_Ascites*Ascites[n] + k_Spiders*Spiders[n] + k_edema_type_3;
    }
    survival[n] ~ bernoulli_logit(mu); 
  }
}

