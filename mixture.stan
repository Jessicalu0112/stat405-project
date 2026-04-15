data {
  int<lower=0> N;
  array[N] int drug_use;
  vector[N] Bilirubin;
  vector[N] platelet;
  array[N] int edema;
  vector[N] Ascites;
  vector[N] Spiders;
  array[N] int survival;
}

parameters {
  real k_drug_use;
  real k_drug_no_use;
  real k_Bilirubin;
  real k_platelet;
  real k_edema_type_1;
  real k_edema_type_2;
  real k_edema_type_3;
  real k_Ascites;
  real k_Spiders;
}

transformed parameters {
  vector<lower=0, upper=1>[N] p_drug;
  vector<lower=0, upper=1>[N] p_no_drug;

  for (n in 1:N) {
    if (drug_use[n] == 1) {
      if (edema[n] == 1) {
        p_drug[n] = inv_logit(
          k_drug_use + k_Bilirubin * Bilirubin[n] + k_platelet * platelet[n]
          + k_Ascites * Ascites[n] + k_Spiders * Spiders[n] + k_edema_type_1
        );
      } else if (edema[n] == 2) {
        p_drug[n] = inv_logit(
          k_drug_use + k_Bilirubin * Bilirubin[n] + k_platelet * platelet[n]
          + k_Ascites * Ascites[n] + k_Spiders * Spiders[n] + k_edema_type_2
        );
      } else {
        p_drug[n] = inv_logit(
          k_drug_use + k_Bilirubin * Bilirubin[n] + k_platelet * platelet[n]
          + k_Ascites * Ascites[n] + k_Spiders * Spiders[n] + k_edema_type_3
        );
      }
    } else {
      p_drug[n] = 1e-5;
    }
  }

  for (n in 1:N) {
    if (drug_use[n] != 1) {
      if (edema[n] == 1) {
        p_no_drug[n] = inv_logit(
          k_drug_no_use + k_Bilirubin * Bilirubin[n] + k_platelet * platelet[n]
          + k_Ascites * Ascites[n] + k_Spiders * Spiders[n] + k_edema_type_1
        );
      } else if (edema[n] == 2) {
        p_no_drug[n] = inv_logit(
          k_drug_no_use + k_Bilirubin * Bilirubin[n] + k_platelet * platelet[n]
          + k_Ascites * Ascites[n] + k_Spiders * Spiders[n] + k_edema_type_2
        );
      } else {
        p_no_drug[n] = inv_logit(
          k_drug_no_use + k_Bilirubin * Bilirubin[n] + k_platelet * platelet[n]
          + k_Ascites * Ascites[n] + k_Spiders * Spiders[n] + k_edema_type_3
        );
      }
    } else {
      p_no_drug[n] = 1e-5;
    }
  }
}

model {
  k_drug_use ~ normal(0,10);
  k_drug_no_use ~ normal(0,10);
  k_Bilirubin ~ normal(-2,1);
  k_platelet ~ normal(-2,1);
  k_edema_type_1 ~ normal(2,1);
  k_edema_type_2 ~ normal(0,1);
  k_edema_type_3 ~ normal(0, 1);
  
  
  for (n in 1:N){
    if (drug_use[n] == 1){
        survival[n] ~ bernoulli(p_drug[n]);
  }else{
    survival[n] ~ bernoulli(p_no_drug[n]);
  }
  }
}



