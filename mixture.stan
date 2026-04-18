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
  ordered[2] k_drug_comp;
  real k_Bilirubin;
  real k_platelet;
  real k_Ascites;
  real k_Spiders;
  real k_edema_type_1;
  real k_edema_type_2;
  real k_edema_type_3;
  real<lower=0, upper=1> theta;
}


transformed parameters {
  vector[N] eta_comp1;
  vector[N] eta_comp2;

  for (n in 1:N) {
    real edema_effect;
    if (edema[n] == 1) {
      edema_effect = k_edema_type_1;
    } else if (edema[n] == 2) {
      edema_effect = k_edema_type_2;
    } else {
      edema_effect = k_edema_type_3;
    }
    real xb;
    xb = k_Bilirubin * Bilirubin[n] + k_platelet * platelet[n]
      + k_Ascites * Ascites[n] + k_Spiders * Spiders[n] + edema_effect;

    eta_comp1[n] = xb + k_drug_comp[1] * drug_use[n];
    eta_comp2[n] = xb + k_drug_comp[2] * drug_use[n];
  }
}


model {
  k_Bilirubin ~ normal(-2,1);
  k_platelet ~ normal(-2,1);
  k_edema_type_1 ~ normal(2,1);
  k_edema_type_2 ~ normal(0,1);
  k_edema_type_3 ~ normal(-2,1);
  
  k_drug_comp ~ normal(0, 1);
  theta ~ beta(1, 1);

  for (n in 1:N) {
    target += log_mix(
      theta,
      bernoulli_logit_lpmf(survival[n] | eta_comp1[n]),
      bernoulli_logit_lpmf(survival[n] | eta_comp2[n])
    );
  }
}
