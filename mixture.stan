data {
  int<lower=1> N;
  array[N] int survival;
  array[N] int drug_use;
  vector[N] Bilirubin;
  vector[N] platelet;
  array[N] int Ascites;
  array[N] int Spiders;
  array[N] int Edema_S;
  array[N] int Edema_Y;
}

parameters {
  real k_intercept;
  real k_Bilirubin;
  real k_platelet;
  real k_Ascites;
  real k_Spiders;
  real k_Edema_S;
  real k_Edema_Y;
  ordered[2] k_drug_comp;
  real<lower=0, upper=1> theta;
}


transformed parameters {
  vector[N] eta_comp1;
  vector[N] eta_comp2;

  for (n in 1:N) {
    real xb;
    xb =
      k_intercept + k_Bilirubin * Bilirubin[n] + k_platelet * platelet[n]
      + k_Ascites * Ascites[n] + k_Spiders * Spiders[n] + k_Edema_S * Edema_S[n]
      + k_Edema_Y * Edema_Y[n];

    eta_comp1[n] = xb + k_drug_comp[1] * drug_use[n];
    eta_comp2[n] = xb + k_drug_comp[2] * drug_use[n];
  }
}


model {
  k_intercept ~ normal(0, 2);
  k_Bilirubin ~ normal(0, 1);
  k_platelet ~ normal(0, 1);
  k_Ascites ~ normal(0, 1);
  k_Spiders ~ normal(0, 1);
  k_Edema_S ~ normal(0, 1);
  k_Edema_Y ~ normal(0, 1);
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
