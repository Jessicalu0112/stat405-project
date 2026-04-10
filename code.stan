//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  vector[N] drug_use;
  vector[N] Bilirubin;
  vector[N] platelet;
  int<lower=0, upper=1> survival[N]; // 1 for C, 0 for D
}
parameters {
  real alpha;
  real b_drug;
  real b_bili;
  real b_plat;
}

model {
  // prior
  alpha ~ normal(0, 5);
  b_drug ~ normal(0, 2);
  b_bili ~ normal(0, 2);
  b_plat ~ normal(0, 2);

  // likelihood
  survival ~ bernoulli_logit(alpha + b_drug * drug_use + b_bili * log(Bilirubin) + b_plat * platelet);
}

generated quantities {
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = bernoulli_logit_lpmf(survival[n] | alpha + b_drug * drug_use[n] + b_bili * log(Bilirubin[n]) + b_plat * platelet[n]);
  }
}