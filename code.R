df = read.csv("cirrhosis.csv")
selected = df[1:312,]
#the selected participants participant in placebo v drug

#simple case: select only death/survive cases as binary
library("dplyr")

copy = selected

copy %>% filter(Status != "CL")

copy <- copy %>%
  mutate(flag = if_else(Status == 'C', 1, 0))

copy <- copy %>%
  mutate(drug_use = if_else(Drug == 'D-penicillamine', 1, 0))

copy <- copy %>% 
  tidyr::drop_na()

copy$Ascites_dummy <- ifelse(copy$Ascites == "Y", 1, 0)
copy$Spiders_dummy <- ifelse(copy$Spiders == "Y", 1, 0)
copy$Edema_dummy <- ifelse(copy$Edema == "Y", 1,
                           ifelse(copy$Edema == "N", 2, 3))

copy <- copy[copy$Status != "CL", ]

#simple model
library(cmdstanr)
mod <- cmdstan_model("code.stan")
# standardized
copy$Bilirubin_s = as.numeric(scale(copy$Bilirubin))
copy$Platelets_s = as.numeric(scale(copy$Platelets))

# data list
stan_data_1 <- list(
  N = 258,
  drug_use = copy$drug_use,
  Bilirubin = copy$Bilirubin_s, 
  platelet = copy$Platelets_s,  
  survival = as.integer(copy$flag)
)
# method1:mcmc
fit <- mod$sample(
  data = stan_data_1, 
  iter_sampling = 10000,
  seed = 123
)

# method2 : VI
fit_1_vi <- mod$variational(data = stan_data_1, seed = 123)

# comparison
print("Model 1 - MCMC Summary:")
fit$summary(c("k_drug_use", "k_Bilirubin"))
print("Model 1 - VI Summary:")
fit_1_vi$summary(c("k_drug_use", "k_Bilirubin"))





#model 2: using edema, drug use and etc.

stan_data_2 <- list(
  N = 258,
  drug_use = as.integer(copy$drug_use), 
  Bilirubin = copy$Bilirubin_s,  
  platelet = copy$Platelets_s,   
  survival = as.integer(copy$flag),
  edema = as.integer(copy$Edema_dummy),
  Spiders = copy$Spiders_dummy,
  Ascites = copy$Ascites_dummy
)

mod_2 <- cmdstan_model("code_drug_use.stan")

# method 1: MCMC
fit_2 <- mod_2$sample(data = stan_data_2, iter_sampling = 10000, seed = 123)
# method 2: VI 
fit_2_vi <- mod_2$variational(data = stan_data_2, seed = 123)
#comparison
print("Model 2 - MCMC Summary:")
print(fit_2$summary())
print("Model 2 - VI Summary:")
print(fit_2_vi$summary())





#model 3: considering mixture model to detect drug efficiency
mod_3 <- cmdstan_model("mixture.stan")
copy$Edema_S <- ifelse(copy$Edema == "S", 1, 0)
copy$Edema_Y <- ifelse(copy$Edema == "Y", 1, 0)
stan_data_3 <- list(
  N = 258,
  survival = copy$flag,
  drug_use = copy$drug_use,
  Bilirubin = copy$Bilirubin_s,
  platelet = copy$Platelets_s,
  Ascites = copy$Ascites_dummy,
  Spiders = copy$Spiders_dummy,
  Edema_S = copy$Edema_S,
  Edema_Y = copy$Edema_Y
)
#method 1
fit_3 <- mod_3$sample(
  data = stan_data_3,
  iter_warmup = 1000,
  iter_sampling = 2000
)

fit_3$summary()



# method 2
fit_3_vi <- mod_3$variational(
  data = stan_data_3,
  seed = 123,
  output_samples = 2000
)

#comparison
print("Model 3 - MCMC Summary:")
print(fit_3$summary())
print("Model 3 - VI Summary:")
print(fit_3_vi$summary())
sh
