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
copy$Edema_dummy <- ifelse(copy$Spiders == "Y", 1, ifelse(copy$Spiders == "N",2,3))

#simple model
library(cmdstanr)
mod <- cmdstan_model("code.stan")
fit <- mod$sample(
                data = list(
                  N = 276,
                  drug_use = copy$drug_use,
                  Bilirubin = copy$Bilirubin,
                  platelet = copy$Platelets,
                  survival = copy$flag),
                
                iter_sampling = 10000
                )

fit$summary()
probability  = fit$draws("p",format = 'df')



#using endema, drug use and etc.
mod_2 <- cmdstan_model("code_drug_use.stan")
fit_2 <- mod_2$sample(
  data = list(
    N = 276,
    drug_use = copy$drug_use,
    Bilirubin = copy$Bilirubin,
    platelet = copy$Platelets,
    survival = copy$flag,
    endema = copy$Edema_dummy,
    Spiders = copy$Spiders_dummy,
    Ascites = copy$Ascites_dummy
    ),
  
  iter_sampling = 10000
)

fit_2$summary()
probability  = fit_2$draws("p",format = 'df')
