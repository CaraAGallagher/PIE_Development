# Cara Gallagher
# February 9th, 2023
# Energy Budget with Individual Variation project
# Morris screening with sensitivity package
# Output analysis

#------------------------------------------------#
####  Packages: #### 
library("sensitivity")
library("tidyverse")

source("Scripts/Sensitivity analysis scripts/SA_morris_params.R")

#results <- read_csv("Data/Sensitivity analysis/SA_morris-5863.csv", skip = 6)
results <- read_csv("Data/Sensitivity analysis/SA_morris_3-6469.csv", skip = 6)

results <- results %>%
  rename("run_number" = '[run number]', "step" = '[step]') %>% 
  select(run_number, step, SA.BMadult, SA.BMemb, SA.BMwean, SA.age1stbirth, SA.LpY, SA.LSb, SA.dens)

inputs <- read_csv("Data/Sensitivity analysis/Morris_sensitivity_inputs.csv")

names(inputs) <- c("run_number", parameter_combos$ParameterCode)

patterns <- c("SA.BMadult", "SA.BMemb", "SA.BMwean", "SA.age1stbirth", "SA.LpY", "SA.LSb", "SA.dens")

finalOut <- tibble(params = colnames(inputs)[-1])

for (i in 1:length(patterns)) {
  
Morris <- left_join(inputs, results, by = "run_number")

Morris <- Morris %>% 
  rename(y = patterns[i]) %>% 
  pull(y)

M$X <- inputs[,-1]

tell(M, Morris)

mu_star <- (apply(M$ee, 2, function(M) mean(abs(M))))

max_mu <- max(mu_star)

mu <- (mu_star / max_mu) * 100

#print(mu)

finalOut <- cbind(
  finalOut,
  mu
)
}

colnames(finalOut) <- c("Parameter", patterns)

finalOut %>% 
  select(-SA.LpY) %>% 
  mutate(Tot = rowSums(across(where(is.numeric)))) %>% 
  arrange(-Tot) %>% 
  slice_head(n = 10)


#write.csv(finalOut, "Data/Sensitivity analysis/Morris_Outputs.csv")

ggplot(results1, aes(x = `HR-r-max`, y = SA.age1stbirth)) +
  geom_jitter() +
  geom_smooth(method = "loess")
 
 
sigma <- (apply(M$ee, 2, sd))
 
out <- bind_cols("param" = colnames(inputs)[-1], "mu" = mu.star, "sigma" = sigma)
 
 
morplot <- ggplot(out, aes(mu, sigma, label = param))+
 geom_text(size = 2)
morplot

plot(M)

outSums <- finalOut %>% 
  rowwise() %>% 
  mutate(total = sum(c_across(2:8))) %>% 
  arrange(desc(total))
