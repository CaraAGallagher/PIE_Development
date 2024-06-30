# Cara Gallagher
# February 13th, 2023
# Energy Budget with Individual Variation project
# Sobol sensitivity analysis with sensitivity package
# Parameter set creation

#------------------------------------------------#
####  Packages: #### 
library("sensitivity")
library("tidyverse")
library("lhs")

MIparameters <- c(
  "DE-fat",
  "emb-growth-c",
  "t-gest",
  "max-SL",
  "t-mating-start",
  "gamma",
  "t-mating-end",
  "slope-pcot",
  "t-max-age",
  "HIF"
)

parameter_combos <- read_csv("Data/Sensitivity analysis/Sensitivity_analysis.csv")

parameter_combos <- parameter_combos %>% 
  select(`Parameter code`, `Lower extreme`, `Lower median`, `Central value`, `Upper median`, `Upper extreme`) %>% 
  rename(ParameterCode = `Parameter code`, LExt = `Lower extreme`, LMed = `Lower median`, Cen = `Central value`, UMed = `Upper median`, UExt = `Upper extreme`) %>% 
  drop_na() %>% 
  mutate_at(vars(-1), as.numeric) %>% 
  filter(ParameterCode %in% MIparameters)

# create two LHS matrices to develop Sobol inputs
LHS1 <- randomLHS(500, 10)
LHS2 <- randomLHS(500, 10)

# for LHS1
LHS1new <- tibble()

for (i in 1:nrow(parameter_combos)) {
  for (n in 1:nrow(LHS1)) {
  LHS1new[n,i] <- parameter_combos[i,2] + (LHS1[n,i] * (parameter_combos[i,6] - parameter_combos[i,2]))
  }
}

colnames(LHS1new) <- parameter_combos$ParameterCode

# for LHS2
LHS2new <- tibble()

for (i in 1:nrow(parameter_combos)) {
  for (n in 1:nrow(LHS2)) {
    LHS2new[n,i] <- parameter_combos[i,2] + (LHS2[n,i] * (parameter_combos[i,6] - parameter_combos[i,2]))
  }
}

colnames(LHS2new) <- parameter_combos$ParameterCode

# now create Sobol inputs
#S <- sobol2007(model = NULL, LHS1new, LHS2new, nboot = 1000)
#S <- sobol(model = NULL, LHS1new, LHS2new, nboot = 1000)
S <- soboljansen(model = NULL, LHS1new, LHS2new, nboot = 1000)

 
design <- S$X

# output table
#write.csv(design,file='Data/Sensitivity analysis/Sobol_sensitivity_inputs1.csv')




