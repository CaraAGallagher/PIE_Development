# Cara Gallagher
# February 9th, 2023
# Energy Budget with Individual Variation project
# Morris screening with sensitivity package
# Parameter set creation 

#------------------------------------------------#
####  Packages: #### 
library("sensitivity")
library("tidyverse")

parameter_combos <- read_csv("Data/Sensitivity analysis/Sensitivity_analysis.csv")
parameter_combos <- parameter_combos %>% 
  select(`Parameter code`, `Lower extreme`, `Lower median`, `Central value`, `Upper median`, `Upper extreme`) %>% 
  rename(ParameterCode = `Parameter code`, LExt = `Lower extreme`, LMed = `Lower median`, Cen = `Central value`, UMed = `Upper median`, UExt = `Upper extreme`) %>% 
  drop_na() %>% 
  mutate_at(vars(-1), as.numeric)

M <- morris(model = NULL, factors = nrow(parameter_combos), r = 50, design = list(type = "oat", levels = 5, grid.jump = 2), binf = parameter_combos$LExt, bsup = parameter_combos$UExt, scale = TRUE)

design <- M$X
outputs <- design

colnames(outputs) <- parameter_combos$ParameterCode


#unique(outputs[,1])

#write.csv(outputs,file='Data/Sensitivity analysis/Sensitivity_inputs.csv')



