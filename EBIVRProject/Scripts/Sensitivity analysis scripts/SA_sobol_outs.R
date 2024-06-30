# Cara Gallagher
# February 14th, 2023
# Energy Budget with Individual Variation project
# Sobol sensitivity analysis with sensitivity package
# Output analysis

#------------------------------------------------#
####  Packages: #### 
library("sensitivity")
library("tidyverse")
library(NatParksPalettes)
library(showtext)


font_add_google(name = "Montserrat", family = "Montserrat")
showtext_auto()

pal <- c("#BC4A53","#E4714E","#E4AA4E","#278192","#3F3F7B","#278192","#E4AA4E","#E4AA4E","#E4AA4E","#7FD7C4")

##################################################

source("Scripts/Sensitivity analysis scripts/SA_sobol_params.R")


inputs <- read_csv("Data/Sensitivity analysis/Sobol_sensitivity_inputs.csv")

inputs <- inputs %>% 
  #rename("run_number" = ...1)
  mutate(run_number = row_number())

results <- read_csv("Data/Sensitivity analysis/SA_sobol-5895.csv", skip = 6)
results <- read_csv("Data/Sensitivity analysis/SA_sobol_3-6505.csv", skip = 6)

results <- results %>%
  rename( run_number = "[run number]", step = "[step]") %>% 
  arrange(run_number)

results <- results %>% 
  select(-step)

# for (i in 2:8) {
# 
#   out <- pull(results, i) 
#   mean <- mean(out)
#   
#   for (ii in 1:nrow(results)) {
#     results[ii,i] <- results[ii,i] - mean
#     
#   }
#   
#   }



patterns <- c("SA.BMadult", "SA.BMemb", "SA.BMwean", "SA.age1stbirth", "SA.LpY", "SA.LSb", "SA.dens")

S$X <- inputs[,-1]

mainEffect <- tibble(pattern = as.character(), parameter = as.character(), mean = as.numeric(), se = as.numeric())
totalEffect <- tibble(pattern = as.character(), parameter = as.character(), mean = as.numeric(), se = as.numeric())

for (i in 1:length(patterns)) {
 # i <- 6
  sobol <- left_join(inputs, results, by = "run_number")
  
  sobol <- sobol %>% 
    rename(y = patterns[i]) %>% 
    pull(y)
  
  sob <- tell(S, sobol)

  print(sob)
  
  plot(sob)
  
  mE <- tibble(pattern = patterns[i], parameter = rownames(sob$S), mean = sob$S$original, se = sob$S$`std. error`)
  tE <- tibble(pattern = patterns[i], parameter = rownames(sob$T), mean = sob$T$original, se = sob$T$`std. error`)
  
  mainEffect <- rbind(
    mainEffect,
     mE
   )
  totalEffect <- rbind(
    totalEffect,
    tE
  )
}


#write.csv(mainEffect, "Data/Sensitivity analysis/Sobol_Outputs_MainEffect.csv")
#write.csv(totalEffect, "Data/Sensitivity analysis/Sobol_Outputs_TotalEffect.csv")



## for plots:

ins <- inputs

for (i in 2:11) {
  
  out <- pull(ins, i) 
  max <- max(out)
  min <- min(out)
  
  for (ii in 1:nrow(ins)) {
    ins[ii,i] <- (ins[ii,i] - min) / (max - min)
    
  }
  
}


outs <- left_join(ins, results, by = "run_number")

outs <- outs %>%  
  pivot_longer(-c(1:11), names_to = "output_name", values_to = "output_value") %>%  
  pivot_longer(-c(11:13), names_to = "param_name", values_to = "param_value")


outs <- outs %>% 
  mutate(
    param_name = factor(param_name, levels = unique(outs$param_name), labels = c(unique(outs$param_name)[1:5],"SL-max", unique(outs$param_name)[7:10])),
#    param_name = factor(param_name, levels = unique(outs$param_name), labels = c(unique(outs$param_name)[1:3],"growth-lm-prob-mid", unique(outs$param_name)[5:6], "ED-cpro", unique(outs$param_name)[8], "SL-max", "surv-mod-off")),
    output_name = factor(output_name, levels = unique(outs$output_name), labels = c("Adult body mass", "Neonate body mass", "Weaning body mass", "Age at first birth", "Litters per year", "Litter size at birth", "Population density"))
    )


SobolOutputPlots <- ggplot(outs, aes(x = param_value, y = output_value, col = param_name)) +
  geom_point(alpha = 0.05) +
  geom_smooth(method = "gam", size = 2, col = "black")+ #, se = FALSE) +
  facet_grid(rows = vars(output_name), cols = vars(param_name), scales = "free") +
  theme_classic()+ 
  scale_color_manual(values = pal) +
  labs(x = "Scaled parameter value", y = "Output value") +
  scale_x_continuous(breaks = c(0,0.5,1)) +
  theme(text = element_text(size = 50, color = "grey30", family = "Montserrat"),
        legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, family = "Montserrat"),
        strip.text.y.left = element_text(angle = 0, hjust = 1, family = "Montserrat"),
        strip.text = element_text(color = "grey30", size = 40, family = "Montserrat"),
        strip.background = element_blank())
SobolOutputPlots

#ggsave("Figures/SAOuts/SobolOuts.png", SobolOutputPlots, width = 15, height = 15)


