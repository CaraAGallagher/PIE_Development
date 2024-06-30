# Cara Gallagher
# February 22nd, 2022
# Energy Budget with Individual Variation project
# Growth output verification

##################################################
# Packages:
library(tidyverse)
library(readxl)
library(showtext)


font_add_google(name = "Montserrat", family = "Montserrat")
showtext_auto()


##################################################

### Empirical relationships

# Using data from:
## Fedyk  1974 - Acta Theriologica
## Hansson 1991 - Acta Theriologica
## Rudolf et al. 2017 - Experimental gerontology
## Sawicka-Kapusta 1974 - Acta Theriologica
## Balčiauskienė 2007 - Acta Zoologica Lituanica
## Gębczyński 1975 - Acta Theriologica


GrowthDat <- read_excel("Data/Parameterization/Growth/GrowthCurveData.xlsx")

# estimate fat percent for missing values using regressions from Sawicka-Kapusta 1974
# then calculate lean mass
GrowthDat <-  GrowthDat %>%
  mutate(Perc_fat = ifelse((is.na(Perc_fat) & Environment == "Lab"), 
                           (0.0258*(Mass_kg * 1000)^1.6571)/(Mass_kg * 1000), 
                           Perc_fat)) %>% 
  mutate(Mass_kg_lm = Mass_kg * (1-Perc_fat)) %>% 
  select(Age_days, Mass_kg_lm, Mass_kg) %>% 
  gather(key = "LeanorTot", value = "Mass", -Age_days) %>% 
  mutate(source = "Emp", LeanorTot = ifelse( LeanorTot == "Mass_kg", "Total mass", "Lean mass") )


pal <- colorRampPalette(c("#BC4A53","#3F3F7B","#278192","#00B089"))


ggplot() +
  geom_point(data = GrowthDat, aes(x = Age_days, y = Mass, col = LeanorTot)) +
  scale_color_manual(values = pal) +
  labs(x = "Age (days)", y = "Lean mass (kg)") +
  theme_classic() + 
  theme(legend.position = "none")



### model outputs ###

modOut <- read_csv("Data/Verification/Growth/EBIVPrototype_BodyMass.csv", skip = 17)

modOut1 <- modOut %>% 
  select(x...1, y...2) %>% 
  rename(Age_days = x...1, Mass = y...2) %>% 
  mutate(LeanorTot = "Total mass")

modOut2 <- modOut %>% 
  select(x...5, y...6) %>% 
  rename(Age_days = x...5, Mass = y...6) %>% 
  mutate(LeanorTot = "Lean mass")

modOut <- bind_rows(modOut1, modOut2) %>% 
  mutate(source = "Mod")

# ggplot() +
#   geom_smooth(data = modOut, aes(x = Age_days, y = Mass, col = LeanorTot)) +
#   scale_color_manual(values = c(pal[2],pal[1],pal[2],pal[1])) +
#   labs(x = "Age (days)", y = "Lean mass (kg)") +
#   theme_classic() + 
#   theme(legend.position = "none")

allDat <- bind_rows(GrowthDat, modOut)

growthVerPlot <- ggplot() +
  geom_point(data = GrowthDat, aes(x = Age_days, y = Mass, col = LeanorTot)) +
  geom_smooth(data = modOut, aes(x = Age_days, y = Mass, col = LeanorTot), se = FALSE, method = "gam", size = 1.5) +
  scale_color_manual(values = pal(4)) +
  labs(x = "Age [days]", y = "Mass [kg]") +
  theme_classic() + 
  theme(legend.position = "none") +
  facet_grid(cols = vars(LeanorTot))
growthVerPlot

##################################################
# Growth rate 


modOutGR <- read_csv("Data/Verification/Growth/EBIVPrototype_GrowthRateByAge.csv", skip = 16)

modOutGR <- modOutGR %>% 
  select(x, y) %>% 
  rename(Mass = x, growthCosts = y) 


pal <- c("#BC4A53","#3F3F7B","#278192","#00B089")

ggplot(modOutGR, aes(x = Mass, y = growthCosts)) +
  geom_point(col = "grey50", alpha = 0.05) +
  geom_smooth(col = pal[4], method = "gam", size = 2, se = FALSE) + 
  labs(x = "Mass [kg]", y = "Growth costs [J 30min-1]") +
  theme_classic() 



##################################################
# Calculation tests

percAlloCheck <- tibble(
  growthProbConst = 5,
  growthProbMid = 0.4,
  SLmax = 0.398,
  SL = seq(0.00,0.4, 0.01)
#  SL = 0.257
)

percAlloCheck <- percAlloCheck %>% 
  mutate(percAlloGrowth  = 1 / (1 + exp(-growthProbConst * ( SL / SLmax - growthProbMid ))))

ggplot(percAlloCheck, aes(x = SL, y = percAlloGrowth)) + 
  geom_line() +
  ylim(0,1)

growthCheck <- tibble(
  mLeanK = 0.0964, 
  mLeanInf = 0.0264, 
  percAlloGrowth = 0.921,
 # percAlloGrowth = seq(0.00, 1, 0.01),
  leanMass = 0.02
  #leanMass = seq(0.001,0.03, 0.001)
 
)

growthCheck <- growthCheck %>% 
  mutate(growth = (( mLeanK * percAlloGrowth ) / 48 )*((( mLeanInf * percAlloGrowth ) ^( 1 / 3 )) * ( leanMass ^ ( 2 / 3 )) - leanMass )) 

ggplot(growthCheck, aes(x = leanMass, y = growth)) + 
  geom_line()

ggplot(growthCheck, aes(x = percAlloGrowth, y = growth)) + 
  geom_line()


mGrowth <- 0.00000249 * 5373096.042733403 / 0.444



##################################################
# Lean mass by age per storage level

source("Scripts/Input scripts/EBIV_growth_models.R")

modOutLM <- read_csv("../../MetabolicVariationLargeOuts/EBIV_LeanMassStorageLevels.csv", skip = 6)

modOutLM <- modOutLM %>% 
  select('SL-val', 'mean [age] of turtles', 'mean [lean-mass] of turtles') %>% 
  rename(SLVal = 'SL-val', age = 'mean [age] of turtles', leanMass = 'mean [lean-mass] of turtles') %>% 
  group_by(SLVal, age) %>% 
  summarise(meanLM = mean(as.numeric(leanMass)), sdLM = sd(as.numeric(leanMass))) %>% 
  mutate(age = as.numeric(age)) %>% 
  mutate(SLVal = SLVal * 100) %>% 
  mutate(SLVal = factor(SLVal, levels = c("2.5","5","7.5","10","15","20","30")))

pal <- colorRampPalette(c("#465300","#778C04","#AABC4A","#DBE3B3"))


LMSLplot <- ggplot() +
  geom_ribbon(data = modOutLM, aes(x = age, ymin = meanLM - sdLM, ymax = meanLM + sdLM, fill = SLVal), alpha = 0.2) + 
  geom_line(data = outDat, aes(x = age, y = value, linetype = mod), size = 1.5, col = "grey30") +
  geom_line(data = modOutLM, aes(x = age, y = meanLM, col = as.factor(SLVal)), size = 1) +
  scale_fill_manual(values = pal(7), name = "Storage level [%]") +
  scale_color_manual(values = pal(7), name = "Storage level [%]") +
  scale_linetype_manual(values = c("mod1"= "solid", "mod2" = "dotted"), labels = c("Average", "Maximum")) +
  xlim(17,630)+
  ylim(0.005,0.0275)+
  labs(x = "Age [days]", y = "Lean mass [kg]", linetype="Empirical curves") +
  theme_classic() +
  theme(text = element_text(size = 50, color = "grey30", family = "Montserrat"),
        axis.line = element_line(color = "grey30"),
        legend.key.width = unit(1,"cm"))
LMSLplot

#ggsave("Figures/Verification/LMSLPlotOut.png", LMSLplot , width = 10, height = 6)











