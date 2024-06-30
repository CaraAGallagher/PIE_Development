# Cara Gallagher
# June 8th, 2022
# Energy Budget with Individual Variation project  
# Prepping patterns for calibration  

##################################################
# Packages:
library(tidyverse)

##################################################

####  List of patterns to use: #### 

##    Relevant parameter key: 
#     Allocation parameters: 
#       Pregnancy k         (P)
#       Pregnancy c         
#       Lactation k         (L)
#       Lactation c         
#       Growth k            (G)
#       Growth c            
#     Mortality parameters:  
#       Survival k          (S)
#       Survival c          
#       Survival embryo     (SE)
#       Survival offspring  (SO)

##    Patterns & relevant parameters:  
# 1.  Fetal mass at birth                           - P, SE
# 2.  Birth mass by litter size                     - P, SE
# 3.  Total body mass by age                        - P, L, G, S, SE, SO 
# 4.  Lean mass by age                              - P, L, G, S, SE, SO
# 5.  Lactating mother mass by pup age              - L, G, SO
# 6.  Lactating mother food intake by pup age       - L, G, SO
# 7.  Total litter mass by pup age                  - L, G, SO
# 8.  Mother peak food intake by litter size        - L, SO
# 9.  Mother peak energy use by litter size         - L, SO
# 10. Mother peak milk transfer by litter size      - L, SO
# 11. Pup mass at weaning by litter size            - L, SO
# 12. Litter size at birth                          - G, SE
# 13. Litter size at weaning                        - L, SO
# 14. Probability of weaning by mother body mass    - L, SO 
# 15. Average / range of body fat %                 - All
# 16. Body fat % of living animals                  - All
# 17. Field metabolic rate by body mass             - All 

##    For evaluation:
# 18. State-dependent field metabolic rate          - All 
# 19. State-dependent food consumption              - All
# 20. Survival rates                                - S, SE, SO
# 21. Age-class structure                           - All
# 22. Local population densities                    - L, P, S, SE, SO 


####  Pattern prep: #### 

####  Pattern 1. Fetal mass at birth  #### 
# Qualitative
# Categorical fit (average +- standard deviation) to within 1.0 to 2.5 g 
  ## Koivula et al. 2003 Ecology 

p1 <- tibble(source = c("Empirical", "Empirical"), metric = c("max","min"), value = c(2.5,1.0))
  
ggplot(p1, aes(x = source, y = value, col = metric )) +
  geom_point() + 
  theme_classic()

####  Pattern 2. Birth mass by litter size  #### 
# Qualitative
# A negative relationship between birth mass and litter size
  ## Based on: Mappes and Koskela 2004 - Evolution, data pulled using Automeris WebPlotDigitizer


####  Pattern 3. Total body mass by age  #### 
# Quantitative
# RMSE from fit to total body mass pulled from: 
  ##  Fedyk  1974 - Acta Theriologica
  ##  Hansson 1991 - Acta Theriologica
  ##  Rudolf et al. 2017 - Experimental gerontology
  ##  Sawicka-Kapusta 1974 - Acta Theriologica
  ##  Balčiauskienė 2007 - Acta Zoologica Lituanica
  ##  Gębczyński 1975 - Acta Theriologica
  # data here: ~/Data/Parameterization/Growth/GrowthCurveData.xlsx
    ## LeanorTot == "Total mass"

p3 <- tibble(
  MeanA = 0.02483015,
  Meank = 0.03913599,
  MeanNo = 0.005569125,
  age = seq(1,620,1),
  totalMass = MeanA*(1 - (1 - (MeanNo / MeanA)^(1/3))*exp((-Meank*age)/3))^3
)

ggplot(p3, aes(x = age, y = totalMass)) +
  geom_line() + 
  theme_classic()


####  Pattern 4. Lean mass by age  #### 
# Quantitative
# RMSE from fit to total body mass pulled from: 
##  Fedyk  1974 - Acta Theriologica
##  Hansson 1991 - Acta Theriologica
##  Rudolf et al. 2017 - Experimental gerontology
##  Sawicka-Kapusta 1974 - Acta Theriologica
##  Balčiauskienė 2007 - Acta Zoologica Lituanica
##  Gębczyński 1975 - Acta Theriologica
# data here: ~/Data/Parameterization/Growth/GrowthCurveData.xlsx
  ## LeanorTot == "Lean mass"

p4 <- tibble(
  MeanA = 0.01930132,
  Meank = 0.04845531,
  MeanNo = 0.004688886,
  age = seq(1,620,1),
  leanMass = MeanA*(1 - (1 - (MeanNo / MeanA)^(1/3))*exp((-Meank*age)/3))^3
)

ggplot(p4, aes(x = age, y = leanMass)) +
  geom_line() + 
  theme_classic()

####  Pattern 5. Lactating mother mass by pup age  #### 
# Quantitative  
# Only use lactation day 15 or below to match with data
# Maximize percent falling into body mass +- SE range
  ## data from Sadowksa et al. 2016

p5 <- read_excel("Data/Verification/Reproduction/Sadowska2016/Fig1Dat.xlsx")

p5 <- p5 %>% 
  select(ageGest, Metric, bodyMass) %>% 
  pivot_wider(names_from = Metric, values_from = bodyMass)

ggplot(p5, aes(x = ageGest, ymin = Lo, ymax = Hi)) +
  geom_ribbon(fill ="grey70") +
  theme_classic() +
  ylim(5,35)

####  Pattern 6. Lactating mother food intake by pup age  #### 
# Quantitative  
# Maximize percent falling into body mass +- SE range
  ## data from Sadowksa et al. 2016

p6 <- read_excel("Data/Verification/Reproduction/Sadowska2016/Fig1Dat.xlsx")

p6 <- p6 %>% 
  select(ageGest, Metric, foodCons) %>% 
  pivot_wider(names_from = Metric, values_from = foodCons )

ggplot(p6, aes(x = ageGest, ymin = Lo, ymax = Hi)) +
  geom_ribbon(fill ="grey70") +
  theme_classic()

####  Pattern 7. Total litter mass by pup age  #### 
# Quantitative  
# Maximize percent falling into body mass +- SE range
  ## data from Sadowksa et al. 2016

p7 <- read_excel("Data/Verification/Reproduction/Sadowska2016/Fig1Dat.xlsx")
p7 <- p7 %>% 
  select(ageGest, Metric, LitterMass) %>% 
  pivot_wider(names_from = Metric, values_from = LitterMass )

ggplot(p7, aes(x = ageGest, ymin = Lo, ymax = Hi)) +
  geom_ribbon(fill ="grey70") +
  theme_classic()


####  Pattern 8. Mother peak food intake by litter size  #### 
# Quantitative  
# RMSE from predictions from a non-linear regression model (for a female of average body mass (29.1 g))
  ## data from Sadowksa et al. 2016

p8 <- read_excel("Data/Verification/Reproduction/Sadowska2016/Fig2Dat.xlsx")
p8 <- p8 %>% 
  filter(Metric == "FC") %>% 
  mutate(Value = Value * 17.80) %>% # converted to kJ to compare 
  mutate(p8.hi = Value * 1.25, p8.lo = Value * 0.75)
  
ggplot(p8, aes(x = litterSize, y = Value)) +
  geom_line() + 
  theme_classic()

####  Pattern 9. Mother peak energy use by litter size  #### 
# Quantitative  
# RMSE from predictions from a non-linear regression model (for a female of average body mass (29.1 g))
  ## data from Sadowksa et al. 2016

p9 <- read_excel("Data/Verification/Reproduction/Sadowska2016/Fig2Dat.xlsx")
p9 <- p9 %>% 
  filter(Metric == "ADMR") 

ggplot(p9, aes(x = litterSize, y = Value)) +
  geom_line() + 
  theme_classic()


####  Pattern 10. Mother peak milk transfer by litter size  #### 
# Quantitative  
# RMSE from predictions from a non-linear regression model (for a female of average body mass (29.1 g))
  ## data from Sadowksa et al. 2016

p10 <- read_excel("Data/Verification/Reproduction/Sadowska2016/Fig2Dat.xlsx")
p10 <- p10 %>% 
  filter(Metric == "MEO") 

ggplot(p10, aes(x = litterSize, y = Value)) +
  geom_line() + 
  theme_classic()


####  Pattern 11. Pup mass at weaning by litter size  #### 
# Qualitative
# A negative relationship between birth mass and litter size
# Based on data in: 
  ## Oksanen et al. 2001
  ## Sievert et al. 2021 - "each additional pup in a litter decreased the individual pup birth body mass by 0.07 g"
 
####  Pattern 12. Litter size at birth  #### 
# Qualitative  
# Categorical fit (average) to within 3.6 to 6.1 pups
# Based on data in:
  ## Bujalska & Ryszkowski 1966
  ## Koivula et al. 2003 Ecology
  ## Kaczmarski 1966 - Acta Theriologica
  ## Bujalska & Ryszkowski 1966
  ## Drożdż, 1963 & Brambell & Rowlands, 1936 ref in in Bujalska & Ryszkowski 1966
  ## Naumov, 1948 & Popov, 1960 ref in in Bujalska & Ryszkowski 1966
  ## Zejda, 1966
  ## Buchalczyk 1970 - Acta Theriologica
  ## Sadowska et al. 2016 - JEB

p12 <- c(4.9,5.27,5,4,4.08,6.1,4.5,3.6,5.4)
p12 <- tibble(source = c("Empirical", "Empirical"), metric = c("max","min"), value = range(p12))

ggplot(p12, aes(x = source, y = value, col = metric )) +
  geom_point() + 
  theme_classic()


####  Pattern 13. Litter size at weaning  #### 
# Qualitative  
# Categorical fit (average) to within 1.28 to 5.28 pups
# Based on data in:
  ## Kaczmarski 1966 - Acta Theriologica
  ## Koivula et al. 2003 Ecology
  ## Koskela 1998 Oecologia - control animals
  ## Koskela et al. 1998 - control animals
  ## Mappes et al. 1995
  ## Ołdakowski et al. 2015 - control animals
  ## Ołdakowski et al. 2012 - control animals

p13 <- c(4,1.8,1.56,1.28,5.17,2.34,5.28,4.8,4)
p13 <- tibble(source = c("Empirical", "Empirical"), metric = c("max","min"), value = range(p13))

ggplot(p13, aes(x = source, y = value, col = metric )) +
  geom_point() + 
  theme_classic()


####  Pattern 14. Probability of weaning by mother body mass  #### 
# Quantitative 
# RMSE from logistic function from: 
  ## Jonsson et al. 2002  

p14 <- tibble(mass = c(20,22,24,26,28,30,32,34,21,23,25,27,29,31,33),
              prob = c(0.09,0.16,0.30,0.49,0.69,0.86,0.96,0.95,0.11,0.22,0.40,0.60,0.79,0.92,0.97))

ggplot(p14, aes(x = mass, y = prob)) +
  geom_smooth(se = FALSE) +
  geom_point() + 
  theme_classic()
                      

####  Pattern 15. Average / range of body fat %  #### 
# Qualitative
# Categorical fit (average +- standard deviation) within 0.03 to 0.29 % body fat
# This is the range of values from: 
  ## Fedyk 1974
  ## Sawicka-Kapusta 1974
  ## Tidhar and Speakman 2007

p15 <- tibble(source = c("Empirical", "Empirical"), metric = c("max","min"), value = c(0.03,0.29))

ggplot(p15, aes(x = source, y = value, col = metric )) +
  geom_point() + 
  theme_classic()


####  Pattern 16. Body fat % of living animals  #### 
# Quantitative  
# Maximize percent of storage levels for animals which died below lower limit (3%) of empirical values for living animals
# Data from Sadowksa et al. 2016
  ## Fedyk 1974
  ## Sawicka-Kapusta 1974
  ## Tidhar and Speakman 2007

p16 <- tibble(source = c("Empirical"), metric = c("max"), value = c(0.03))

ggplot(p16, aes(x = source, y = value, col = metric )) +
  geom_point() + 
  theme_classic()

####  Pattern 17. Field metabolic rate by body mass #### 
# Quantitative  
# Not including lactating animals
# RMSE from average of two rodent curves from:
  ## King 1974
  ## Nagy, Girard, & Brown 1999

p17 <- tibble(
  mass = seq(0.01,0.03, 0.001),
  Nagy1987 = 10^(1.022) * (mass * 1000)^0.51,
  Speakman1999 = exp(1.878) * (mass * 1000)^0.66,   
  #King1974 = 753 * mass^0.67,                 
  Nagy1999 = 5.48 * (mass * 1000)^0.71)

p17 <- p17 %>% 
  mutate(avgFMR = (King1974 + Nagy1999) / 2)

ggplot(p17, aes(x = mass, y = avgFMR)) +
  geom_smooth(se = FALSE) +
  geom_point() + 
  theme_classic()


#### Evaluation:  ####

####  Pattern 18. State-dependent field metabolic rate  #### 
# Qualitative 
# Compare visually
# Data for nonreproducing and lactating animals from: 
  ## Peacock et al. 2004
  ## Sadowska et al. 2016
  ## Górecki 1968
  ## Tidhar et al. 2007
  ## Rutkowska et al. 2011

p18 <- tibble(
  mass = c(20.0, 23.8, 23.4, 25.7, 26.0, 26.8, 27.2, 27.3, 27.0, 27.1, 26.7, 28.7, 28.3, 28.4, 28.4, 28.8, 28.3, 27.9, 29.3, 30.0, 29.9, 29.7, 29.4, 29.2, 30.4, 30.8, 30.6, 30.0, 30.3, 31.2, 31.1, 34.3, 33.4, 32.5, 32.4, 33.3, 33.6, 33.1, 34.1, 36.0, 36.0, 35.4, 37.1),
  FMR = c(45.3, 57.1, 79.9, 62.2, 67.6, 66.2, 69.4, 72.6, 72.4, 76.6, 80.3, 94.7, 84.7, 76.7, 71.3, 71.8, 64.7, 61.9, 67.0, 70.7, 73.4, 76.7, 76.1, 78.3, 81.4, 80.6, 86.4, 88.1, 102.8, 95.8, 72.7, 56.2, 69.8, 85.9, 87.2, 90.7, 90.7, 103.3, 108.1, 121.6, 98.2, 89.9, 91.7))
p18 <- tibble(
  mean = c(88.8, 70.9, 57, 97, ((4.07 * 20.1 * 23.9 * 24) / 1000), ((4.29 * 20.1 * 23.7 * 24) / 1000), ((3.79 * 20.1 * 19.3 * 24) / 1000), ((3.65 * 20.1 * 19.8 * 24) / 1000), 66.44, mean(p18$FMR)),
  error = c(5.13, 3.95, 3, 1.7, ((0.62 * 20.1 * 23.9 * 24) / 1000), ((0.64 * 20.1 * 23.7 * 24) / 1000), ((0.36 * 20.1 * 19.3 * 24) / 1000), ((0.53 * 20.1 * 19.8 * 24) / 1000), 7.8, sd(p18$FMR)),
  state = c("Nonreproducing", "Nonreproducing", "Nonreproducing", "Lactating", "Nonreproducing", "Nonreproducing", "Nonreproducing", "Nonreproducing", "Nonreproducing", "Lactating"),
  source = c("Peacock et al. 2004", "Peacock et al. 2004", "Sadowska et al. 2016", "Sadowska et al. 2016", "Górecki 1968", "Górecki 1968", "Górecki 1968", "Górecki 1968", "Tidhar et al. 2007", "Rutkowska et al. 2011"))

ggplot(p18, aes(x = state, y = mean, group = mean, col = state, ymin = mean - error, ymax = mean + error)) + 
  geom_linerange(position = position_dodge(width = 0.75), size = 1) +
  geom_point(position = position_dodge(width = 0.75), size = 2) +
  scale_color_manual(values = pal, guide = "none") +
  scale_fill_manual(values = pal, guide = "none") +
  theme_classic() 
  
####  Pattern 19. State-dependent food consumption  #### 
# Qualitative 
# Compare visually
# Data for all states except pregnant AND lactating animals from: 
## Piątkowska & Weiner 1987
## Kaczmarski 1966
## Meese 1971
## Peacock and Speakman 2001
## Sadowska et al. 2016

p19 <- tibble(
  mean = c(3.26 * 19.6, 81.55, 97.06, 100.18, 111.90, 124.29, 170.01, 184.14, 66.78, 58.49, 105.6, 50.5, 49.0, 90.07, 270.56),
  error = c(0.23 * 19.6, 6.19, 14.23, 9.79, 15.70, 20.42, 36.21, 42.17, NA, NA, 2.64, 1.06, 1.91, 3.56, 5.34),
  source = c("Piątkowska & Weiner 1987", "Kaczmarski 1966", "Kaczmarski 1966", "Kaczmarski 1966", "Kaczmarski 1966", "Kaczmarski 1966", "Kaczmarski 1966", "Kaczmarski 1966", "Meese 1971", "Meese 1971", "Peacock et al. 2004", "Peacock and Speakman 2001", "Peacock and Speakman 2001", "Sadowska et al. 2016", "Sadowska et al. 2016"),
  state = c("Nonreproducing", "Nonreproducing", "Pregnant", "Pregnant", "Pregnant", "Lactating", "Lactating", "Lactating", "Nonreproducing", "Nonreproducing", "Nonreproducing", "Juvenile", "Juvenile", "Nonreproducing", "Lactating"))
p19 <- p19 %>% 
  mutate(state = fct_relevel(state, "Juvenile", "Nonreproducing", "Pregnant", "Lactating"))

ggplot(p19, aes(x = state, y = mean, group = mean, shape = source, col = state)) +
  geom_linerange(aes(ymin = mean - error, ymax = mean + error), position = position_dodge(width = 1), size = 1) +
  geom_point(position = position_dodge(width = 1), size = 2) +
  ylim(0,550) +
  theme_classic() +
  scale_color_discrete(guide = 'none') +
  labs( x = NULL, y = "Daily food consumption [kJ day-1]", shape = NULL) +
  theme(legend.position = c(0.3, 0.8))


####  Pattern 20. Survival rates  #### 
# Qualitative 
# Compare visually
# Data:
  ## 22% survival from one breeding season to the next in Koskela 1998
  ## 63.6% for control females from beginning of July to October 12-14th in Mappes, Koskela, & Ylonen 1995 
  ## 24% survived from the entire breeding season (from May to September or October) in Boratynski and Koteja 2009  
  ## 79% monthly survival probability between spring and fall for reproductively inactive & PUUV antibody negative voles in Tersago et al. 2012 
  ## 55% monthly survival probability between spring and fall for reproductively active & PUUV antibody negative voles in Tersago et al. 2012 
  ## 79% monthly survival probability between March - October for females in both grids in Newson 1963 - mean(c(0.81,0.77,0.72,0.89,0.83,0.83,0.68))
  ## 98.4% 15 day survival for female voles in Haapakoski and Ylönen 2010
  ## 96% 7 day survival for weanlings in mid-summer in Eccard et al. 2002 - Evolutionary ecology
  ## Also see Johnsen et al. 2016
    ## Remember that often animals leaving the study area or not recaught are considered dead


####  Pattern 21. Age-class structure  #### 




####  Pattern 22. Local population densities  #### 
# Quantitative
# Compare to ranges:
  ## Spring:: Lo: 8.8  Hi: 78.9
  ## Summer:: Lo: 9.2  Hi: 48.2 
  ## Autumn:: Lo: 14.8 Hi: 82.8
# Data:  
  ##  May   8.8 +- 5.8 ind/hec (12.3 +- 8.1  ind/1.4hec) - Mazurkiewicz 1994
  ##  July  27.5 +- 10.9 ind/hec (38.5 +- 15.2 ind/1.4hec) - Mazurkiewicz 1994
  ##  Aug   43.2 +- 11.8 ind/hec (60.5 +- 16.5 ind/1.4hec) - Mazurkiewicz 1994
  ##  Oct   33.2 +- 12.1 ind/hec (46.5 +- 17.0 ind/1.4hec) - Mazurkiewicz 1994
  ##  Spr   27.9 ind/hec - Reil et al. 2017 - mean of seasonal values across 4 sites and 4 years
  ##  Sum   48.2 ind/hec - Reil et al. 2017 - mean of seasonal values across 4 sites and 4 years
  ##  Aut   34.1 ind/hec - Reil et al. 2017 - mean of seasonal values across 4 sites and 4 years
  ##  Spr   78.9 ind/hec - Massay et al. 2008 - mean of seasonal values across 4 pop phases and 2 years
  ##  Aut   82.8 ind/hec - Massay et al. 2008 - mean of seasonal values across 4 pop phases and 2 years
  ##  SumE  9.2  ind/hec - Koivula et al 2003 - mean of seasonal values across 3 years (minimum number known alive)
  ##  SumM  17.8 ind/hec - Koivula et al 2003 - mean of seasonal values across 3 years (minimum number known alive)
  ##  SumL  28.4 ind/hec - Koivula et al 2003 - mean of seasonal values across 3 years (minimum number known alive)
  ##  Aut   14.8 ind/hec - Koivula et al 2003 - mean of seasonal values across 3 years (minimum number known alive)
spring <- c(8.8, 27.9, 78.9) / 2
summer <- c(27.5, 43.2, 48.2, 9.2, 17.8, 28.4) / 2 
autumn <- c(33.2, 34.1, 82.8, 14.8) / 2

quantile(c(spring, summer, autumn))
median(c(spring, summer, autumn))

mean(c(spring, summer, autumn))
sd(c(spring, summer, autumn))

densByMonth <- tibble(
  month = c(5, 7, 8, 10, 4, 7, 10, 4, 10, 6, 7, 8, 10),
  dens = c(8.8, 27.5, 43.5, 33.2, 27.9, 48.2, 34.1, 78.9, 82.8, 9.2, 17.8, 28.4, 14.8) / 2)

ggplot(densByMonth, aes(x = month, y = dens)) + 
  geom_hline(yintercept = median(densByMonth$dens), color = "grey40", linetype = "dashed", size = 1.5) +
  geom_hline(yintercept = mean(densByMonth$dens), color = "grey60", linetype = "dashed", size = 1.5) +
  geom_point(col = "darkgreen", size = 2) +
  theme_classic() +
  ylim(0,45)

####  Pattern 23. Home range size  #### 
# Quantitative
# Compare to range:
  ## 11.3 - 32.2 m radius from home range center - assuming circular home ranges
# Data:
## Min:  
  ## 500 m2 in Mazurkiewicz 1983, Radda 1968, Eccard and Ylönen 2002, 2007 - refs in Haupt et al 2010
  ## 297.8 m2 in Mappes et al. 1995
  ## 418.6 m2 in Schirmer et al. 2019
  ## 737 m2 in Bujalska and Grüm 1987 - immature females 
## Max:  
  ## 2000 m2 in Mazurkiewicz 1983, Radda 1968, Eccard and Ylönen 2002, 2007 - refs in Haupt et al 2010
  ## 2103.5 m2 in Mappes et al. 1995
  ## 6471.9 m2 in Schirmer et al. 2019
  ## 897 m2 in Bujalska and Grüm 1987 - mature females
## Home range diameter: 
  ## 20.93 m in Carslake 2003 - Females 

# Calculating radius 
HRmin <- c(500, 297.8,418.6)
HRmax <- c(2000, 2103.5,6471.9)

mean(sqrt(HRmin / pi))
mean(sqrt(HRmax / pi))


