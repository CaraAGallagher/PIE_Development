# Cara Gallagher
# November 15th, 2021
# Energy Budget with Individual Variation project
# Modelling growth

##################################################
# Packages:
library(tidyverse)
library(readxl)
library(patchwork)
library(nlme)
library(showtext)

font_add_google(name = "Montserrat", family = "Montserrat")
showtext_auto()

##################################################
# Using data from:
## Fedyk  1974 - Acta Theriologica
## Hansson 1991 - Acta Theriologica
## Rudolf et al. 2017 - Experimental gerontology
## Sawicka-Kapusta 1974 - Acta Theriologica
## Balčiauskienė 2007 - Acta Zoologica Lituanica
## Gębczyński 1975 - Acta Theriologica

## Use of VB curves:
# Han, Q., Zhang, M., Guo, C., Zhou, X., Li, B. and Wang, Y., 2017. 
# Density effect on postnatal growth of laboratory-bred Yangtze vole 
# (Microtus fortis calamorum). Pakistan Journal of Zoology, 49(6).

##### Growth functions #####

fit.bertalanffy <- function(age, data) {                                 # create von bertalanffy fitting function
  d <- data.frame(age, data) 
  # pick starting values
  
  starting.values <- c(                                                  # starting values
    A=A1,                                                                # asymptotic starting mass set to max in dataset
    k=k1,
    Mo = Mo1)                                           
  # print("Starting Values for Optimization: ")
  # print(starting.values)
  
  formula.bertalanffy <- "data~A*(1 - (1 - (Mo / A)^(1/3))* exp((-k*age)/3))^3"               # fit using standard VB curve
  nls(formula.bertalanffy, d, starting.values)                           # use nls to fit curve using data and specified starting values
}

fit.gompertz <- function(age, data) {                                 # create gompertz fitting function
  d <- data.frame(age, data) 
  # pick starting values
  
  starting.values <- c(                                               # starting values
    A=A1,                                    # asymptotic starting mass set to max in dataset
    k=k1,
    No = No1)                                           
  # print("Starting Values for Optimization: ")
  # print(starting.values)
  
  formula.gompertz <- "data~A*exp(log(A/No)*exp((-k)*age))"                # fit using standard gompertz curve
  nls(formula.gompertz, d, starting.values)                           # use nls to fit curve using data and specified starting values
}


fit.logistic <- function(age, data) {                                 # create gompertz fitting function
  d <- data.frame(age, data) 
  # pick starting values
  
  starting.values <- c(                                               # starting values
    A=A1,                                    # asymptotic starting mass set to max in dataset
    k=k1,
    No = No1)                                           
  # print("Starting Values for Optimization: ")
  # print(starting.values)
  
  fit.log <- data ~ SSlogis(age, A, k, No)         
  nls(fit.log, d)                           # use nls to fit curve using data and specified starting values
}

##### Load data #####

GrowthDat <- read_excel("Data/Parameterization/Growth/GrowthCurveData.xlsx")

# estimate fat percent for missing values using regressions from Sawicka-Kapusta 1974
# then calculate lean mass
 GrowthDat <-  GrowthDat %>%
    mutate(InfLM = ifelse((is.na(Perc_fat) & Environment == "Lab"), 
                            "Y",
                            "N")) %>% 
     mutate(Perc_fat = ifelse((is.na(Perc_fat) & Environment == "Lab"), 
                              (0.0258*(Mass_kg * 1000)^1.6571)/(Mass_kg * 1000), 
                              Perc_fat)) %>% 
     mutate(Mass_kg_lm = Mass_kg * (1-Perc_fat))

GrowthDatMean <- GrowthDat %>% 
  filter(Only_max == "N") 

# ggplot(GrowthDatMean, aes(x = Age_days, y = Perc_fat)) +
#   geom_point(aes(col = Environment)) +
#   geom_smooth(method = "loess", span = 0.5) +
#   theme_classic()


# create empty output dataframe to save to
outputsMean <- data.frame(                        
  A=numeric(),
  Asd=numeric(),
  k=numeric(),
  ksd=numeric(),
  Mo=numeric(),
  Mosd=numeric(),
  AIC=numeric())

pots <- seq(0.000001, 0.020001, 0.0001)

for (i in pots) {
  tryCatch({
    k1 = i
    A1 =  max(GrowthDatMean$Mass_kg_lm) 
    to1 = max(GrowthDatMean$Age_days)
    Mo1 = min(GrowthDatMean$Mass_kg_lm)
    fitMean <- fit.bertalanffy(GrowthDatMean$Age_days, GrowthDatMean$Mass_kg_lm)  # run formula
   # print(i)
  }, error=function(e){})
}

outputsMean[1,] <- c(A=summary(fitMean)$coefficients[1,1], Asd=summary(fitMean)$coefficients[1,2], k=summary(fitMean)$coefficients[2,1], ksd=summary(fitMean)$coefficients[2,2],  No=summary(fitMean)$coefficients[3,1], Nosd=summary(fitMean)$coefficients[3,2], AIC=AIC(fitMean))

MeanA <- outputsMean[1,1]
Meank <- outputsMean[1,3]
MeanNo <- outputsMean[1,5]
MeanAsd <- outputsMean[1,2]
Meanksd <- outputsMean[1,4]
MeanNosd <- outputsMean[1,6]

age <- seq(1,620,1)
Mean <- MeanA*(1 - (1 - (MeanNo / MeanA)^(1/3))*exp((-Meank*age)/3))^3

# Load data:

GrowthDatMax <- GrowthDat %>%  
  filter(Only_max  == "Y") 

GrowthDatMax <- GrowthDatMax %>%
  bind_rows(GrowthDat[which(GrowthDat$Age_days == min(GrowthDat$Age_days)),])

# create empty output dataframe to save to
outputsMax <- data.frame(                        
  A=numeric(),
  Asd=numeric(),
  k=numeric(),
  ksd=numeric(),
  Mo=numeric(),
  Mosd=numeric(),
  AIC=numeric())

for (i in pots) {
  tryCatch({
    k1 = i
    A1 =  max(GrowthDatMax$Mass_kg_lm) 
    to1 = max(GrowthDatMax$Age_days)
    Mo1 = min(GrowthDatMax$Mass_kg_lm)
    fitMax <- fit.bertalanffy(GrowthDatMax$Age_days, GrowthDatMax$Mass_kg_lm)  # run formula
   # print(i)
  }, error=function(e){})
}

outputsMax[1,] <- c(A=summary(fitMax)$coefficients[1,1], Asd=summary(fitMax)$coefficients[1,2], k=summary(fitMax)$coefficients[2,1], ksd=summary(fitMax)$coefficients[2,2],  No=summary(fitMax)$coefficients[3,1], Nosd=summary(fitMax)$coefficients[3,2], AIC=AIC(fitMax))

MaxA <- outputsMax[1,1]
Maxk <- outputsMax[1,3]
MaxNo <- outputsMax[1,5]
MaxAsd <- outputsMax[1,2]
Maxksd <- outputsMax[1,4]
MaxNosd <- outputsMax[1,6]

Max <- MaxA*(1 - (1 - (MaxNo / MaxA)^(1/3))*exp((-Maxk*age)/3))^3

Mid <- (mean(c(MeanA, MaxA))) *(1 - (1 - (mean(c(MeanNo, MaxNo)) / mean(c(MeanA, MaxA)))^(1/3))*exp((-mean(c(Meank, Maxk))*age)/3))^3

outDat <- tibble(
  age,
  mod1 = Mean,
  mod2 = Max,
)

outDat <- outDat %>% 
  gather(key = "mod", value = "value", -age)

# pal <- lacroix_palette("Pamplemousse", type = "discrete")
# 
# outplot <- ggplot() +
#   geom_point(data = GrowthDat, aes(x = Age_days, y = Mass_kg_lm, col = Only_max)) +
#   geom_line(data = outDat, aes(x = age, y = value, col = mod), size = 1.5)+
#   scale_color_manual(values = c(pal[2],pal[1],pal[2],pal[1])) +
#   labs(x = "Age [days]", y = "Lean mass [kg]") +
#   theme_classic() + 
#   theme(legend.position = "none") +
#   annotate(geom="text", x=500, y=0.0075, label="Maximum growth", color=pal[1], size=8)+
#   annotate(geom="text", x=500, y=0.005, label="Average growth", color=pal[2], size=8) +
#   theme(text = element_text(size = 20, color = "grey30"),
#         axis.line = element_line(color = "grey30"),
#         legend.position = c(0.17, 0.9),
#         legend.background = element_blank())
# outplot



pal <- c("#839136","#AABC4A")

outplot <- ggplot() +
  geom_point(data = GrowthDat, aes(x = Age_days, y = Mass_kg_lm, col = Only_max, shape = InfLM), size = 3, alpha = 0.75) +
  geom_line(data = outDat, aes(x = age, y = value, col = mod), size = 2)+
  scale_color_manual(values = c(pal[1],pal[2],"grey60","grey20")) +
  labs(x = "Age [days]", y = "Lean mass [kg]") +
  theme_classic() + 
  theme(legend.position = "none") +
  annotate(geom="text", x=500, y=0.0085, label="Maximum", color=pal[2], size=20, family = "Montserrat")+
  annotate(geom="text", x=500, y=0.006, label="Average", color=pal[1], size=20, family = "Montserrat") +
  theme(text = element_text(size = 50, color = "grey30", family = "Montserrat"),
        axis.line = element_line(color = "grey30"),
        legend.position = "none")
outplot

#ggsave("Figures/ODDplots/Growth.png", outplot, height = 5.5, width = 7)

