# Cara Gallagher
# November 5th, 2021
# Energy Budget with Individual Variation project
# Basal metabolic rate parameter estimation

##################################################
# Packages:
library(tidyverse)
library(nlme)
library(readxl)
library(patchwork)
library(showtext)


font_add_google(name = "Montserrat", family = "Montserrat")
showtext_auto()

##################################################
# Using data from: 
## Sadowska et al. 2015 Proc B for first run
## Grosiak et al. 2020 - Front in Physio
## Górecki 1968 - Acta Theriologica
# Can include others later, such as:
## Sadowska et al. 2007 - Evolution
## Labocha et al. 2004 - Proc B
## Boratyński & Paweł Koteja 2009 - Funct Eco
## Stawski et al. 2017 - Front in Physio
## Górecki,  1968; Hart,  1971;  Grodziński,  1985  refs in Piątkowska & Weiner 1987


## can compare to relationship in Speakman 2000 which was: RMR (kJ day-1) = 10^0.7056 + body mass (g)^0.668

# Load data:
GrosiakDat <- read_excel("Data/Parameterization/BMR/GrosiakEtAl2020.xlsx", sheet = "data_RMR")

GrosiakDat <- GrosiakDat %>% 
  filter(T == "C", Ta == max(Ta))

GrosiakDat <- GrosiakDat %>% 
  select(MB0, RMR_W) %>% 
  drop_na() %>% 
  mutate(BodyMass = MB0 / 1000, # convert from g to kg
         BMR = RMR_W * 60 * 30, # convert from W to J per 30min
         source = "Grosiak") %>% 
  select(-MB0, -RMR_W)



SadowskaDat <- read_excel("Data/Parameterization/BMR/SadowskaEtAl2015.xls", sheet = "Data")

SadowskaDat <- SadowskaDat %>% 
  filter(Selection == "C" | Selection == "H") %>% 
  select(BodyMass, BMR) %>% 
  drop_na() %>% 
  mutate(BodyMass = BodyMass / 1000, # convert from g to kg
         BMR = (BMR * 20.1 * 1000)/(1000 * 2), # convert from ml O2 per hr to J per 30min
         source = "Sadowska")

# value for 1 L O2 = 20.1 kJ is from: 
# Schmidt-Nielsen  K.  Scaling: Why  Is  Animal  Size  So  Impor-
# tant? Cambridge University Press, Cambridge. 1984.

GoreckiDat <- tibble(
  mass = (c(17.5,17.5,19.5,20.0,20.0,21.0,22.0,24.0,24.6)/1000),
  BMRccm = c(2.21,3.20,2.52,2.70,2.84,2.86,3.45,3.10,2.33)
)

GoreckiDat <- GoreckiDat %>% 
  mutate(BMR = BMRccm * 20.1 * (mass * 1000) / 2) %>% 
  mutate(source = "Gorecki") %>% 
  rename(BodyMass = mass) %>% 
  select(-BMRccm)

BMBMRDat <- bind_rows(GrosiakDat, SadowskaDat, GoreckiDat)

# estimate fat percent for missing values using regressions from Sawicka-Kapusta 1974
# then calculate lean mass
BMBMRDat <-  BMBMRDat %>%
  mutate(Perc_fat = (0.0258*(BodyMass * 1000)^1.6571)/(BodyMass * 1000)) %>%
  mutate(BodyMass = BodyMass * (1-Perc_fat))


ggplot(BMBMRDat, aes(x = BodyMass, y = BMR, col = source)) + 
  geom_point()

# Testing linear model fits:

lm_1 <- lm(log10(BMR) ~ log10(BodyMass), data = BMBMRDat)
#glm_gamma <- glm(log10(BMR) ~ log10(BodyMass), family = Gamma(link = "identity"), data = BMBMRDat, start=c(0.5,0.5))
gls_1 <- gls(log10(BMR) ~ log10(BodyMass), data = BMBMRDat)
gls_2 <- gls(log10(BMR) ~ log10(BodyMass), weights=varPower(form = ~ log10(BodyMass)), data = BMBMRDat)
gls_3 <- gls(log10(BMR) ~ log10(BodyMass), weights=varExp(), data = BMBMRDat)

AIC(lm_1)
#AIC(glm_gamma)
AIC(gls_1)
AIC(gls_2)
AIC(gls_3)

#summary(lm_1)
#summary(glm_gamma)
#summary(gls_1)
#summary(gls_2)
#summary(gls_3)

# basic gls gives the best fit
coef(gls_1)
conf <- confint(gls_1, level = 0.50)


# check gls model outputs

BMBMRDatTest <- BMBMRDat %>% 
  mutate(testBMRM = (10^coef(gls_1)[1]) * BodyMass^(coef(gls_1)[2])) %>% 
  mutate(testBMRH = (10^conf[1,2]) * BodyMass^(conf[2,2])) %>% 
  mutate(testBMRL = (10^conf[1,1]) * BodyMass^(conf[2,1]))



plotOut <- 
  ggplot(BMBMRDatTest, aes(x = BodyMass)) + 
  geom_point(aes(y = BMR, col = source, shape = source), size = 3) +
  geom_line(aes(y = testBMRM), col = "#BC4A53", size = 2)+
  scale_colour_grey(name = NULL, labels = c("Gorecki et al. 2020", "Grosiak et al. 1968", "Sadowska et al. 2015")) +
  scale_shape(name = NULL, labels = c("Gorecki et al. 2020", "Grosiak et al. 1968", "Sadowska et al. 2015")) +
  labs(y = "Basal metabolic rate [J 30min-1]", x = "Body mass [kg]") + 
  theme_classic() +
  theme(text = element_text(size = 40, color = "grey30", family = "Montserrat"),
        axis.line = element_line(color = "grey30"),
        legend.position = c(0.27, 0.9),
        legend.background = element_blank())
plotOut

#ggsave("Figures/ODDplots/BMR.png", plotOut, width = 6)

# random pull
Bo <- runif(1000, min = 10^conf[1,1], max = 10^conf[1,2])
gamma <- runif(1000, min = conf[2,1], max = conf[2,2])
BodyMass <- runif(1000, min = 0.015, max = 0.036)
BMRTest <- Bo * BodyMass^gamma
TestDf <- tibble(BodyMass, BMRTest)
TestDfwDat <- bind_rows(TestDf, BMBMRDat) 

ggplot(TestDfwDat, aes(x = BodyMass)) +
  geom_point(aes(y = BMRTest), col = "grey50", alpha = 0.5) +
  geom_point(aes(y = BMR, col = source), size = 2) +
  theme_classic()




#### variation in BMR #### 

varBMR <- BMBMRDat

varBMR <- varBMR %>% 
  mutate(predBMR = 6053.1 * BodyMass ^ 0.64) %>% 
  mutate(diff = BMR - predBMR) %>% 
  mutate(diffPerc = (diff/predBMR) * 100 ) 

varP1 <- ggplot(varBMR, aes(x = BodyMass, y = BMR)) +
  geom_linerange(aes(ymax = BMR, ymin = predBMR, col = source), alpha = .2) + 
  geom_line(aes(y = predBMR)) +
  geom_point(aes(col = source)) +
  labs(col = "Source:", x = "Body mass [kg]", y = "Basal metabolic rate [J 30 min-1]") +
  theme_classic() +
  theme(legend.position = c(0.2,0.8))
varP1

ggplot(varBMR, aes(x = diff)) +
  geom_histogram(aes(y = ..density..), fill = "grey50", alpha = 0.5, bins = 100)+
  geom_density(size = 2, color = "darkgreen") +
  theme_classic()

varP3 <- ggplot(varBMR, aes(x = diffPerc)) +
  geom_histogram(aes(y = ..density..), fill = "grey50", alpha = 0.5, bins = 100)+
  geom_density(size = 2, color = "grey30") +
  labs(col = NULL, x = "Difference from base curve [%]", y = "Density") +
  theme_classic()
varP3

ggplot(varBMR, aes(x = BodyMass, y = diff)) +
  geom_point() +
  geom_smooth(method = "lm")

varP2 <- ggplot(varBMR, aes(x = BodyMass, y = diffPerc)) +
  geom_linerange(aes(ymax = diffPerc, ymin = 0, col = source), alpha = .2) +  
  geom_point() +
  geom_point(aes(col = source)) +
  geom_smooth(method = "lm", se = FALSE, col = "grey50") +
  theme_classic() +
  labs(col = NULL, x = "Body mass [kg]", y = "Difference from base curve [%]") +
  theme(legend.position = "none")
varP2

varP <- varP1 | (varP2 / varP3)


fit <- lm(BodyMass~diffPerc, data=varBMR)  # Fit the model
summary(fit) 
# par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
# plot(fit)  # Plot the model information
# par(mfrow = c(1, 1))  # Return plotting panel to 1 section
# 
# ggplot(lm(BodyMass~diffPerc, data=varBMR)) + 
#   geom_point(aes(x=.fitted, y=.resid))
