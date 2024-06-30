# Cara Gallagher
# February 19th, 2022
# Energy Budget with Individual Variation project
# Basal metabolic rate output verification

##################################################
# Packages:
library(tidyverse)
library(readxl)

##################################################
# Using data from: 
## Sadowska et al. 2015 - Proc B for first run
## Grosiak et al. 2020 - Front in Physio
## Górecki 1968 - Acta Theriologica

# Load data:
GrosiakDat <- read_excel("Data/Parameterization/BMR/GrosiakEtAl2020.xlsx", sheet = "data_RMR")

GrosiakDat <- GrosiakDat %>% 
  filter(T == "C", Ta == max(Ta))

GrosiakDat <- GrosiakDat %>% 
  select(MB0, RMR_W) %>% 
  drop_na() %>% 
  mutate(BodyMass = MB0 / 1000, # convert from g to kg
         BMR = RMR_W * 60 * 30, # convert from W to J per 30min
         source = "Grosiak et al. 2020") %>% 
  select(-MB0, -RMR_W)



SadowskaDat <- read_excel("Data/Parameterization/BMR/SadowskaEtAl2015.xls", sheet = "Data")

SadowskaDat <- SadowskaDat %>% 
  filter(Selection == "C" | Selection == "H") %>% 
  select(BodyMass, BMR) %>% 
  drop_na() %>% 
  mutate(BodyMass = BodyMass / 1000, # convert from g to kg
         BMR = (BMR * 20.1 * 1000)/(1000 * 2), # convert from ml O2 per hr to J per 30min
         source = "Sadowska et al. 2015")

# value for 1 L O2 = 20.1 kJ is from: 
# Schmidt-Nielsen  K.  Scaling: Why  Is  Animal  Size  So  Impor-
# tant? Cambridge University Press, Cambridge. 1984.

GoreckiDat <- tibble(
  mass = (c(17.5,17.5,19.5,20.0,20.0,21.0,22.0,24.0,24.6)/1000),
  BMRccm = c(2.21,3.20,2.52,2.70,2.84,2.86,3.45,3.10,2.33)
)

GoreckiDat <- GoreckiDat %>% 
  mutate(BMR = BMRccm * 20.1 * (mass * 1000) / 2) %>% 
  mutate(source = "Gorecki 1968") %>% 
  rename(BodyMass = mass) %>% 
  select(-BMRccm)

BMBMRDat <- bind_rows(GrosiakDat, SadowskaDat, GoreckiDat)

# could also include Peacock et al. 2004

# ggplot(BMBMRDat, aes(x = BodyMass, y = BMR, col = source)) + 
#   geom_point()


## Relationship in Speakman 2000: RMR (kJ day-1) = 10^0.7056 + body mass (g)^0.668
mass <- seq(0.015,0.038,0.001)

SpeakmanDat <- tibble(
  mass = seq(0.015,0.038,0.001),
  RMR = (exp(0.7056)*(mass*1000)^0.668) * 1000 / 48, 
  source = "Speakman 2000"
)

KleiberDat <- tibble(
  mass = seq(0.015,0.038,0.001),
  RMR = (293*(mass^0.75)) * 1000 / 48,
  source = "Kleiber 1961"
  )

KotejaWeinerDat <- tibble(
  mass = seq(0.015,0.038,0.001),
  RMR = (3.690*((mass*1000)^0.601)) * 1000 / 48,
  source = "Koteja & Weiner 1993")

curveDat <- bind_rows(SpeakmanDat, KleiberDat, KotejaWeinerDat)

# ggplot() + 
#   geom_point(data = BMBMRDat, aes(x = BodyMass, y = BMR, col = source)) +
#   geom_line(data = curveDat, aes(x = mass, y = RMR, col = source))


### model outputs ###

modOut <- read_csv("Data/Verification/BMR/EBIVPrototype_BasalMetabolicRate.csv", skip = 16)

modOut <- modOut %>% 
  select(x, y) %>% 
  rename(mass = x, BMR = y) %>% 
  filter(mass >= 0.015 & mass <= 0.038)

BMRVerPlot <- 
  ggplot() + 
  geom_point(data = BMBMRDat, aes(x = BodyMass, y = BMR, col = source)) +
  geom_line(data = curveDat, aes(x = mass, y = RMR, col = source), size = 1) +
  geom_smooth(data = modOut, aes(x = mass, y = BMR), col = "grey50", size = 1.5, se = FALSE) +
  labs(x = "Body mass [kg]", y = "Basal metabolic rate [J 30min-1]", col = NULL) +
  theme_classic()
BMRVerPlot




# for TEE
SpeakmanDat <- tibble(
  mass = seq(0.015,0.038,0.001)
) 

SpeakmanDat <- SpeakmanDat %>% 
  mutate(FMR = (exp(1.878)*(mass*1000)^0.659) * 1000 / 48)


KingDat <- tibble(
  mass = seq(0.015,0.038,0.001)
)

KingDat <- KingDat %>% 
  mutate(FMR = (753*(mass^0.67)) * 1000 / 48)