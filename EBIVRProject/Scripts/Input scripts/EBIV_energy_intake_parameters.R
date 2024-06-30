# Cara Gallagher
# November 18th, 2021
# Energy Budget with Individual Variation project
# Energy intake rate parameters

##################################################
# Packages:
library(tidyverse)
library(showtext)


font_add_google(name = "Montserrat", family = "Montserrat")
showtext_auto()

##################################################
# gathering lit values

## maximum total energy intake
# 3.26 +- 0.23 kJ g-1 day-1 - maximum total energy consumption in Piątkowska & Weiner 1987 -  Acta Theriologica
# 2.56 +- 0.21 kJ g-1 day-1 - maximum total energy consumption in Piątkowska & Weiner 1987 -  Acta Theriologica
# 2.95 kJ g-1 day-1 - average assimilated energy for non-growing, non-reproducing voles (25 g) in Kaczmarski 1966  
# 4.07 kJ g-1 day-1 - maximum average assimilated energy for pregnant voles (25 g) in Kaczmarski 1966
# 6.95 kJ g-1 day-1 - maximum average assimilated energy for lactating voles (25 g) in Kaczmarski 1966

## energy assimilation rate
# 82.8 +- 7.2 % - maximum in Piątkowska & Weiner 1987 -  Acta Theriologica
PWAE <- 82.8
# 88.5 +- 2.4 % - SHOULD BE MODIFIED: Digestibility coefficient for non-reproductive females in Kaczmarski 1966 - Acta Theriologica
  # overestimates by 2 - 4% for urinary losses per the authors
KaczmarskiAE <- 88.5 * 0.97 # adjusted by assumed three percent overestimation
# For specific food types - Meese 1971
MeeseAE <- c(90.16,91.35,84.55,81.42,79.01,75.75,88.98,84.98,86.09,72.39)
# Peacock and Speakman 2001
PSHighFatAE <- 86.4
PSStandardAE <- 81.3
AEmean <- mean(c(PWAE,KaczmarskiAE,MeeseAE,PSHighFatAE,PSStandardAE)) # mean of all sources
# Digestive efficiency seems to change with day length - see Krol and Speakman 2007

## maximum energy assimilation
# 2.56 +- 0.21 kJ g-1 day-1 - in Piątkowska & Weiner 1987 -  Acta Theriologica
# 1.26 +- kJ g-1 day-1 - for pregnant (converted) in Kaczmarski 1966 - Acta Theriologica


## average energy assimilation
# 2.95 +- 0.22 kJ g-1 day-1 - non-reproductive females (converted as 17.25 * 4.184 / 24.5g) in Kaczmarski 1966 - Acta Theriologica


## changes with pregnancy
# +117.3, 121.8, & 135.25% over baseline for days 0-6, 7-12, & 13-18, respectively in Kaczmarski 1966 - Acta Theriologica


## changes with lactation
# +150.22, 205.48, & 222.57% over baseline for days 0-6, 7-12, & 13-18, respectively in Kaczmarski 1966 - Acta Theriologica


## total energy expended on preg and lact of one litter
# 364 Kcal (74.72 preg, 289.15 lact) in Kaczmarski 1966 - Acta Theriologica


## functional response curve
# from Lundberg 1988 - The Journal of Animal Ecology
# using the 'disc-equation'
# changed to half hour to fit model time interval
bioAvail <- seq(0, 70000, 1)
totTime <- 0.5
a <- 0.050
Th <- 0.912
  
bioRemoved <- (a*bioAvail*totTime)/(1+(a*bioAvail*Th))

bioRemovedPercent <- (a*bioAvail*totTime)/(1+(a*bioAvail*Th))/ bioAvail # represented as a percent

plot(bioAvail,bioRemovedPercent, ylim = c(0,0.03))
plot(bioAvail,bioRemoved)

## Energy content of foodstuffs
# Using foods from Eccard et al. 2021 - but should check which species make sense here
EccardEDmass <- c(58.5,37.6,14.4,40.0,5.9,6.8,7.6,2.6) / 1000
EccardEDj <- c(1100,760,230,540,100,160,80,90) 
EccardED <- EccardEDj / EccardEDmass

# Using values from Meese 1971 for oats, wheat, acorns, and bluebells (Table 1)
MeeseED <- c(4.77*(1 - 0.108)*(1 - 0.028),4.32*(1 - 0.099)*(1 - 0.023),4.36*(1 - 0.401)*(1 - 0.017),4.33*(1 - 0.713)*(1 - 0.006)) * 4184
mean(MeeseED)
ED <- mean(c(EccardED, MeeseED))

## average water content of food 
# from Meese 1971 for oats, wheat, acorns, and bluebells (Table 1)
MeeseWC <- c(10.8,9.9,40.1,71.3)
mean(MeeseWC)


## heat increment of feeding
# 12.8% of MEI in Campbell et al. 1999 - star-nosed moles
# 22% of MEI in Hastings et al. 1997 - mice
# 42% of MEI in MacArthur and Campbell 1994 - muskrat
# 4.2 or 7.3 of GEI in Hindle et al. 2003 - short-tailed shrew 
# 19.7% of GEI at 30 degC in Even and Blais 2016 - mice 
## 23.6% of DEI - When converted for AE used in paper
HIF <- mean(c(23.6, 22)) # taken as the average per MEI for the two mouse studies


## Metabolizable energy intake per gram per day 
## from Sadowska et al. 2016 
# Using UCL values for unshaved voles
mass <- c(0.0192,0.0236) 
MEI <- c(76.9*1000,73.4*1000)
mean(MEI/(mass^0.75))

mtest <- seq(0.005,0.04,0.005)
MEItest <- 1490.90 * (mtest ^ 0.75) * 1000
plot(mtest, MEItest)

# for lactating animals
# mass plus litter mass for total mass
# mean, not upper CI
massL <- c(27.6+29.9,28.3+33.4,31.2+35.7,30.7+34.9) 
MEIL <- c(175,187,205,200)
mean(MEIL/massL)

MEItest <- c(mass,massL) * 3.56

plot(c(mass,massL), MEItest)


## Stomach mass as percent BW
# stomach mass as percent body mass from Song and Wang 2006
bodyMass <- c(48.50,56.33,48.20,49.74,34.33,48.53,53.41,56.65)
sWetMass <- c(0.23,0.49,0.13,0.16,0.36,0.34,0.43,0.30)
percStomach <- sWetMass / bodyMass
meanPercStomach <- mean(percStomach)

# stomach contents from Meese 1971 
# for nonlactating adults
bodyMassWOs <- c(18.1,22.0,22.3,21.2,17.0,21.2,21.9,15.4,9.9,21.1,
                 24.3,19.1,18.3,20.4,18.2,17.7,16.9,6.9,17.8,17.8,17.8,19.8)
stomContDryMass <- c(0.445,0.502,0.228,0.397,1.062,0.269,0.402,0.823,0.672,0.449,
                     0.133,0.459,0.325,0.187,0.248,0.546,0.223,0.305,0.450,0.163,0.428,0.569)
perBMdryMassDbl <- (stomContDryMass * 2)/bodyMassWOs
mean(perBMdryMassDbl)
sd(perBMdryMassDbl)


### ODD plot for IR-mod and storage level
IRmodDat <- tibble(
  SL = seq(0.0,0.398,0.001),
  IRmod = -(2*(SL/max(SL))-1)^3 + 1)

IRmodplot <- ggplot(IRmodDat, aes(x = SL * 100, y = IRmod)) +
  geom_hline(yintercept = 1, linetype = "dashed", col = "grey80", linewidth = 1) +
  geom_line(col = "#3F3F7B", linewidth = 2) +
  labs(y = "Ingestion rate modifier", x = "Body fat [%]") +
  theme_classic() +
  theme(text = element_text(size = 40, color = "grey10", family = "Montserrat"),
        legend.position = "none")
#ggsave("Figures/ODDplots/IRmod.png", IRmodplot, height = 4)

maxSL <- 0.398
LIP <- 0.184 # as midpoint - 1.5%
UIP <- 0.214 # as midpoint + 1.5%

IRmodDatUpd <- IRmodDat %>% 
  mutate(IRmodUpd = ifelse(SL < LIP, (-(SL/LIP) + 1)^3 + 1, 
                           ifelse(SL > UIP, -((SL-UIP)/(maxSL-UIP))^3 + 1, 1))) %>% 
  pivot_longer(cols = 2:3)

  
ggplot(IRmodDatUpd, aes(x = SL * 100, y = value, col = name)) +
  geom_hline(yintercept = 1, linetype = "dashed", col = "grey80", linewidth = 1) +
  geom_line(linewidth = 2) +
  labs(y = "Ingestion rate modifier", x = "Body fat [%]") +
  theme_classic() +
  theme(text = element_text(size = 40, color = "grey10", family = "Montserrat"),
        legend.position = "none")

IRmodDatUpd <- IRmodDatUpd %>%
  filter(name == "IRmodUpd")
# IRmodDatUpd <- IRmodDatUpd %>%
#   mutate(value = round(value, digits = 2))

IRmodplot <- ggplot(IRmodDatUpd, aes(x = SL * 100, y = value)) +
  geom_hline(yintercept = 1, linetype = "dashed", col = "grey80", linewidth = 1) +
  geom_line(col = "#3F3F7B", linewidth = 2) +
  labs(y = "Ingestion rate modifier", x = "Body fat [%]") +
  theme_classic() +
  theme(text = element_text(size = 40, color = "grey10", family = "Montserrat"),
        legend.position = "none")
IRmodplot
#ggsave("Figures/ODDplots/IRmod.png", IRmodplot, height = 4, width = 7)
  
  
  