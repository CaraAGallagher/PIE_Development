# Cara Gallagher
# November 17th, 2021
# Energy Budget with Individual Variation project
# Reproduction cost parameter estimation

##################################################
# Packages:
library(tidyverse)
library(showtext)


font_add_google(name = "Montserrat", family = "Montserrat")
showtext_auto()

##################################################

#### gestation length #### 
# 22 in Bujalska & Ryszkowski 1966
# 20 in Koivula et al. 2003 Ecology
# 18 in Kaczmarski 1966 - Acta Theriologica
GP <- 20 # gestation period in days (should be longer if lactating, but apparently only impacted in implantation time, not costs per Ożdżeński & Mystkowska 1976)

#### lactation length #### 
# 18 in Kaczmarski 1966 - Acta Theriologica
# 25 in Oksanen et al. 2001
# 20 in Oksanen et al. 1999
# 20 in Horne & Ylönen 1998
LP <- mean(c(18,25,20,20))

####  average litter size - pregnancy #### 
# 4.11 average (range 1 to 6) embryos in Brambell & Rowlands, 1936
# 4.9 average (range 2 to 7) in Nerquaye-Tetteh & Clarke, 1990
# 5.2 average (range 1 to 9) for adults in Nyholm & Meurling, 1979
# 4.6 average (range 3 to 14) for yearlings in Nyholm & Meurling, 1979
# 5.2 average (range 3 to 9) in Wiger, 1979
# 6.4 average (300 / 47) in Mukhacheva & Bezel, 2015
# 3.9 average in Castién & Gosálbez, 1998
# 5.6 average for small embryos in Bujalska & Ryszkowski, 1966
# 5.25 average (441 / 84) for site 2 (control) animals in Ryabokon & Goncharova, 2006
# 4.27 average (mean(c(3,10,12,8,16,19,8,9,4)/c(1,3,3,2,3,3,2,2,1))) in Ożdżeński & Mystkowska 1976
## relationship with body weight in Brambell & Rowlands, 1936
## y = 0.1144*M + 1.92

mean(c(1,2,1,3,3)) # mean low range
mean(c(6,7,9,14,9)) # mean high range


####  Abortion rate in Nyholm & Meurling, 1979 and Ryabokon & Goncharova, 2006




####  average litter size - birth #### 
# 4.9 in Bujalska & Ryszkowski 1966
# 2 to 10 (5.27 +- 1.32 pups, mean +- 1 SE) in Koivula et al. 2003 Ecology
# 3 to 7 (5 average) in Kaczmarski 1966 - Acta Theriologica
# 4 in Baker, 1930 & Wrangel, 1940 ref in in Bujalska & Ryszkowski 1966
# 4.05—4.11 in Drożdż, 1963 & Brambell & Rowlands, 1936 ref in in Bujalska & Ryszkowski 1966
# 6.1 in Naumov, 1948 & Popov, 1960 ref in in Bujalska & Ryszkowski 1966
# 4–5 pups per litter in Zejda, 1966
# 3.6 (1-10) in Buchalczyk 1970 - Acta Theriologica
c(4.9,5.27,5,4,4.08,6.1,4.5,3.6)

#### average litter size - weaning #### 
# 3 to 6 (4 average) in Kaczmarski 1966 - Acta Theriologica
# 1.8 +- 0.48, 1.56 +- 1.31, 1.28 +- 0.74 in Koivula et al. 2003 Ecology
# 5.17 +- 0.71 in Koskela 1998 Oecologia - control animals
# 2.34 +- 0.57 in Koskela et al. 1998 - control animals
# 5.28 +- 0.65 in Mappes et al. 1995
c(4,1.8,1.56,1.28,5.17,2.34,5.28)

#### average offspring survival #### 
# 19.8 & 20.3% (from 0 - 100%) in Koivula et al. 2003 Ecology ( survival to 30 days )


#### age of maturity - females #### 
# 45 days 75% pregnant at this point in Oksanen et al. 2001
# 30-45 days in Bujalska, 1983
# 30-45 days in Buchalczyk 1970 - Acta Theriologica


#### age of maturity - males #### 
# 60 days in Bujalska, 1983
# 60 days in Buchalczyk 1970 - Acta Theriologica


#### maximum fetal mass (g) #### 
# from Sawicka-Kapusta 1974 - as the larger of the mean mass at day 1 + 2 standard deviations
maxFetalMass <- 2.2 + 2 * (0.11 * sqrt(9))

#### fetal growth estimation by age #### 
# from Ożdżeński & Mystkowska 1976 - Acta Theriologica
# using only days 10 - 15 as values from larger fetuses can be "deceptive" per the authors
# max as 1.0 to 2.5 g (1.76 +- 0.01 g) in Koivula et al. 2003 Ecology
fetGrowth <- tibble(
  dev_day = c(10,11,12,13,14,15,20),
  mass_mg_hi = c(14,66,116,214,381,500,2500),
  mass_mg = c(11,41,91,192,323,437,1760),
  mass_mg_lo = c(8,26,61,156,257,376,1000)
)

fetGrowth <- fetGrowth %>% 
  mutate(max_mass = mass_mg_hi / 1000,
         mean_mass = mass_mg / 1000,
         min_mass = mass_mg_lo / 1000
         )

# ggplot(fetGrowth, aes( x = dev_day )) +
#   geom_ribbon(aes( ymax = mass_mg_hi, ymin = mass_mg_lo ), alpha = 0.5) +
#   geom_point(aes( y = mass_mg ))


# fit gompertz curve to fetal growth data
fit.gompertz <- function(age, data) {                                 # create gompertz fitting function
  d <- data.frame(age, data) 

  starting.values <- c(                                               # starting values
    A=A1,                                                             # asymptotic starting mass set to max in dataset
    k=k1)#,
  #  No = No1)                                                        # decided to set No manually as kept getting an error otherwise                             

  formula.gompertz <- "data~A*exp(-log(A/No)*exp((-k)*age))"         # fit using standard gompertz curve
  nls(formula.gompertz, d, starting.values)                          # use nls to fit curve using data and specified starting values
}

# fit curve for mean values
A1 <- 3.4
k1 <- 0.06136265
No <- 0.0000031
fit <- fit.gompertz(fetGrowth$dev_day, fetGrowth$mean_mass)  # run formula
summary(fit)

A_mean <- summary(fit)$coefficients[1,1]
k_mean <- summary(fit)$coefficients[2,1]
No_mean <- No

# fit curve for max values
A1 <- A_mean
k1 <- k_mean
No <- 0.000000069
fithi <- fit.gompertz(fetGrowth$dev_day, fetGrowth$max_mass)  # run formula
summary(fithi)

A_hi <- summary(fithi)$coefficients[1,1]
k_hi <- summary(fithi)$coefficients[2,1]
No_hi <- No

# fit curve for min values
A1 <- A_mean
k1 <- k_mean
No <- 0.0000045
fitlo <- fit.gompertz(fetGrowth$dev_day, fetGrowth$min_mass)  # run formula
summary(fitlo)

A_lo <- summary(fitlo)$coefficients[1,1]
k_lo <- summary(fitlo)$coefficients[2,1]
No_lo <- No

# calculate curves using outputs
preds <- tibble(
  dev_day = seq(1,20,1)
)

preds <- preds %>%  
  mutate(max_mass = A_hi*exp(-log(A_hi/No_hi)*exp(-k_hi*dev_day)),
         mean_mass = A_mean*exp(-log(A_mean/No_mean)*exp(-k_mean*dev_day)),
         min_mass = A_lo*exp(-log(A_lo/No_lo)*exp(-k_lo*dev_day)))

preds <- preds %>%  
  gather(key = "metric", value = "pred_mass_g", -dev_day) 


# derivative check
mass_F <- No_hi
for (i in 2:20) {

add <- k_hi*(log(A_hi) - log(mass_F[[i-1]]))
          
mass_F[[i]] <- mass_F[[i-1]] +(exp(log(mass_F[[i-1]]) + add) - mass_F[[i-1]])
i <- i + 1
}
# plot(seq(1,20,1),mass_F)


# combine with data to plot
dat <- fetGrowth %>% 
  select(dev_day, max_mass, mean_mass, min_mass) %>% 
  gather(key = "metric", value = "emp_mass_g", -dev_day)
  
preds <- preds %>% 
  filter(metric == "max_mass")

all_dat <- left_join(preds, dat, by = c("dev_day", "metric"))

outplot <- ggplot() +
  geom_line(data = preds, aes(x = dev_day, y = pred_mass_g), col = "#E4AA4E", size = 2) +
  geom_point(data = dat, aes(x = dev_day, y = emp_mass_g, col = metric, shape = metric), size = 3) +
  scale_colour_grey(name = NULL, labels = c("Maximum", "Mean", "Minimum")) +
  scale_shape(name = NULL, labels = c("Maximum", "Mean", "Minimum")) +
  labs(x = "Day of development [days]", y = "Fetal mass [g]") +
  theme_classic() +
  theme(text = element_text(size = 40, color = "grey30", family = "Montserrat"),
        axis.line = element_line(color = "grey30"),
        legend.position = c(0.17, 0.9),
        legend.background = element_blank())
outplot

#ggsave("Figures/ODDplots/FetalGrowth.png", outplot, height = 4, width = 5.5)



#### placental and fetal membrane costs #### 
# 2.06 Kcal for 5 offspring (as 10.3 - 8.24) in Kaczmarski 1966 - Acta Theriologica

#### pregnancy efficiency #### 
# 13.8% considering the placentae and membranes in Kaczmarski 1966 - Acta Theriologica
# 11.0% considering only baby voles in Kaczmarski 1966 - Acta Theriologica
# 13.9% for common voles in Migula 1969 - Acta Theriologica

#### lactation efficiency #### 
# 15.1% (when corrected for Urinary loss as in Glazier 1985 - 14.6% original in Kaczmarski 1966 - Acta Theriologica
# 15.4% for common voles in Migula 1969 - Acta Theriologica
# 21.7, 19.5, & 18.2% for for fat sand rats in Kam & Degen 1993 - Journal of Theoretical Biology

#### milk production efficiency from stores #### 
# 82% Bondi, 1982 - in Kam, Khokhlova and Degen 2006 &  Kam & Degen, 1993
# 83% for rats pulling from body stores in Romero et al. 1975 - Journal of Dairy Science

#### milk assimilation efficiency for growth of offspring #### 
# 88% as mean of the two alternative values (85 & 91%) provided for fat sand rats in Kam & Degen 1993 - Journal of Theoretical Biology

#### reduction in BMR for pups ####
# estimated from values in Kam, Khokhlova and Degen 2006 - Journal of Zoology
# adjusted for maintenance costs rather than DEE
# using only altricial species of a similar size to bank voles:
# Peromyscus maniculatus, Peromyscus leucopus, Mus musculus & Phodopus sungorus 
# BMRs calculated using relevant relationship from Koteja & Weiner1993

# for mice:
BMmean <- c(4.53,4.9,5.22,3.52)
MEexp <- 2.244 * BMmean^0.706
MEest <- c(2.62,1.75,4.12,3.10)
percEst <- c(MEest / MEexp * 100)

# for hamster:
BMmean <- c(2.12)
MEexp <- 1.959 * BMmean^0.633
MEest <- c(2.33)
percEst <- c(percEst, MEest / MEexp * 100)
mean(percEst)


#### placental energy density ####
# Luz and Griggio 1996 - Ann Nutr Metab
# for rats fed either ab lib, or 20%, 40%, or 60% food restricted  
# energy in kJ over mass in g (kJ / g)
pEDLuzGriggio <- mean(c(2.28/0.67, 1.98/0.61, 1.80/0.59, 1.65/0.50)) * 1000

# Greizerstein 1982 - Biology of Reproduction
# placental percent protein and fat for deposition efficiency 
# pulled from fig. 1 using automeris
pPercLip <- mean(c(3.3, 3.3, 3.1))
pPercPro <- mean(c(12.9, 12.5, 14.0))
# deposition efficiencies from Pullar and Webster 1977
DELip <- 0.735   
DEpro <- 0.444
# placental deposition efficiency 
DEpl <- (pPercLip * DELip + pPercPro * DEpro) / (pPercLip + pPercPro)


#### Relationship between litter size and mass at birth ####
# from Mappes and Koskela 2004 - Evolution
# Data pulled using Automeris WebPlotDigitizer
# Rounded to minimize measurement error
sizeMass <- read_csv2("Data/Parameterization/Reproduction/LitterSizePupMass.csv")

sizeMass <- sizeMass %>% 
  rename(litterSize = `Litter size`, pupMass = `Pup mass`)

mod <- lm(pupMass ~ litterSize, sizeMass)

sizeMass <- sizeMass %>% 
  mutate(pred = coef(mod)[[2]] * litterSize + coef(mod)[[1]]) %>% 
  mutate(litterMass = pred * litterSize)

sizeMassPlot <-
ggplot(sizeMass, aes(x = litterSize)) + 
  geom_jitter(aes(y = pupMass), size = 2, fill = "grey50", alpha = 0.25, width = 0.1) +
  geom_line(aes(y = pred), size = 1.5, col = "maroon") + 
  labs(x = "Litter size [N]", y = "Average pup mass [g]") +
  theme_classic()
sizeMassPlot 

## placental calculation from Mu et al. 2008
dsmating <- seq(10,20,0.1)
plMass <- -0.54180+0.07887*dsmating-0.002243*dsmating^2
pl <- tibble(dsmating,plMass)


ggplot(pl, aes(x = dsmating, y = plMass)) +
  geom_line(col = "#E4AA4E", size = 2) +
  labs(x = "Day of development [days]", y = "Placental mass [g]") +
  theme_classic() +
  scale_x_continuous(breaks = seq(10,20,2), labels = seq(10,20,2)) +
  theme(text = element_text(size = 40, color = "grey30", family = "Montserrat"),
        axis.line = element_line(color = "grey30"),
        legend.position = c(0.17, 0.9),
        legend.background = element_blank())

#ggsave("Figures/ODDplots/PlacentalGrowth.png", height = 4, width = 5.5)
