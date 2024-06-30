# Cara Gallagher
# November 9th, 2021
# Energy Budget with Individual Variation project
# Planning cost of transport modelling

##################################################
# Packages:
library(tidyverse)
library(patchwork)
library(nlme)
library(showtext)
library(LaCroixColoR)


font_add_google(name = "Montserrat", family = "Montserrat")
showtext_auto()

##################################################

# in Pontzer 2016 - https://doi.org/10.1098/rsbl.2015.0935 - COT can be described using the equation:  

# range of masses common for bank voles
Mb <- seq(0.015, 0.035, 0.001)
#Mb <- seq(0.01, 150.01, 0.01)

LLtest <- Mb * (4.5 / 0.037)    # test based on Pontzer 2009 using data for Musk shrew- bad approx
# Mb <- 0.0222

ECOT <- 10.6*Mb^-0.29

# comparison of various approaches for min COT in J kg-1 m-1
ECOTpontzer <- 10.6*Mb^-0.29 
ECOTtaylor1982 <- 10.7*Mb^-0.316     # From Taylor et al. 1982 - Energetics and mechanics of terrestrial locomotion 
ECOTfulltu <- 10.8*Mb^-0.32     # From Full and Tu 1991 - . Mechanics of a rapid running insect:two-, four- and six-legged locomotion
ECOTcalden <- (10.7*Mb^0.68 )/ Mb    # From Calden 1966 - in Marie's ODD
ECOTgarland <- (10.678*Mb^0.70 )/ Mb    # From garland et al 1983 
ECOTgarland2017 <- 10^(((0.567+log10(Mb)*0.717)/ Mb) * 20.1)
ECOTrezende <- ((0.0136 + (Mb * 1000)*0.00094)* 20.1)/(Mb) # Effects of Size, Sex, and Voluntary Running Speeds on Costs of Locomotion in Lines of Laboratory Mice Selectively Bred for High Wheel‐Running Activity
ECOTpontzerhh <- 90.284*LLtest^-0.77 # Pontzer 2009


# plot(Mb, ECOT, ylim = c(0,50))
# lines(Mb, ECOTpontzer, col = "red")
# lines(Mb, ECOTtaylor1982, col = "green")
# lines(Mb, ECOTfulltu, col = "blue")
# lines(Mb, ECOTcalden, col = "orange")
# lines(Mb, ECOTgarland, col = "yellow")


# comparison of various approaches for speed dependent COT
v <- seq(0.1,4,0.01) * 1000 / 3600  # speed in m / s
ECOTpontzerV <- ECOTpontzer[11] * Mb[11] * v   # from Pontzer 2016 in J s-1
ECOTtaylor1982V <- ECOTtaylor1982[11] * Mb[11] * v   
ECOTfulltuV <- ECOTfulltu[11] * Mb[11] * v   
ECOTcaldenV <- ECOTcalden[11] * Mb[11] * v   
ECOTgarlandV <- ECOTgarland[11] * Mb[11] * v   

# plot(v, ECOTpontzerV )
# lines(v, ECOTpontzerV, col = "red")
# lines(v, ECOTtaylor1982V, col = "green")
# lines(v, ECOTfulltuV, col = "blue")
# lines(v, ECOTcaldenV, col = "orange")
# lines(v, ECOTgarlandV, col = "yellow")

# other estimates

# COT from mice in Vaanholt et al. 2007 - https://doi.org/10.1242/jeb.001974  
MbVaanholtC <- 34.6 / 1000
MbVaanholtW <- 28.2 / 1000

COTtotVaanholtC <- 2.3 / MbVaanholtC
COTtotVaanholtW <- 1.6 / MbVaanholtW

# from Chappell et al. 2004 -  Voluntary running in deer mice: speed, distance, energy costs and temperature effects 
Vdm <- seq(0,6,0.1) 
BMdm <- 21.5
VO2dm <- ((1.916+0.501*Vdm)* 20.1)  / 60
Vdm <- Vdm * 1000 / 3600
WPreddm <- ((7880.479 * (BMdm/1000)^0.70) / (30 * 60)) + (4.70 * (BMdm/1000)^0.63) + ((10.6*(BMdm/1000)^-0.29 )* (BMdm/1000) * Vdm)
# plot(Vdm, VO2dm)
# lines(Vdm, WPreddm)


# from Dlugosz et al. 2009 - Locomotor trade-offs in mice selectively bred for high voluntary wheel running
Vmm1 <- seq(0,100,1)
BMmm1 <- 30
VO2mm1 <- (2.25+0.03029*Vmm1)* 20.1 / 60
Vmm1 <- Vmm1 / 60
WPredmm1 <- ((7880.479 * (BMmm1/1000)^0.70) / (30 * 60)) + (4.70 * (BMmm1/1000)^0.63) + ((10.6*(BMmm1/1000)^-0.29 )* (BMmm1/1000) * Vmm1)
# plot(Vmm1, VO2mm1)
# lines(Vmm1, WPredmm1)

# from Rezende et al. 2006 - Effects of Size, Sex, and Voluntary Running Speeds on Costs of Locomotion in Lines of Laboratory Mice Selectively Bred for High Wheel‐Running Activity
Vmm2 <- seq(0,100,1)
BMmm2 <- 27.1
VO2mm2 <- ((3.17+0.043*Vmm2) * 20.1) / 60
Vmm2 <- Vmm2 / 60
WPredmm2 <- ((7880.479 * (BMmm2/1000)^0.70) / (30 * 60)) + (4.70 * (BMmm2/1000)^0.63) + ((10.6*(BMmm2/1000)^-0.29 )* (BMmm2/1000) * Vmm2)
# plot(Vmm2, VO2mm2)
# lines(Vmm2, WPredmm2)

# plot(1, type="n", xlab="", ylab="",xlim = c(0,1), ylim = c(0,2))
# lines(Vdm, VO2dm, col = "red")
# lines(Vdm, WPreddm, col = "red", lty = "dashed")
# lines(Vmm1, VO2mm1, col = "orange")
# lines(Vmm1, WPredmm1, col = "orange", lty = "dashed")
# lines(Vmm2, VO2mm2, col = "green")
# lines(Vmm2, WPredmm2, col = "green", lty = "dashed")
# 
# allmice <- tibble(
#   speed = c(Vdm, Vmm1, Vmm2),
#   emp = c(VO2dm, VO2mm1, VO2mm2),
#   pred = c(WPreddm, WPredmm1, WPredmm2)
# )
# 
# allmice <- allmice %>%
#   mutate(diff= ((pred - emp)/ emp)* 100)
# 
# ggplot(allmice, aes(x = diff)) + geom_histogram(binwidth = 0.5)
# 
# mean(allmice$diff)
# sd(allmice$diff)

### collecting postural cost estimates

# from Chappell et al. 2004 -  Voluntary running in deer mice: speed, distance, energy costs and temperature effects 
# in ml O2 min-1
BmChappell <- 0.0222
postCostChappell <- 1.25    # said that did not change with ambient temperature
RMR <- 0.53   # RMR at 25 degC
percPostCostChappell <- ((postCostChappell / RMR)*100)

# from Dlugosz et al. 2009 - Locomotor trade-offs in mice selectively bred for high voluntary wheel running
# for both normal lines (in ml O2 min-1)
BmDlugosz <- c(0.03033,0.03096)
postCostDlugosz <- c(1.544,1.531)
RMR <- c(0.730,0.727)
percPostCostDlugosz <- (postCostDlugosz / RMR)*100

# from Rezende et al. 2006 - Effects of Size, Sex, and Voluntary Running Speeds on Costs of Locomotion in Lines of Laboratory Mice Selectively Bred for High Wheel‐Running Activity
# for both male and female control lines (in ml O2 min-1)
BmRezende <- c(0.0268,0.0413)
postCostRezende <- c(1.619,1.924)
RMR <- c(0.870,0.817)
percPostCostRezende <- (postCostRezende / RMR)*100

# overall average 
percPostCost <- mean(c(percPostCostChappell, percPostCostDlugosz, percPostCostRezende))
percPostCost

# relationship with body mass
micePostCost <- tibble(
  Bm = c(BmChappell,BmDlugosz,BmRezende),
  postCost =c(postCostChappell, postCostDlugosz, postCostRezende),
  source = c("Chappell et al. 2004", "Dlugosz et al. 2009", "Dlugosz et al. 2009", "Rezende et al. 2006", "Rezende et al. 2006")
)

micePostCost <- micePostCost %>% 
  mutate(postCost = postCost * 20.1 / 60)   # in J s-1

#ggplot(micePostCost, aes(x = Bm, y = postCost)) + geom_point()

gls_1 <- gls(log10(postCost) ~ log10(Bm), data = micePostCost)

micePostCost <- micePostCost %>% 
  mutate(predCost = (10^coef(gls_1)[1]) * Bm^(coef(gls_1)[2]))

cotplotout <- ggplot(micePostCost, aes(x = Bm)) + 
  geom_point(aes(y = postCost, col = source, shape = source), size = 3)+ 
  geom_line(aes(y = predCost), col = "#E4714E", size = 2) +
  scale_colour_grey(name = NULL) +
  scale_shape(name = NULL) +
  labs( x = "Body mass [kg]", y = "Postural cost [W]")+ 
  theme_classic() +
  theme(text = element_text(size = 40, color = "grey30", family = "Montserrat"),
        axis.line = element_line(color = "grey30"),
        legend.position = c(0.27, 0.9),
        legend.background = element_blank())
cotplotout

#ggsave("Figures/ODDplots/COT.png", cotplotout, height = 4, width = 6)


##### example output for a 30g vole

ExampleOut <- tibble(
  speed = seq(0,40,1) / 60,
  BodyMass = 0.030)
ExampleOut <- ExampleOut %>% 
  mutate(
    BMR = (6053.1* BodyMass^0.64) / (30 * 60),      # from estimate in EBIV_BMR_parameter_estimation.R script converted to W
    PCOT = (4.70 * BodyMass^0.63),                  # from postural cost/body mass relationship estimated from mouse studies above
    iCOT = (10.6*BodyMass^-0.29 )* BodyMass,        # from Pontzer 2016 in J m-1
    COTtot = BMR + PCOT + iCOT * speed              # in W
  )

pal <- lacroix_palette("Pamplemousse", type = "discrete")

plot1 <- ggplot(ExampleOut, aes(x = speed)) +
  geom_ribbon(aes(ymax = COTtot), ymin = 0, fill = pal[6]) +
  geom_ribbon(aes(ymax = PCOT + BMR), ymin = 0, fill = pal[5]) +
  geom_ribbon(aes(ymax = BMR), ymin = 0, fill = pal[1]) +
  lims(y = c(0,1.75)) +
  theme_classic() +
  labs( x = "Running speed (m s-1)", y = "Metabolic cost (W)") +
  annotate(geom="text", x=0.35, y=0.2, label="Basal metabolic rate",
           color="white", size = 5)+
  annotate(geom="text", x=0.35, y=0.65, label="Postural costs",
           color="white", size = 5)+
  annotate(geom="text", x=0.35, y=1, label="Incremental costs",
           color="white", size = 5)



ExampleOutCOT <- tibble(BodyMass = seq(0.015,0.035,0.001))
ExampleOutCOT <- ExampleOutCOT %>% 
  mutate(
    BMR = (7880.479 * BodyMass^0.70) / (30 * 60),   # from estimate in EBIV_BMR_parameter_estimation.R script converted to W
    iCOT = (10.6*BodyMass^-0.29),                   # from Pontzer 2016 in J kg-1 m-1
    COTmin = iCOT 
  )
### ^^^ I think these aren't in the same units, CHECK

plot2 <- ggplot(ExampleOutCOT, aes(x = BodyMass, y = COTmin)) +
  geom_line(col = pal[4], size = 1.5) +
 # lims(y = c(0,2)) +
  theme_classic() +
  labs( x = "Body mass (kg)", y = "Minimum cost of transport (J kg-1 m-1)")

fullPlot <- plot1 + plot2 + 
  plot_layout(widths = c(3, 3), heights = 0.5)

Eval <- tibble(
  speed = seq(0,40,1), 
  Dlugosz_30.3g = (2.272+0.0336*speed)*20.1/60,
  Dlugosz_31.0g = (2.258+0.0365*speed)*20.1/60,
  RezendeM_40.8g = (2.873+0.0641*speed)*20.1/60,
  RezendeF_27.1g = (2.497+0.0360*speed)*20.1/60,
  Chappell_21.5g = (1.916+0.501*(speed / 16.6667))*20.1/60,
  Taylor_30g = (((10.7*(0.03^-0.316))*(speed/60)) + (6.03*(0.03^-0.303)))* 0.03,
  Model_30g = ExampleOut$COTtot
)

Eval <- Eval %>% 
  gather(key = "Source", value = "value", -speed)

EvalPlot <-ggplot(Eval, aes(x=speed, y = value, col = Source)) + 
  geom_line(size = 1.5)+
  lims(y = c(0,2)) +
  theme_classic() +
  labs( x = "Running speed (m min-1)", y = "Metabolic cost (W)")


Eval <- tibble(
  speed = seq(0,40,1), 
  Dlugosz_30.3g = ((2.272+0.0336*speed)*20.1/60)/0.0303,
  Dlugosz_31.0g = ((2.258+0.0365*speed)*20.1/60)/0.031,
  RezendeM_40.8g = ((2.873+0.0641*speed)*20.1/60)/0.0408,
  RezendeF_27.1g = ((2.497+0.0360*speed)*20.1/60)/0.0271,
  Chappell_21.5g = ((1.916+0.501*(speed / 16.6667))*20.1/60)/0.0215,
  Taylor_30g = (((10.7*(0.03^-0.316))*(speed/60)) + (6.03*(0.03^-0.303))),
  Model_30g = ExampleOut$COTtot/0.030
)

Eval <- Eval %>% 
  gather(key = "Source", value = "value", -speed)

EvalPlotPerMass <- ggplot(Eval, aes(x=speed, y = value, col = Source)) + 
  geom_line(size = 1.5)+
  lims(y = c(0,50)) +
  theme_classic() +
  labs( x = "Running speed (m min-1)", y = "Metabolic cost (W kg-1)")


### Maximum metabolism during forced treadmill running ###
# In Jaromin et al 2016
# data from Figure 3a, line extracted for C running using Webplotdigitizer

Jaromin2016 <- tibble(
  mass = c(15.89, 34.07), 
  VO2max = c(3.7421875, 6.0859375) # ml O2 / min
)

Jaromin2016 <- Jaromin2016 %>% 
  mutate(Emax = VO2max * 20.1 / 60) # convert to watts

mod <- tibble(
      mass = seq(0.015,0.035,0.001),
      speed = 0.53)

mod <- mod %>% 
  mutate(
    BMR = (6053.1* mass^0.64) / (30 * 60),      # from estimate in EBIV_BMR_parameter_estimation.R script converted to W
    PCOT = (4.70 * mass^0.63),                  # from postural cost/body mass relationship estimated from mouse studies above
    iCOT = (10.6*mass^-0.29 )* mass,            # from Pontzer 2016 in J m-1
    modEmax = BMR + PCOT + iCOT * speed         # in W
  ) 


# Additional data on maximum metabolic rates from Rudolf et al 2017 - https://doi.org/10.1016/j.exger.2017.08.007

Rudolf2017points <- tibble(
  mass = c(21.9,27.7,27.9),
  VO2run = c(4.3,5.0,4.9),
  maxSprintSpeed = c(88.2,93.0,106.8),
  maxLongRunSpeed = c(44.0,44.2,41.3),
  speedAtVO2run = c(31.0,33.5,30.3))

Rudolf2017points <- Rudolf2017points %>% 
  mutate(Erun = VO2run * 20.1 / 60, 
         maxSprintSpeed = maxSprintSpeed / 60,
         maxLongRunSpeed = maxLongRunSpeed / 60,
         speedAtVO2run = speedAtVO2run / 60)

# comparing to model
# mean(c(1.09 / 1.44, 1.28 / 1.68, 1.28 / 1.64))


Rudolf2017Line <- tibble(
  mass = c(14.42,32.44,16.78,41.20,17.73,41.70),
  VO2max = c(3.44,5.58,3.75,6.36,3.92,6.33) 
)

Rudolf2017Line <- Rudolf2017Line %>% 
  mutate(Emax = VO2max * 20.1 / 60) # convert to watts

ggplot() + 
  geom_point(data = Rudolf2017points, aes(x=mass,y=Erun))+
  geom_line(data = Jaromin2016,aes(x=mass,y=Emax), col = "grey50")+ 
  geom_line(data = Rudolf2017Line,aes(x=mass,y=Emax))+ 
  geom_line(data = mod,aes(x=mass*1000,y=(modEmax/0.716)), col = "red")


