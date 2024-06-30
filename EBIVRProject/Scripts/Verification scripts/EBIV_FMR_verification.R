# Cara Gallagher
# March 1st, 2022
# Energy Budget with Individual Variation project
# Field metabolic rates output verification

##################################################
# Packages:
library(tidyverse)
library("LaCroixColoR")
library(patchwork)

##################################################

## FMR by mass
# Empirical relationships

empRel <- tibble(
  mass = seq(0.01,0.03, 0.001),
  Speakman2000 = exp(1.878) * ((mass * 1000)^0.659),    # small mammals
  King1974 = 753 * mass^0.67,                           # rodents
  Nagy1999 = 5.48 * (mass * 1000)^0.71                  # rodents
)

empRel <- empRel %>% 
  pivot_longer(names_to = "source", values_to = "FMR", !mass)



# model outputs 

modOut <- read_csv("Data/Verification/FMR/EBIVPrototype_FMRByMass.csv", skip = 16)

modOut <- modOut %>% 
  select(x, y) %>% 
  rename(mass = x, FMR = y) %>% 
  mutate(FMR = FMR / 1000)


pal <- rev(lacroix_palette("Pamplemousse", type = "discrete"))

FMRVerPlot1 <- 
ggplot() +
  geom_point(data = modOut, aes(x = mass, y = FMR), col = "grey50", alpha = .01) +
  geom_line(data = empRel, aes(x = mass, y = FMR, col = source), size = 2) +
  labs(x = "Body mass [kg]", y = "Field metabolic rate [kJ day-1]", col = NULL) +
  scale_color_manual(values = pal) +
  theme_classic() +
  theme(legend.position = c(0.5, 0.975),
        legend.direction = "horizontal",
        legend.background = element_rect(fill = NA))
FMRVerPlot1

## FMR by state 
# For Rutkowska et al. 2011 - for day 15 of lactation
Rutkowska2011 <- tibble(
  mass = c(20.0, 23.8, 23.4, 25.7, 26.0, 26.8, 27.2, 27.3, 27.0, 27.1, 26.7, 28.7, 28.3, 28.4, 28.4, 28.8, 28.3, 27.9, 29.3, 30.0, 29.9, 29.7, 29.4, 29.2, 30.4, 30.8, 30.6, 30.0, 30.3, 31.2, 31.1, 34.3, 33.4, 32.5, 32.4, 33.3, 33.6, 33.1, 34.1, 36.0, 36.0, 35.4, 37.1),
  FMR = c(45.3, 57.1, 79.9, 62.2, 67.6, 66.2, 69.4, 72.6, 72.4, 76.6, 80.3, 94.7, 84.7, 76.7, 71.3, 71.8, 64.7, 61.9, 67.0, 70.7, 73.4, 76.7, 76.1, 78.3, 81.4, 80.6, 86.4, 88.1, 102.8, 95.8, 72.7, 56.2, 69.8, 85.9, 87.2, 90.7, 90.7, 103.3, 108.1, 121.6, 98.2, 89.9, 91.7)
)

empPoints <- tibble(
  mean = c(88.8, 70.9, 57, 97, ((4.07 * 20.1 * 23.9 * 24) / 1000), ((4.29 * 20.1 * 23.7 * 24) / 1000), ((3.79 * 20.1 * 19.3 * 24) / 1000), ((3.65 * 20.1 * 19.8 * 24) / 1000), 66.44, mean(Rutkowska2011$FMR)),
  error = c(5.13, 3.95, 3, 1.7, ((0.62 * 20.1 * 23.9 * 24) / 1000), ((0.64 * 20.1 * 23.7 * 24) / 1000), ((0.36 * 20.1 * 19.3 * 24) / 1000), ((0.53 * 20.1 * 19.8 * 24) / 1000), 7.8, sd(Rutkowska2011$FMR)),
  state = c("Nonreproducing", "Nonreproducing", "Nonreproducing", "Lactating", "Nonreproducing", "Nonreproducing", "Nonreproducing", "Nonreproducing", "Nonreproducing", "Lactating"),
  source = c("Peacock et al. 2004", "Peacock et al. 2004", "Sadowska et al. 2016", "Sadowska et al. 2016", "Górecki 1968", "Górecki 1968", "Górecki 1968", "Górecki 1968", "Tidhar et al. 2007", "Rutkowska et al. 2011")
)


# model outputs 

modOut <- read_csv("Data/Verification/FMR/EBIVPrototype_FMRByState.csv", skip = 20)

modOut <- modOut %>% 
  select(y...2, y...6, y...10, y...14, y...18) %>% 
  rename("Juvenile" = y...2, "Nonreproducing" = y...6, "Lactating" = y...10, "Lactating and pregnant" = y...14, "Pregnant" = y...18) %>% 
  pivot_longer(everything(), names_to = "state", values_to = "FMR")  %>% 
  mutate(state = fct_relevel(state, "Juvenile", "Nonreproducing", "Pregnant", "Lactating", "Lactating and pregnant"),
         FMR = FMR / 1000)


pal <- lacroix_palette("PeachPear", type = "discrete")
pal <-c(pal[1],pal[3],pal[4],pal[5],pal[6])

FMRVerPlot3 <- 
ggplot() + 
  geom_violin(data = modOut, aes(x = state, y = FMR, fill = state), col = NA, alpha = 0.25) +
  geom_boxplot(data = modOut, aes(x = state, y = FMR, col = state, fill = state), fill = "white", width = 0.1, alpha = 0.5) +
  geom_linerange(data = empPoints, aes(x = state, y = mean, group = mean, col = state, ymin = mean - error, ymax = mean + error), position = position_dodge(width = 0.75), size = 1) +
  geom_point(data = empPoints, aes(x = state, y = mean, group = mean, shape = source, col = state), position = position_dodge(width = 0.75), size = 2) +
  scale_color_manual(values = pal, guide = "none") +
  scale_fill_manual(values = pal, guide = "none") +
  theme_classic() +
  labs( x = NULL, y = "Daily energy use [kJ day-1]", shape = NULL) +
  theme(legend.position = c(0.5, 0.975),
        legend.direction = "horizontal",
        legend.background = element_rect(fill = NA))
FMRVerPlot3

## Change in FMR with food debt 
# model outputs 

modOut <- read_csv("Data/Verification/FMR/EBIVPrototype_FMRByFoodDebt.csv", skip = 16)

modOut <- modOut %>% 
  select(x, y) %>% 
  rename(foodDebt = x, FMR = y) %>% 
  mutate(FMR = FMR / 1000)



ggplot(modOut, aes(x = foodDebt, y = FMR)) +
  geom_point( col = "grey50", alpha = .01) +
  geom_smooth(method = "lm") +
  labs(x = "Food debt [g]", y = "Field metabolic rate [kJ day-1]", col = NULL) +
  theme_classic() 


## Change in FMR with daily activity 
# model outputs 

modOut <- read_csv("Data/Verification/FMR/EBIVPrototype_FMRByActivity.csv", skip = 16)

modOut <- modOut %>% 
  select(x, y) %>% 
  rename(activity = x, FMR = y) %>% 
  mutate(FMR = FMR / 1000)


FMRVerPlot2 <- 
ggplot(modOut, aes(x = activity, y = FMR)) +
  geom_point( col = "grey50", alpha = .01) +
  geom_smooth(method = "lm", se = FALSE, size = 2, col = "grey50") +
  labs(x = "Percent of day active [%]", y = "Field metabolic rate [kJ g-1 day-1]", col = NULL) +
  theme_classic() 
FMRVerPlot2


## Focal follow plot

modOut <- read_csv("Data/Verification/FMR/EBIVPrototype_FF50.csv", skip = 22)

modOutB <- modOut %>% 
  select(x...1, y...2) %>% 
  rename(age = x...1, BMR = y...2) %>% 
  mutate(BMR = BMR / 1000) %>% 
  group_by(age) %>% 
  summarise(BMR = mean(BMR))%>% 
  rename(value = BMR) %>% 
  mutate(metric = "BMR")

modOutL <- modOut %>% 
  select(x...5, y...6) %>% 
  rename(age = x...5, Loco = y...6) %>% 
  mutate(Loco = Loco / 1000) %>% 
  group_by(age) %>% 
  summarise(Loco = mean(Loco))%>% 
  rename(value = Loco) %>% 
  mutate(metric = "Loco")

modOutG <- modOut %>% 
  select(x...9, y...10) %>% 
  rename(age = x...9, Growth = y...10) %>% 
  mutate(Growth = Growth / 1000) %>% 
  group_by(age) %>% 
  summarise(Growth = mean(Growth))%>% 
  rename(value = Growth) %>% 
  mutate(metric = "Growth")

modOutP <- modOut %>% 
  select(x...13, y...14) %>% 
  rename(age = x...13, Preg = y...14) %>% 
  mutate(Preg = Preg / 1000) %>% 
  group_by(age) %>% 
  summarise(Preg = mean(Preg))%>% 
  rename(value = Preg) %>% 
  mutate(metric = "Preg")

modOutLa <- modOut %>% 
  select(x...17, y...18) %>% 
  rename(age = x...17, Lact = y...18) %>% 
  mutate(Lact = Lact / 1000) %>% 
  group_by(age) %>% 
  summarise(Lact = mean(Lact))%>% 
  rename(value = Lact) %>% 
  mutate(metric = "Lact")

modOutH <- modOut %>% 
  select(x...25, y...26) %>% 
  rename(age = x...25, HIF = y...26) %>% 
  mutate(HIF = HIF / 1000) %>% 
  group_by(age) %>% 
  summarise(HIF = mean(HIF)) %>% 
  rename(value = HIF) %>% 
  mutate(metric = "HIF")

# modOut <- modOutB %>% 
#   left_join(modOutL, by = "age") %>% 
#   left_join(modOutG, by = "age") %>% 
#   left_join(modOutP, by = "age") %>% 
#   left_join(modOutLa, by = "age") %>% 
#   left_join(modOutH, by = "age") 

modOut <- bind_rows(modOutB, modOutL, modOutG, modOutP, modOutLa, modOutH)

modOut <- modOut %>% 
  filter(age != 20 & age < 95) %>% 
  mutate(metric = factor(metric, levels=rev(c("BMR", "Loco", "Growth", "HIF", "Preg", "Lact")))) %>% 
  complete(age, metric, fill = list(value = 0))
  
  
pal <- lacroix_palette("PassionFruit", type = "discrete")

FMRVerPlot4 <- 
  ggplot(modOut, aes(x = age, y = value, fill = metric)) +
  geom_area(alpha = 0.5) +
  scale_fill_manual(values = rev(pal)) +
  labs(x = "Age [days]", y = "Metabolic rate [kJ day-1]", fill = NULL) +
  theme_classic()
FMRVerPlot4







