# Cara Gallagher
# August 29th, 2022
# Energy Budget with Individual Variation project
# Calibration output plots

#------------------------------------------------#
####  Packages: #### 
library(tidyverse)
library(showtext)
library(patchwork)

#------------------------------------------------#

font_add_google(name = "Montserrat", family = "Normal")
showtext_auto()

####  Pattern 1. Fetal mass at birth  #### 

p1.plot <- tibble(x = 1, Hi = 2.5, Lo = 1.0)

p1.plot <- 
  ggplot(p1.plot, aes(x = x)) +
  geom_rect(aes(ymin = Lo, ymax = Hi, xmin = x - 0.5, xmax = x + 0.5), fill = "grey80") + 
  labs(title = "Pattern 1. Fetal mass at birth", y = "Fetal mass [g]") +
  xlim(0,2) +
  ylim(0.5,3) +
  theme_classic() +
  theme(text = element_text(family = "Normal", size = 16, color = "grey10"),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.5))
p1.plot


####  Pattern 2. Birth mass by litter size  #### 

p2.plot <- 
  ggplot() +
  geom_segment(aes(x = 1, y = 2.5, yend = 1, xend = 9), col = "grey80", size = 5) +
  labs(x = "Litter size [N]", y = "Fetal mass [g]", title = "Pattern 2. Birth mass by litter size") +
  theme_classic() +
  ylim(0.5,3) +
  scale_x_continuous(breaks = 1:9) +
  theme(text = element_text(family = "Normal", size = 16, color = "grey10"),
        plot.title=element_text(hjust=0.5))
p2.plot


####  Pattern 3. Total body mass by age  #### 

p3.plot <- tibble(
  age = seq(21,620,1),
  total.mass = 0.02483015*(1 - (1 - (0.005569125 / 0.02483015)^(1/3))*exp((-0.03913599*age)/3))^3,
  total.mass.hi = total.mass * 1.25,
  total.mass.lo = total.mass * 0.75
)


p3.plot <- ggplot(p3.plot, aes(x = age)) +
  geom_ribbon(aes(ymin = total.mass.lo * 1000, ymax = total.mass.hi * 1000), fill = "grey80") + 
  labs(title = "Pattern 3. Total body mass by age", y = "Total body mass [g]", x = "Age [days]") +
  theme_classic() +
  theme(text = element_text(family = "Normal", size = 16, color = "grey10"),
        plot.title=element_text(hjust=0.5))


####  Pattern 4. Lean mass by age  #### 

p4.plot <- tibble(
  age = seq(21,620,1),
  lean.mass = 0.01930132*(1 - (1 - (0.004688886 / 0.01930132)^(1/3))*exp((-0.04845531*age)/3))^3,
  lean.mass.hi = lean.mass * 1.25,
  lean.mass.lo = lean.mass * 0.75
)


p4.plot <- ggplot(p4.plot, aes(x = age)) +
  geom_ribbon(aes(ymin = lean.mass.lo * 1000, ymax = lean.mass.hi * 1000), fill = "grey80") + 
  labs(title = "Pattern 4. Lean mass by age", y = "Lean body mass [g]", x = "Age [days]") +
  theme_classic() +
  theme(text = element_text(family = "Normal", size = 16, color = "grey10"),
        plot.title=element_text(hjust=0.5))


####  Pattern 5. Lactating mother mass by pup age  #### 

library(readxl)
p567.pattern <- read_excel("Data/Verification/Reproduction/Sadowska2016/Fig1Dat.xlsx")


p5.plot <- p567.pattern %>% 
  select(ageGest, Metric, bodyMass) %>% 
  pivot_wider(names_from = Metric, values_from = bodyMass) %>% 
  rename(p5.Hi = Lo, p5.Lo = Hi) %>% 
  select(-Mean)


p5.plot <- ggplot(p5.plot, aes(x = ageGest)) +
  geom_ribbon(aes(ymin = p5.Lo, ymax = p5.Hi), fill = "grey80") + 
  labs(title = "Pattern 5. Lactating mother mass by pup age", y = "Mother body mass [g]", x = "Days since birth") +
  theme_classic() +
  ylim(15,40) +
  theme(text = element_text(family = "Normal", size = 16, color = "grey10"),
        plot.title=element_text(hjust=0.5))


####  Pattern 6. Lactating mother food intake by pup age  #### 

p6.plot <- p567.pattern %>% 
  select(ageGest, Metric, foodCons) %>% 
  pivot_wider(names_from = Metric, values_from = foodCons ) %>% 
  rename(p6.hi = Lo, p6.lo = Hi) %>% # names were swapped
  select(-Mean) %>% 
  mutate(p6.hi = p6.hi * 17.80, p6.lo = p6.lo * 17.80)   # converted to kJ to compare


p6.plot <- ggplot(p6.plot, aes(x = ageGest)) +
  geom_ribbon(aes(ymin = p6.lo, ymax = p6.hi), fill = "grey80") + 
  labs(title = "Pattern 6. Lactating mother food intake by pup age", y = "Mother energy consumption [kJ]", x = "Days since birth") +
  theme_classic() +
  theme(text = element_text(family = "Normal", size = 16, color = "grey10"),
        plot.title=element_text(hjust=0.5))



####  Pattern 7. Total litter mass by pup age  #### 

p7.plot <- p567.pattern %>% 
  select(ageGest, Metric, LitterMass) %>% 
  pivot_wider(names_from = Metric, values_from = LitterMass) %>% 
  rename(p7.hi = Lo, p7.lo = Hi) %>% 
  select(-Mean) 

p7.plot <- ggplot(p7.plot, aes(x = ageGest)) +
  geom_ribbon(aes(ymin = p7.lo, ymax = p7.hi), fill = "grey80") + 
  labs(title = "Pattern 7. Total litter mass by pup age", y = "Total litter mass [g]", x = "Days since birth") +
  theme_classic() +
  theme(text = element_text(family = "Normal", size = 16, color = "grey10"),
        plot.title=element_text(hjust=0.5))


####  Pattern 8. Mother peak food intake by litter size  #### 

p8910.pattern <- read_excel("Data/Verification/Reproduction/Sadowska2016/Fig2Dat.xlsx")

p8.plot <- p8910.pattern %>% 
  filter(Metric == "FC") %>% 
  mutate(Value = Value * 17.80) %>% # converted to kJ to compare
  mutate(p8.hi = Value * 1.25, p8.lo = Value * 0.75) %>% 
  select(-Value, -Metric)


p8.plot <- ggplot(p8.plot, aes(x = litterSize)) +
  geom_ribbon(aes(ymin = p8.lo, ymax = p8.hi), fill = "grey80") + 
  labs(title = "Pattern 8. Mother peak food intake by litter size", y = "Mother energy consumption [kJ]", x = "Litter size [N]") +
  theme_classic() +
  ylim(0, 450) +
  scale_x_continuous(breaks = 1:9) +
  theme(text = element_text(family = "Normal", size = 16, color = "grey10"),
        plot.title=element_text(hjust=0.5))



####  Pattern 9. Mother peak energy use by litter size  #### 

p9.plot <- p8910.pattern %>% 
  filter(Metric == "ADMR") %>% 
  mutate(p9.hi = Value * 1.25, p9.lo = Value * 0.75) %>% 
  select(-Value, -Metric)


p9.plot <- ggplot(p9.plot, aes(x = litterSize)) +
  geom_ribbon(aes(ymin = p9.lo, ymax = p9.hi), fill = "grey80") + 
  labs(title = "Pattern 9. Mother peak energy use by litter size", y = "Mother peak energy use [kJ day-1]", x = "Litter size [N]") +
  theme_classic() +
  ylim(0, 200) +
  scale_x_continuous(breaks = 1:9) +
  theme(text = element_text(family = "Normal", size = 16, color = "grey10"),
        plot.title=element_text(hjust=0.5))



####  Pattern 10. Mother peak milk transfer by litter size  #### 

p10.plot <- p8910.pattern %>% 
  filter(Metric == "MEO") %>% 
  mutate(p10.hi = Value * 1.25, p10.lo = Value * 0.75) %>% 
  select(-Value, -Metric)

p10.plot <- ggplot(p10.plot, aes(x = litterSize)) +
  geom_ribbon(aes(ymin = p10.lo, ymax = p10.hi), fill = "grey80") + 
  labs(title = "Pattern 10. Mother peak milk transfer by litter size", y = "Mother peak milk transfer [kJ day-1]", x = "Litter size [N]") +
  theme_classic() +
  scale_x_continuous(breaks = 1:9) +
  theme(text = element_text(family = "Normal", size = 16, color = "grey10"),
        plot.title=element_text(hjust=0.5))
p10.plot

####  Pattern 11. Pup mass at weaning by litter size  #### 

p11.plot <- 
  ggplot() +
  geom_segment(aes(x = 1, y = 15, yend = 10, xend = 9), col = "grey80", size = 5) +
  labs(x = "Litter size [N]", y = "Pup mass [g]", title = "Pattern 11. Pup mass at weaning by litter size ") +
  theme_classic() +
  ylim(5,20) +
  scale_x_continuous(breaks = 1:9) +
  theme(text = element_text(family = "Normal", size = 16, color = "grey10"),
        plot.title=element_text(hjust=0.5))
p11.plot

####  Pattern 12. Litter size at birth  #### 

p12.plot <- tibble(x = 1, Hi = 6.1, Lo = 3.6)

p12.plot <- 
  ggplot(p12.plot, aes(x = x)) +
  geom_rect(aes(ymin = Lo, ymax = Hi, xmin = x - 0.5, xmax = x + 0.5), fill = "grey80") + 
  labs(title = "Pattern 12. Litter size at birth", y = "Litter size [N]") +
  xlim(0,2) +
  ylim(2,8) +
  theme_classic() +
  theme(text = element_text(family = "Normal", size = 16, color = "grey10"),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.5))
p12.plot


####  Pattern 13. Litter size at weaning  #### 


p13.plot <- tibble(x = 1, Hi = 5.28, Lo = 1.28)

p13.plot <- 
  ggplot(p13.plot, aes(x = x)) +
  geom_rect(aes(ymin = Lo, ymax = Hi, xmin = x - 0.5, xmax = x + 0.5), fill = "grey80") + 
  labs(title = "Pattern 13. Litter size at weaning", y = "Litter size [N]") +
  xlim(0,2) +
  ylim(0,6) +
  theme_classic() +
  theme(text = element_text(family = "Normal", size = 16, color = "grey10"),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.5))
p13.plot


####  Pattern 14. Probability of weaning by mother body mass  #### 

p14.plot <- read_excel("Data/Verification/Reproduction/JonssonetalData.xlsx")

p14.plot <- p14.plot %>% 
  mutate(p14.hi = prob + 0.25, p14.lo = prob - 0.25) %>% 
  mutate(p14.hi = ifelse(p14.hi > 1, 1, p14.hi), p14.lo = ifelse(p14.lo < 0, 0, p14.lo))


p14.plot <- 
  ggplot(p14.plot, aes(x = mass)) +
  geom_ribbon(aes(ymin = p14.lo, ymax = p14.hi), fill = "grey80") + 
  labs(title = "Pattern 14. Probability of weaning by body mass", y = "Probability of weaning", x = "Mother body mass [g]") +
#  ylim(0,6) +
  theme_classic() +
  theme(text = element_text(family = "Normal", size = 16, color = "grey10"),
        plot.title=element_text(hjust=0.5))
p14.plot

####  Pattern 15. Average / range of body fat %  #### 

p15.plot <- tibble(x = 1, Hi = 29, Lo = 3)

p15.plot <- 
  ggplot(p15.plot, aes(x = x)) +
  geom_rect(aes(ymin = Lo, ymax = Hi, xmin = x - 0.5, xmax = x + 0.5), fill = "grey80") + 
  labs(title = "Pattern 15. Range of percent body fat", y = "Body fat [%]") +
  xlim(0,2) +
  ylim(0,40) +
  theme_classic() +
  theme(text = element_text(family = "Normal", size = 16, color = "grey10"),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.5))
p15.plot


####  Pattern 16. Body fat % of animals at death  #### 

p16.plot <- tibble(x = 1, Hi = 3)

p16.plot <- 
  ggplot(p16.plot, aes(x = x)) +
  geom_rect(aes(ymin = 0, ymax = Hi, xmin = x - 0.5, xmax = x + 0.5), fill = "grey80") + 
  labs(title = "Pattern 16. Percent body fat at death", y = "Body fat [%]") +
  xlim(0,2) +
  ylim(0,8) +
  theme_classic() +
  theme(text = element_text(family = "Normal", size = 16, color = "grey10"),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.5))
p16.plot



####  Pattern 17. Field metabolic rate by body mass #### 


p17.plot <- tibble(
  mass = seq(1,60,1),
  Speakman1999 = exp(1.878) * (mass * 1000)^0.66,   
  Nagy1999 = 5.48 * mass^0.71)

p17.plot <- p17.plot %>% 
  mutate(avgFMR = (Speakman1999 + Nagy1999) / 2) %>% 
  mutate(p17.hi = avgFMR * 1.25, p17.lo = avgFMR * 0.75) %>% 
  select(mass, p17.hi, p17.lo)


p17.plot <- ggplot(p17.plot, aes(x = mass)) +
  geom_ribbon(aes(ymin = p17.lo, ymax = p17.hi), fill = "grey80") + 
  labs(title = "Pattern 17. Field metabolic rate by body mass", y = "Field metabolic rate [kJ day-1]", x = "Body mass [g]") +
  theme_classic() +
  theme(text = element_text(family = "Normal", size = 16, color = "grey10"),
        plot.title=element_text(hjust=0.5))
p17.plot


#------------------------------------------------#
#### All together ####

fullPlot <- 
  (p1.plot | p2.plot | p3.plot | p4.plot | p5.plot) / 
  (p6.plot | p7.plot | p8.plot | p9.plot | p10.plot) / 
  (p11.plot | p12.plot | p13.plot | p14.plot | p15.plot) /
  (p16.plot | p17.plot | plot_spacer() | plot_spacer() | plot_spacer() )

ggsave("Figures/FullCalPlot.png", fullPlot, width = 10.5, height = 6.25)









