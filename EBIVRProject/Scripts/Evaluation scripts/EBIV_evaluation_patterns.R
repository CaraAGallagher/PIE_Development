# Cara Gallagher
# February 1st, 2023
# Energy Budget with Individual Variation project
# Evaluation of model outputs

#------------------------------------------------#
####  Packages: #### 
library(tidyverse)
library(patchwork)
library(showtext)


font_add_google(name = "Montserrat", family = "Montserrat")
showtext_auto()



#### Evaluation patterns ####

EvalOuts <- read.csv("Data/Evaluation/EvaluationOutputs.csv", skip = 6)

EvalOuts <- EvalOuts %>%  select(c(1,18:28))
colnames(EvalOuts) <- c("run.num", "p17.out", "p18.out", "p19.out", "p20.out", "p21.out", "p22.out", "p23.out", "p24.out", "p25.out", "p26.out", "p27.out")

nam <- c(as.character(1:10000))

#pal <- natparks.pals(name="Denali",n=6,type="continuous")


pal <- colorRampPalette(c("#3F3F7B","#278192","#00B089","#AABC4A", "#E5DC54"))
pal <- pal(7)
pal <- pal[2:7]
textSizeAxisTitle <- 40
textSizeAxisText <- 30

####  Pattern 17. State-dependent field metabolic rate  #### 
# Qualitative 
# Compare visually
# Data for nonreproducing and lactating animals from: 
## Peacock et al. 2004
## Sadowska et al. 2016
## Górecki 1968
## Tidhar et al. 2007
## Rutkowska et al. 2011

p17 <- EvalOuts %>%  select(run.num, p17.out)


p17 <- p17 %>% 
  separate(p17.out, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  arrange(run.num, num) %>% 
  drop_na() %>% 
  group_by(run.num) %>% 
  mutate(order = rep(1:2, each = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), times = 2)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(state = `1`, FMR = `2`) %>% 
  mutate(FMR = FMR / 1000) %>% 
  select(state, FMR) %>% 
  mutate(state = ifelse(state == 1, "Juvenile", ifelse(state == 2, "Nonreproducing", ifelse(state == 3, "Pregnant", ifelse(state == 4, "Lactating", "Lactating and pregnant")))),
       state = fct_relevel(state, "Juvenile", "Nonreproducing", "Pregnant", "Lactating", "Lactating and pregnant"))


p17.pattern <- tibble(
  mass = c(20.0, 23.8, 23.4, 25.7, 26.0, 26.8, 27.2, 27.3, 27.0, 27.1, 26.7, 28.7, 28.3, 28.4, 28.4, 28.8, 28.3, 27.9, 29.3, 30.0, 29.9, 29.7, 29.4, 29.2, 30.4, 30.8, 30.6, 30.0, 30.3, 31.2, 31.1, 34.3, 33.4, 32.5, 32.4, 33.3, 33.6, 33.1, 34.1, 36.0, 36.0, 35.4, 37.1),
  FMR = c(45.3, 57.1, 79.9, 62.2, 67.6, 66.2, 69.4, 72.6, 72.4, 76.6, 80.3, 94.7, 84.7, 76.7, 71.3, 71.8, 64.7, 61.9, 67.0, 70.7, 73.4, 76.7, 76.1, 78.3, 81.4, 80.6, 86.4, 88.1, 102.8, 95.8, 72.7, 56.2, 69.8, 85.9, 87.2, 90.7, 90.7, 103.3, 108.1, 121.6, 98.2, 89.9, 91.7))
p17.pattern <- tibble(
  mean = c(88.8, 70.9, 57, 97, ((4.07 * 20.1 * 23.9 * 24) / 1000), ((4.29 * 20.1 * 23.7 * 24) / 1000), ((3.79 * 20.1 * 19.3 * 24) / 1000), ((3.65 * 20.1 * 19.8 * 24) / 1000), 66.44, mean(p17.pattern$FMR)),
  error = c(5.13, 3.95, 3, 1.7, ((0.62 * 20.1 * 23.9 * 24) / 1000), ((0.64 * 20.1 * 23.7 * 24) / 1000), ((0.36 * 20.1 * 19.3 * 24) / 1000), ((0.53 * 20.1 * 19.8 * 24) / 1000), 7.8, sd(p17.pattern$FMR)),
  state = c("Nonreproducing", "Nonreproducing", "Nonreproducing", "Lactating", "Nonreproducing", "Nonreproducing", "Nonreproducing", "Nonreproducing", "Nonreproducing", "Lactating"),
  source = c("Peacock et al. 2004", "Peacock et al. 2004", "Sadowska et al. 2016", "Sadowska et al. 2016", "Górecki 1968", "Górecki 1968", "Górecki 1968", "Górecki 1968", "Tidhar et al. 2007", "Rutkowska et al. 2011"))


p17.plot <- ggplot() +
  geom_violin(data = p17, aes(x = state, y = FMR, col = state, fill = state), size = 0.9, alpha = 0.25) +
  geom_boxplot(data = p17, aes(x = state, y = FMR, col = state, fill = state), lwd = 0.9, fill = "white", width = 0.1, alpha = 0.5) +
  geom_linerange(data = p17.pattern, aes(x = state, y = mean, group = mean, ymin = mean - error, ymax = mean + error), col = "grey30", position = position_dodge(width = 0.75), size = 1) +
  geom_point(data = p17.pattern, aes(x = state, y = mean, group = mean, shape = source), col = "grey30", fill = "white", position = position_dodge(width = 0.75), size = 2.5, stroke = 1.1) +
  scale_color_manual(values = pal, guide = "none") +
  scale_fill_manual(values = pal, guide = "none") +
  scale_shape_manual(values = c(21,8,25,18,23)) +
  theme_classic() +
  ylim(0, 210) +
  labs(y = "Field metabolic rate\n[kJ day-1]", x = NULL, shape = NULL) +
  theme(legend.position = c(0.5, 0.95),
        legend.direction = "horizontal",
        legend.background = element_rect(fill = NA),
        text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat", lineheight = 0.35),
        axis.line = element_line(color = "grey30"))
p17.plot

####  Pattern 18. State-dependent food consumption  #### 
# Qualitative 
# Compare visually
# Data for all states except pregnant AND lactating animals from: 
## Piątkowska & Weiner 1987
## Kaczmarski 1966
## Meese 1971
## Peacock and Speakman 2001
## Sadowska et al. 2016



p18 <- EvalOuts %>%  select(run.num, p18.out)


p18 <- p18 %>% 
  separate(p18.out, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  arrange(run.num, num) %>% 
  drop_na() %>% 
  group_by(run.num) %>% 
  mutate(order = rep(1:2, each = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), times = 2)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(state = `1`, foodCons = `2`) %>% 
  mutate(foodCons = foodCons * 12.2811) %>% 
  select(state, foodCons) %>% 
  mutate(state = ifelse(state == 1, "Juvenile", ifelse(state == 2, "Nonreproducing", ifelse(state == 3, "Pregnant", ifelse(state == 4, "Lactating", "Lactating and pregnant")))),
         state = fct_relevel(state, "Juvenile", "Nonreproducing", "Pregnant", "Lactating", "Lactating and pregnant"))

p18.pattern <- tibble(
  mean = c(3.26 * 19.6, 81.55, 97.06, 100.18, 111.90, 124.29, 170.01, 184.14, 66.78, 58.49, 105.6, 50.5, 49.0, 90.07, 270.56),
  error = c(0.23 * 19.6, 6.19, 14.23, 9.79, 15.70, 20.42, 36.21, 42.17, NA, NA, 2.64, 1.06, 1.91, 3.56, 5.34),
  source = c("Piątkowska & Weiner 1987", "Kaczmarski 1966", "Kaczmarski 1966", "Kaczmarski 1966", "Kaczmarski 1966", "Kaczmarski 1966", "Kaczmarski 1966", "Kaczmarski 1966", "Meese 1971", "Meese 1971", "Peacock et al. 2004", "Peacock and Speakman 2001", "Peacock and Speakman 2001", "Sadowska et al. 2016", "Sadowska et al. 2016"),
  state = c("Nonreproducing", "Nonreproducing", "Pregnant", "Pregnant", "Pregnant", "Lactating", "Lactating", "Lactating", "Nonreproducing", "Nonreproducing", "Nonreproducing", "Juvenile", "Juvenile", "Nonreproducing", "Lactating"))
p18.pattern <- p18.pattern %>% 
  mutate(state = fct_relevel(state, "Juvenile", "Nonreproducing", "Pregnant", "Lactating"))

p18.plot <- ggplot() + 
  geom_violin(data = p18, aes(x = state, y = foodCons, col = state, fill = state), size = 0.9,alpha = 0.25, bw = 10) +
  geom_boxplot(data = p18, aes(x = state, y = foodCons, col = state, fill = state), lwd = 0.9, fill = "white", width = 0.1, alpha = 0.5) +
  geom_linerange(data = p18.pattern, aes(x = state, y = mean, group = mean, ymin = mean - error, ymax = mean + error), col = "grey30", position = position_dodge(width = 0.75), size = 1) +
  geom_point(data = p18.pattern, aes(x = state, y = mean, group = mean, shape = source), col = "grey30", position = position_dodge(width = 0.75), size = 2.5, stroke = 1.1) +
  scale_color_manual(values = pal, guide = "none") +
  scale_fill_manual(values = pal, guide = "none") +
  scale_shape_manual(values = c(19,17,15,8,3,18)) +
  ylim(0, 750) +
  theme_classic() +
  labs( x = NULL, y = "Daily food consumption\n[kJ day-1]", shape = NULL) +
  theme(legend.position = c(0.5, 0.9),
        legend.direction = "horizontal",
        legend.background = element_rect(fill = NA),
        text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat", lineheight = 0.35),
        axis.line = element_line(color = "grey30"))
p18.plot

####  Pattern 19. Survival rates  #### 
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
## 66.8% and 72.0% for mature and immature females, respectively, in the breeding period for 6 week survival in Buchalczyk 1970
## See Crespin et al. 2002 - Oikos
## Remember that often animals leaving the study area or not recaught are considered dead
## Survivorship curve in Gliwicz  - Acta Theriologica, 1983 -


p19 <- EvalOuts %>%  select(run.num, p19.out)


p19 <- p19 %>%
  separate(p19.out, nam, sep = " ") %>%
  gather(key = "num", value = "value",-run.num) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>%
  arrange(run.num, num) %>%
  drop_na() %>% 
  mutate(order = rep(1:3, times = (max(row_number()) / 3)), num = rep(1:(max(row_number()) / 3), each = 3)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(age = `1`, birthday = `2`, day = `3`) %>% 
  select(age, birthday, day)


p19 <- p19 %>% 
  group_by(age) %>% 
  summarise(n = n()) %>% 
  mutate(age = age + 1) 

age0 <- tibble(age = 0, n = sum(p19$n))

p19 <- bind_rows(age0, p19)

p19 <- p19 %>% 
  mutate(rem = NA) 

for (i in 2:nrow(p19)) {

    if (i == 2) {
    p19[i,3] <-  p19[i - 1,2] - p19[i,2] 
    
  } else {
    p19[i,3] <- p19[i - 1,3] - p19[i,2] 
  }
  
}

p19 <- p19 %>% 
  mutate(remPerc = (rem / age0$n)  * 100)

p19[1,4] <- 100

p19.pattern <- tibble(
  day = c(0,43,130,306,394,479),
  survPerc = c(100.0,	35.0,	14.8,	6.3,	3.3,	0.3),
  source = c("Gliwicz 1983","Gliwicz 1983","Gliwicz 1983","Gliwicz 1983","Gliwicz 1983","Gliwicz 1983"))



p19.plot <- ggplot() + 
  geom_line(data = p19, aes(x = age, y = remPerc), col = pal[2], size = 1) +
  geom_line(data = p19.pattern, aes(x = day, y = survPerc), col = "grey30", size = 1) +
  theme_classic() +
  labs( x = "Age [days]", y = "Survival [%]", shape = NULL ) +
  theme(text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"),
        axis.line = element_line(color = "grey30"))
p19.plot


# From PanTHERIA 

library(traitdata)
data("pantheria")

#smallMams <- pantheria %>% filter(AdultBodyMass_g <= 60 & Order == "Rodentia")
patheriaVoles <- pantheria %>% filter(Species == "glareolus")

# glossary: trait_glossary[which(trait_glossary$datasetName == "pantheria"),]

####  Pattern 20. Adult body mass  #### 
# from PanTHERIA database:

p20 <- EvalOuts %>%  select(run.num, p20.out)

p20 <- p20 %>% 
  separate(p20.out, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         num = as.numeric(num), value = as.numeric(value)) %>% 
  drop_na() %>% 
  select(value) %>% 
  rename(BM = value)

p20.pattern <- patheriaVoles$AdultBodyMass_g

p20.plot <- ggplot(p20, aes(x = 0.5, y = BM)) + 
  geom_flat_violin(position = position_nudge(x = .1), size = 0.9, fill = pal[2], col = pal[2], alpha = 0.25, bw = 3) +
  geom_boxplot(aes(x = 0.6), lwd = 0.9, fill = "white",  col = pal[2], width = 0.05, alpha = 0.5) + 
  geom_hline(yintercept = p20.pattern,  col = "grey30", size = 1) +
  geom_point(x = 0.6, y = mean(p20$BM), col = pal[2], size = 3, stroke = 1.1, shape = 5) + 
  coord_flip() +
  theme_classic() +
  labs( x = "Density", y = "Adult body mass [g]", shape = NULL ) +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line = element_line(color = "grey30"),
        text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"))
p20.plot

####  Pattern 21. Age at first birth  #### 
# from PanTHERIA database:


p21 <- EvalOuts %>%  select(run.num, p21.out)

p21 <- p21 %>% 
  separate(p21.out, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         num = as.numeric(num), value = as.numeric(value)) %>% 
  drop_na() %>% 
  select(value) %>% 
  rename(afb = value)

p21.pattern <- patheriaVoles$AgeatFirstBirth_d

p21.plot <- ggplot(p21, aes(x = 0.5, y = afb)) + 
  geom_flat_violin(position = position_nudge(x = .1), size = 0.9, fill = pal[2], col =  pal[2], alpha = 0.25, bw = 3) +
  geom_boxplot(aes(x = 0.6), lwd = 0.9, fill = "white",  col = pal[2], width = 0.05, alpha = 0.5) + 
  geom_hline(yintercept = p21.pattern, col = "grey30", size = 1) +
  geom_point(x = 0.6, y = mean(p21$afb), col = pal[2], size = 3, stroke = 1.1, shape = 5) + 
  coord_flip() +
  theme_classic() +
  labs( x = "Density", y = "Age at first birth [days]", shape = NULL ) +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line = element_line(color = "grey30"),
        text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"))
p21.plot

####  Pattern 22. Basal metabolic rate  #### 
# in J per 30 min per g
# from PanTHERIA database:

p22 <- EvalOuts %>%  select(run.num, p22.out)

p22 <- p22 %>% 
  separate(p22.out, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  arrange(run.num, num) %>% 
  drop_na() %>% 
  group_by(run.num) %>% 
  mutate(order = rep(1:2, times = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), each = 2)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(BMR = `1`, mass = `2`) %>% 
  ungroup() %>% 
  select(BMR, mass) %>% 
  mutate(BMRmass = BMR / mass)

p22.pattern <- ((patheriaVoles$BasalMetRate_mLO2hr / 2) * 20.1) / patheriaVoles$BasalMetRateMass_g

p22.plot <- ggplot(p22, aes(x = 0.5, y = BMRmass)) + 
  geom_flat_violin(position = position_nudge(x = .1), size = 0.9, fill = pal[2], col = pal[2], alpha = 0.25, bw = 0.25) +
  geom_boxplot(aes(x = 0.6), lwd = 0.9, fill = "white",  col = pal[2], width = 0.05, alpha = 0.5) + 
  geom_hline(yintercept = p22.pattern, col = "grey30", size = 1) +
  geom_point(x = 0.6, y = mean(p22$BMRmass), col = pal[2], size = 3, stroke = 1.1, shape = 5) + 
  coord_flip() +
  theme_classic() +
  labs( x = "Density", y = "BMR [J g-1 30min-1]", shape = NULL ) +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line = element_line(color = "grey30"),
        text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"))
p22.plot

####  Pattern 23. Litter size  #### 
# from PanTHERIA database:


p23 <- EvalOuts %>%  select(run.num, p23.out)

p23 <- p23 %>% 
  separate(p23.out, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         num = as.numeric(num), value = as.numeric(value)) %>% 
  drop_na() %>% 
  select(value) %>% 
  rename(LS = value)

p23.pattern <- patheriaVoles$LitterSize

p23.plot <- ggplot(p23, aes(x = 0.5, y = LS)) + 
  geom_flat_violin(position = position_nudge(x = .1), size = 0.9, fill = pal[2], col = pal[2], alpha = 0.25, bw = 0.75) +
  geom_boxplot(aes(x = 0.6), lwd = 0.9, fill = "white",  col = pal[2], width = 0.05, alpha = 0.5) + 
  geom_hline(yintercept = p23.pattern, col = "grey30", size = 1) +
  geom_point(x = 0.6, y = mean(p23$LS), col = pal[2], size = 3, stroke = 1.1, shape = 5) + 
  coord_flip() +
  theme_classic() +
  scale_y_continuous(breaks = seq(1,9,1)) +
  labs( x = "Density", y = "Litter size at birth [N]", shape = NULL ) +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line = element_line(color = "grey30"),
        text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"))
p23.plot

####  Pattern 24. Litters per year  #### 
# from PanTHERIA database:

p24 <- EvalOuts %>%  select(run.num, p24.out)


p24 <- p24 %>% 
  separate(p24.out, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  arrange(run.num, num) %>% 
  drop_na() %>% 
  group_by(run.num) %>% 
  mutate(order = rep(1:2, times = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), each = 2)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(age = `1`, Nlitters = `2`) %>% 
  ungroup() %>% 
  select(age, Nlitters) %>% 
#  filter(age > 300) %>% 
  mutate(class = ifelse(age > 300, "Year 2", "Year 1"))

p24.pattern <- patheriaVoles$LittersPerYear

p24.plot <- ggplot(p24, aes(x = class, y = Nlitters)) + 
  geom_flat_violin(position = position_nudge(x = 0), size = 0.9, fill = pal[2], col = pal[2], alpha = 0.25, bw = 0.25) +
  geom_boxplot(aes(x = class), lwd = 0.9, fill = "white",  col = pal[2], width = 0.1, alpha = 0.5) + 
  geom_hline(yintercept = p24.pattern, col = "grey30", size = 1) +
 # geom_hline(yintercept = mean(p24$Nlitters), col = pal[2], size = 1) + 
  coord_flip() +
  theme_classic() +
  scale_x_discrete(expand=c(0.1, 0)) +
  labs( x = "Density", y = "Litters per year [N]", shape = NULL ) +
  theme(text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"),
        axis.line = element_line(color = "grey30"),
        axis.title.y=element_blank())
p24.plot


####  Pattern 25. Neonate body mass  #### 
# from PanTHERIA database:


p25 <- EvalOuts %>%  select(run.num, p25.out)

p25 <- p25 %>% 
  separate(p25.out, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         num = as.numeric(num), value = as.numeric(value)) %>% 
  drop_na() %>% 
  select(value) %>% 
  rename(BMn = value)

p25.pattern <- patheriaVoles$NeonateBodyMass_g

p25.plot <- ggplot(p25, aes(x = 0.5, y = BMn)) + 
  geom_flat_violin(position = position_nudge(x = .1), size = 0.9, fill = pal[2], col = pal[2], alpha = 0.25, bw = 0.1) +
  geom_boxplot(aes(x = 0.6), lwd = 0.9, fill = "white",  col = pal[2], width = 0.05, alpha = 0.5) + 
  geom_hline(yintercept = p25.pattern, col = "grey30", size = 1) +
  geom_point(x = 0.6, y = mean(p25$BMn), col = pal[2], size = 3, stroke = 1.1, shape = 5) + 
  coord_flip() +
  theme_classic() +
  labs( x = "Density", y = "Neonate mass [g]", shape = NULL ) +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line = element_line(color = "grey30"),
        text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"))
p25.plot

####  Pattern 26. Weaning body mass  #### 
# from PanTHERIA database:

p26 <- EvalOuts %>%  select(run.num, p26.out)

p26 <- p26 %>% 
  separate(p26.out, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         num = as.numeric(num), value = as.numeric(value)) %>% 
  drop_na() %>% 
  select(value) %>% 
  rename(BMw = value)

p26.pattern <- patheriaVoles$WeaningBodyMass_g

p26.plot <- ggplot(p26, aes(x = 0.5, y = BMw)) + 
  geom_flat_violin(position = position_nudge(x = .1), size = 0.9, fill = pal[2], col = pal[2], alpha = 0.25, bw = 1) +
  geom_boxplot(aes(x = 0.6), lwd = 0.9, fill = "white", col = pal[2], width = 0.05, alpha = 0.5) + 
  geom_hline(yintercept = p26.pattern, col = "grey30", size = 1) +
  geom_point(x = 0.6, y = mean(p26$BMw), col = pal[2], size = 3, stroke = 1.1, shape = 5) + 
  coord_flip() +
  theme_classic() +
  labs( x = "Density", y = "Weaning mass [g]", shape = NULL ) +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line = element_line(color = "grey30"),
        text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"))
p26.plot

####  Pattern 27. Population density  #### 
# Individuals per hectare, divided by 2 for 1:1 female to male ratio
# from PanTHERIA database:

p27 <- EvalOuts %>%  select(run.num, p27.out)

p27 <- p27 %>% 
  separate(p27.out, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         num = as.numeric(num), value = as.numeric(value)) %>% 
  drop_na() %>% 
  select(value) %>% 
  rename(dens = value)

p27.pattern <- patheriaVoles$PopulationDensity_n.km2 / 100 / 2

p27.plot <- ggplot(p27, aes(x = 0.5, y = dens)) + 
  geom_flat_violin(position = position_nudge(x = .1), size = 0.9, fill = pal[2], col = pal[2], alpha = 0.25, bw = 1) +
  geom_boxplot(aes(x = 0.6), lwd = 0.9, fill = "white",  col = pal[2], width = 0.05, alpha = 0.5) + 
  geom_hline(yintercept = p27.pattern, col = "grey30", size = 1) +
  geom_point(x = 0.6, y = mean(p27$dens), col = pal[2], size = 3, stroke = 1.1, shape = 5) + 
  coord_flip() +
  theme_classic() +
  labs( x = "Density", y = "Population density [Females ha-1]", shape = NULL ) +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line = element_line(color = "grey30"),
        text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"))
p27.plot


#### All together ####

fullPlot <- 
  p17.plot /
  p18.plot /
  (p19.plot | p20.plot | p21.plot) /
  (p22.plot | p23.plot | p24.plot) /
  (p25.plot | p26.plot | p27.plot) + 
  plot_layout(heights = c(2,2,1,1,1)) + 
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(color = "grey30", family = "Montserrat"))
#fullPlot

ggsave("Figures/EvalOuts/fullOutPlotUpd.png", fullPlot, height = 12, width = 10)

# MS figure
fullPlot <- 
  p17.plot /
  p18.plot /
  (p19.plot | p27.plot) + 
  plot_layout(heights = c(2,2,1.5)) + 
  plot_annotation(tag_levels = 'A', tag_suffix = ' ') & 
  theme(plot.tag = element_text(color = "grey30", family = "Montserrat"))
#fullPlot

ggsave("Figures/App 1/Figure3.png", fullPlot, height = 10, width = 10)


#### geom_flat_violin ####


"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

geom_flat_violin <-
  function(mapping = NULL,
           data = NULL,
           stat = "ydensity",
           position = "dodge",
           trim = TRUE,
           scale = "area",
           show.legend = NA,
           inherit.aes = TRUE,
           ...) {
    ggplot2::layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomFlatViolin,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(trim = trim,
                    scale = scale,
                    ...)
    )
  }

#'geom_flat_violin_HELPER2
#'
#' Borrowed from
#' \href{https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R}{Ben Marwick}.
#' Original author David Robinson.
#'
#' @format NULL
#' @usage NULL
GeomFlatViolin <-
  ggplot2::ggproto(
    "GeomFlatViolin",
    ggplot2::Geom,
    setup_data = function(data, params) {
      data$width <- data$width %||%
        params$width %||% (ggplot2::resolution(data$x, FALSE) * 0.9)
      
      # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
      data %>%
        dplyr::group_by(.data = ., group) %>%
        dplyr::mutate(
          .data = .,
          ymin = min(y),
          ymax = max(y),
          xmin = x,
          xmax = x + width / 2
        )
    },
    
    draw_group = function(data, panel_scales, coord)
    {
      # Find the points for the line to go all the way around
      data <- base::transform(data,
                              xminv = x,
                              xmaxv = x + violinwidth * (xmax - x))
      
      # Make sure it's sorted properly to draw the outline
      newdata <-
        base::rbind(
          dplyr::arrange(.data = base::transform(data, x = xminv), y),
          dplyr::arrange(.data = base::transform(data, x = xmaxv), -y)
        )
      
      # Close the polygon: set first and last point the same
      # Needed for coord_polar and such
      newdata <- rbind(newdata, newdata[1,])
      
      ggplot2:::ggname("geom_flat_violin",
                       GeomPolygon$draw_panel(newdata, panel_scales, coord))
    },
    
    draw_key = ggplot2::draw_key_polygon,
    
    default_aes = ggplot2::aes(
      weight = 1,
      colour = "grey20",
      fill = "white",
      size = 0.5,
      alpha = NA,
      linetype = "solid"
    ),
    
    required_aes = c("x", "y")
  )






