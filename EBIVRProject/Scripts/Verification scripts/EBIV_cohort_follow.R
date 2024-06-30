# Cara Gallagher
# March 7th, 2023
# Energy Budget with Individual Variation project
# Cohort follow for model verification

##################################################
# Packages:
library(tidyverse)
library(patchwork)
library(showtext)


font_add_google(name = "Montserrat", family = "Montserrat")
showtext_auto()

##################################################

nam <- c(as.character(1:1000000))

modOut <- read_csv("Data/Verification/EBIV_Cohort_Follow.csv", skip = 6)

#### Survivorship curve #####

survOut <- modOut %>%  
  select('[run number]', 'age-list') %>% 
  rename(runNum = '[run number]', ages = 'age-list')


survOut <- survOut %>% 
  separate(ages, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -runNum) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  arrange(runNum, num) %>% 
  drop_na() %>% 
  group_by(runNum, value) %>% 
  select(-num) %>% 
  summarise(n = n()) %>% 
  filter(value > 4) %>%
  mutate(perc = n/max(n)) 

pal1 <- colorRampPalette(c("#007057","#00B089","#7FD7C4"))
pal1 <- sample(pal1(100))

survPlot <- ggplot(survOut,aes(x = value, y = perc)) +
  geom_line(aes(col = as.factor(runNum)), size= 1, alpha = 0.5) +
  geom_smooth(method = "loess", size = 2, col = "black", span = 0.5, se = FALSE) +
  scale_color_manual(values = pal1, guide = "none") +
  scale_y_continuous(labels = c(0,25,50,75,100)) +
  labs(x = "Age [days]", y = "Survivorship [%]") +
  theme_classic() +
  theme(text = element_text(size = 50, color = "grey30", family = "Montserrat"),
        axis.line = element_line(color = "grey30"))
survPlot

#### Mass and energy use ####

massEuseOut <- modOut %>%  
  select('[run number]', 'cohort-list') %>% 
  rename(runNum = '[run number]', coOut = 'cohort-list')

massEuseOut <- massEuseOut %>% 
  separate(coOut, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -runNum) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  arrange(runNum, num) %>% 
  drop_na() %>% 
  group_by(runNum) %>% 
  mutate(order = rep(1:9, times = (max(row_number()) / 9)), num = rep(1:(max(row_number()) / 9), each = 9)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(age = `1`, leanMass = `2`, mass = `3`, BMR = `4`, move = `5`, growth = `6`, preg = `7`, lact = `8`, HIF = `9`) %>% 
  ungroup() %>% 
  select(-c(runNum, num)) 

### mass and lean mass 

massOut <- massEuseOut %>%  
  select(age,leanMass,mass) %>% 
  group_by(age) %>% 
  summarise(meanLM = mean(leanMass), meanM = mean(mass)) %>% 
  pivot_longer(cols = c(meanLM,meanM)) 

pal <- c("#AABC4A","#278192")

massPlot <- ggplot(massOut,aes(x = age, y = value, col = name)) +
  geom_smooth(size= 1.5, method = "loess", span = 0.1, se = FALSE) +
  scale_color_manual(values = pal, labels = c("Lean mass", "Total body mass")) +
  labs(x = "Age [days]", y = "Mass [kg]", color = element_blank()) +
  theme_classic() +
  theme(text = element_text(size = 50, color = "grey30", family = "Montserrat"),
        axis.line = element_line(color = "grey30"),
        legend.position = c(0.22, 0.95),
        legend.background = element_blank(),
        legend.spacing.x = unit(0.2, 'cm'))
massPlot


### energy use

EuseOut <- massEuseOut %>% 
  select(-leanMass,-mass) %>% 
  group_by(age) %>% 
  summarise(mBMR = mean(BMR), mMove = mean(move), mGrowth = mean(growth), mPreg = mean(preg), mLact = mean(lact), mHIF = mean(HIF)) %>%
 # mutate(mMove = mMove + mBMR, mGrowth = mGrowth + mMove, mPreg = mPreg + mGrowth, mLact = mLact + mPreg, mHIF = mHIF + mLact) %>% 
  pivot_longer(cols = c(mBMR,mMove,mGrowth,mPreg,mLact,mHIF)) 

EuseOut$name <- factor(EuseOut$name , levels=rev(c("mBMR","mMove","mGrowth","mPreg","mLact","mHIF")))

pal2 <- rev(c("#BC4A53","#E4714E","#AABC4A","#D79121","#E4AA4E","#3F3F7B"))

EusePlot <- ggplot(EuseOut,aes(x = age, y = value, fill = name)) +
  geom_area(alpha=0.9, size=.6, colour="white") +
  scale_fill_manual(values = pal2, labels = rev(c("BMR", "Activity", "Lean mass deposition", "Pregnancy", "Lactation", "HIF"))) +
  labs(x = "Age [days]", y = "Energy use [J per 30min]", fill = element_blank()) +
  theme_classic() +
  theme(text = element_text(size = 50, color = "grey30", family = "Montserrat"),
        axis.text.y = element_text(lineheight = 0.1),
        axis.line = element_line(color = "grey30"),
        legend.position = c(0.13, 0.8),
        legend.background = element_blank(),
        legend.spacing.x = unit(0.2, 'cm'))
EusePlot


#### all together
allPlots <- (massPlot + survPlot) / EusePlot + 
  plot_annotation(tag_levels = 'A')  &
  theme(plot.tag = element_text(family = "Montserrat"))
allPlots

#ggsave("Figures/Verification/CohortPlotOut.png", allPlots , width = 11, height = 8)








