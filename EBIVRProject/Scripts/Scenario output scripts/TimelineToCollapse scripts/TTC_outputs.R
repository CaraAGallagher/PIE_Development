# Cara Gallagher
# April 26th, 2023
# Energy Budget with Individual Variation project
# Test of the Timeline-to-collapse concept presented in Cerini et al. 2023
# Output analysis

#------------------------------------------------#
####  Packages: #### 
library(tidyverse)
library(patchwork)

#------------------------------------------------#


results <- read_csv("Data/Scenarios/TimelineToCollapse/TTCtest3.csv", skip = 6)

results <- results %>%
  select('[run number]', '[step]', 'TTC-max', 'TTC-res', 'TTC-behav', 'TTC-morph', 'TTC-SL', 'TTC-LH', 'TTC-abun') %>% 
  rename("run_number" = '[run number]', "step" = '[step]', "TTC_max" = 'TTC-max', "TTC_res" = 'TTC-res', "TTC_behav" = 'TTC-behav', "TTC_morph" = 'TTC-morph', "TTC_SL" ='TTC-SL', 'TTC_LH' ='TTC-LH', "TTC_abun" = 'TTC-abun')

nam <- c(as.character(1:300))

results <- results %>% 
  filter(TTC_max %in% c(0.1, 1))
#### Resources #### 

resources <- results %>%  select(run_number, TTC_max, TTC_res)

resources <- resources %>% 
  separate(TTC_res, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run_number, -TTC_max) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  drop_na() %>% 
  group_by(TTC_max, num) %>% 
  summarise(mean = mean(value), sd = sd(value))

# rPlot <- ggplot(resources, aes(x = num, y = value, col = as.factor(TTC_max), fill = as.factor(TTC_max))) + 
#   geom_smooth(method = "gam", level = 1-1e-10) +
#   labs(y = "Resource abundance [g]", x = NULL, col = "Resource modifier", fill = "Resource modifier") +
#   theme_classic()
#rPlot 

rPlot <- ggplot(resources, aes(x = num, y = mean)) + 
 # geom_ribbon(aes(ymax = mean + sd, ymin = mean - sd, fill = as.factor(TTC_max)), alpha = 0.25) +
  geom_line(aes(col = as.factor(TTC_max)), size = 1.5) +
  labs(y = "Resource abundance [g]", x = NULL, col = "Resource modifier", fill = "Resource modifier") +
  theme_classic()
#rPlot 

#### Behavior #### 

behavior <- results %>%  select(run_number, TTC_max, TTC_behav)

behavior <- behavior %>% 
  separate(TTC_behav, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run_number, -TTC_max) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  drop_na() %>% 
  group_by(TTC_max, num) %>% 
  summarise(mean = mean(value), sd = sd(value))

bPlot <- ggplot(behavior, aes(x = num, y = mean)) + 
 # geom_ribbon(aes(ymax = mean + sd, ymin = mean - sd, fill = as.factor(TTC_max)), alpha = 0.25) +
  geom_line(aes(col = as.factor(TTC_max)), size = 1.5) +
  labs(y = "Active time [Prop]", x = NULL, col = "Resource modifier", fill = "Resource modifier") +
  theme_classic()
#bPlot

#### Morphology #### 

morph <- results %>%  select(run_number, TTC_max, TTC_morph)

morph <- morph %>% 
  separate(TTC_morph, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run_number, -TTC_max) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  drop_na() %>% 
  group_by(TTC_max, num) %>% 
  summarise(mean = mean(value), sd = sd(value))

mPlot <- ggplot(morph, aes(x = num, y = mean)) + 
 # geom_ribbon(aes(ymax = mean + sd, ymin = mean - sd, fill = as.factor(TTC_max)), alpha = 0.25) +
  geom_line(aes(col = as.factor(TTC_max)), size = 1.5) +
  labs(y = "Body mass [kg]", x = NULL, col = "Resource modifier", fill = "Resource modifier") +
  theme_classic()
#mPlot





morph2 <- results %>%  select(run_number, TTC_max, TTC_SL)

morph2 <- morph2 %>% 
  separate(TTC_SL, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run_number, -TTC_max) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  drop_na() %>% 
  group_by(TTC_max, num) %>% 
  summarise(mean = mean(value), sd = sd(value))

mPlot <- ggplot(morph2, aes(x = num, y = mean)) + 
 # geom_ribbon(aes(ymax = mean + sd, ymin = mean - sd, fill = as.factor(TTC_max)), alpha = 0.25) +
  geom_line(aes(col = as.factor(TTC_max)), size = 1.5) +
  labs(y = "Storage level [Prop]", x = NULL, col = "Resource modifier", fill = "Resource modifier") +
  theme_classic()
#mPlot


#### Life history #### 

lifehistory <- results %>%  select(run_number, TTC_max, TTC_LH)

lifehistory <- lifehistory %>% 
  separate(TTC_LH, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run_number, -TTC_max) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  drop_na() %>% 
  group_by(TTC_max, num) %>% 
  summarise(mean = mean(value), sd = sd(value))

lhPlot <- ggplot(lifehistory, aes(x = num, y = mean)) + 
#  geom_ribbon(aes(ymax = mean + sd, ymin = mean - sd, fill = as.factor(TTC_max)), alpha = 0.25) +
  geom_line(aes(col = as.factor(TTC_max)), size = 1.5) +
  labs(y = "Total offspring [N]", x = NULL, col = "Resource modifier", fill = "Resource modifier") +
  theme_classic()
#lhPlot


#### Abundance #### 

abundance <- results %>%  select(run_number, TTC_max, TTC_abun)

abundance <- abundance %>% 
  separate(TTC_abun, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run_number, -TTC_max) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  drop_na() %>% 
  group_by(TTC_max, num) %>% 
  summarise(mean = mean(value), sd = sd(value))

aPlot <- ggplot(abundance, aes(x = num, y = mean)) + 
 # geom_ribbon(aes(ymax = mean + sd, ymin = mean - sd, fill = as.factor(TTC_max)), alpha = 0.25) +
  geom_line(aes(col = as.factor(TTC_max)), size = 1.5) +
  labs(y = "Abundance [N]", x = "Day", col = "Resource modifier", fill = "Resource modifier") +
  theme_classic()
#aPlot


#### All together #### 
rPlot / bPlot / mPlot / lhPlot / aPlot + plot_layout(guides = "collect") & theme(legend.position = 'top')

