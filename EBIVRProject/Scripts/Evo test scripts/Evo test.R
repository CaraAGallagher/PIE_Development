# Cara Gallagher
# September 6th, 2022
# Energy Budget with Individual Variation project
# Testing evolutionary outputs

##################################################
# Packages:
library(tidyverse)
library("LaCroixColoR")
library(patchwork)


##################################################

BMR1 <- read_csv("Data/Evo testing/BMR_1.csv", skip = 17)

BMR1 <- BMR1 %>% 
  select(x...1, y...2, y...6) %>% 
  rename(tick = x...1, mean = y...2, sd = y...6)  %>% 
  mutate(metric = "BMR")

Growth1 <- read_csv("Data/Evo testing/Growth_1.csv", skip = 17)

Growth1 <- Growth1 %>% 
  select(x...1, y...2, y...6) %>% 
  rename(tick = x...1, mean = y...2, sd = y...6) %>% 
  mutate(metric = "Growth")

Repro1 <- read_csv("Data/Evo testing/Repro_1.csv", skip = 17)

Repro1 <- Repro1 %>% 
  select(x...1, y...2, y...6) %>% 
  rename(tick = x...1, mean = y...2, sd = y...6)  %>% 
  mutate(metric = "Reproduction")

All1 <- bind_rows(BMR1, Growth1, Repro1)


plot1 <- ggplot(All1, aes(x = tick, y = mean)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = metric), alpha = 0.25) +
  geom_line(size = 1.5, aes(col = metric))+
  theme_classic() +
  ylim(0,2) +
  scale_color_manual(values=lacroix_palette("PassionFruit",type = "discrete", n=3)) +
  scale_fill_manual(values=lacroix_palette("PassionFruit",type = "discrete", n=3)) +
  scale_x_continuous(labels = c(0,25,50,75,100)) +
  labs( x = "Years", y = "Trait value", title = "1% variance", col = NULL, fill = NULL) + 
  theme(plot.title = element_text(hjust = 0.5))
plot1

##################################################


BMR5 <- read_csv("Data/Evo testing/BMR_5.csv", skip = 17)

BMR5 <- BMR5 %>% 
  select(x...1, y...2, y...6) %>% 
  rename(tick = x...1, mean = y...2, sd = y...6)  %>% 
  mutate(metric = "BMR")

Growth5 <- read_csv("Data/Evo testing/Growth_5.csv", skip = 17)

Growth5 <- Growth5 %>% 
  select(x...1, y...2, y...6) %>% 
  rename(tick = x...1, mean = y...2, sd = y...6) %>% 
  mutate(metric = "Growth")

Repro5 <- read_csv("Data/Evo testing/Repro_5.csv", skip = 17)

Repro5 <- Repro5 %>% 
  select(x...1, y...2, y...6) %>% 
  rename(tick = x...1, mean = y...2, sd = y...6)  %>% 
  mutate(metric = "Reproduction")

All5 <- bind_rows(BMR5, Growth5, Repro5)
  

plot5 <- ggplot(All5, aes(x = tick, y = mean)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = metric), alpha = 0.25) +
  geom_line(size = 1.5, aes(col = metric))+
  theme_classic() +
  ylim(0,2) +
  scale_color_manual(values=lacroix_palette("PassionFruit",type = "discrete", n=3)) +
  scale_fill_manual(values=lacroix_palette("PassionFruit",type = "discrete", n=3)) +
  scale_x_continuous(labels = c(0,25,50,75,100)) +
  labs( x = "Years", y = "Trait value", title = "5% variance", col = NULL, fill = NULL) + 
  theme(plot.title = element_text(hjust = 0.5))
plot5

##################################################


BMR10 <- read_csv("Data/Evo testing/BMR_10.csv", skip = 17)

BMR10 <- BMR10 %>% 
  select(x...1, y...2, y...6) %>% 
  rename(tick = x...1, mean = y...2, sd = y...6)  %>% 
  mutate(metric = "BMR")

Growth10 <- read_csv("Data/Evo testing/Growth_10.csv", skip = 17)

Growth10 <- Growth10 %>% 
  select(x...1, y...2, y...6) %>% 
  rename(tick = x...1, mean = y...2, sd = y...6) %>% 
  mutate(metric = "Growth")

Repro10 <- read_csv("Data/Evo testing/Repro_10.csv", skip = 17)

Repro10 <- Repro10 %>% 
  select(x...1, y...2, y...6) %>% 
  rename(tick = x...1, mean = y...2, sd = y...6)  %>% 
  mutate(metric = "Reproduction")

All10 <- bind_rows(BMR10, Growth10, Repro10)


plot10 <- ggplot(All10, aes(x = tick, y = mean)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = metric), alpha = 0.25) +
  geom_line(size = 1.5, aes(col = metric))+
  theme_classic() +
  ylim(0,4) +
  scale_color_manual(values=lacroix_palette("PassionFruit",type = "discrete", n=3)) +
  scale_fill_manual(values=lacroix_palette("PassionFruit",type = "discrete", n=3)) +
  scale_x_continuous(labels = c(0,25,50,75,100)) +
  labs( x = "Years", y = "Trait value", title = "10% variance", col = NULL, fill = NULL) + 
  theme(plot.title = element_text(hjust = 0.5))


##################################################

plot1 / plot5 / plot10


##################################################

Run1 <- read_csv("Data/Evo testing/EvoTest10_1.csv", skip = 6)
Run1$outs <- paste(Run1$`evo-test-outs`[1], Run1$`[run number]`[2], sep= " ")
Run1 <- Run1[1,]

Run2 <- read_csv("Data/Evo testing/EvoTest10_2.csv", skip = 6)
Run2$outs <- paste(Run2$`evo-test-outs`[1],Run2$`[run number]`[2], sep= " ")
Run2 <- Run2[1,]

Run3 <- read_csv("Data/Evo testing/EvoTest10_3.csv", skip = 6)
Run3$outs <- paste(Run3$`evo-test-outs`[1],Run3$`[run number]`[2], sep= "")
Run3 <- Run3[1,]

Run4 <- read_csv("Data/Evo testing/EvoTest10_4.csv", skip = 6) 
Run4$outs <- paste(Run4$`evo-test-outs`[1],Run4$`[run number]`[2], sep= " ")
Run4 <- Run4[1,]

Run5 <- read_csv("Data/Evo testing/EvoTest10_5.csv", skip = 6)
Run5$outs <- paste(Run5$`evo-test-outs`[1],Run5$`[run number]`[2], sep= " ")
Run5 <- Run5[1,]

Runs <- bind_rows(Run1,Run2,Run3,Run4,Run5) 

Runs <- Runs %>% 
  select('[run number]', outs) %>% 
  rename(runNum = '[run number]')  

nam <- c(as.character(1:6000)) 

Outs <- Runs %>% 
  separate(outs, nam, sep = " ") %>% 
  gather(key = "num", value = "value", - runNum) 

# fix bracketing issues
Outs$value <- str_replace(Outs$value,"\\[","")
Outs$value <- str_replace(Outs$value,"\\]","")
Outs$value <- str_replace(Outs$value,"\\[","")
Outs$value <- str_replace(Outs$value,"\\]","")

# convert to numbers
Outs$value <- as.numeric(Outs$value)
Outs$num <- as.numeric(Outs$num)

Outs <- Outs %>% 
  group_by(runNum) %>% 
  arrange(num) %>% 
  mutate(order = rep(1:6, times = (n() / 6)), point = rep(1:(n() / 6), each = 6)) %>% 
  drop_na()

Outs <- Outs %>% 
  select(-num) %>% 
  mutate(stat = ifelse(order %in% c(1,3,5), "mean", "sd"), metric = ifelse(order %in% c(1,2), "BMR", ifelse(order %in% c(3,4), "Growth", "Repro"))) %>% 
  select(-order) %>% 
  pivot_wider(names_from = stat, values_from = value) %>% 
  group_by(runNum) 

outsPlot <- ggplot(Outs, aes(x = point, y = mean, group = as.factor(runNum))) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = as.factor(runNum)), alpha = 0.25) +
  geom_line(size = 1.5, aes(col = as.factor(runNum)))+
  theme_classic() +
  ylim(0,5)+
  scale_color_manual(values=lacroix_palette("PassionFruit",type = "discrete", n=6)) +
  scale_fill_manual(values=lacroix_palette("PassionFruit",type = "discrete", n=6)) +
  #  scale_x_continuous(labels = c(0,25,50,75,100)) +
  #  facet_wrap(nrow = 3, vars(metric),   scales = "free_y") + 
  facet_wrap(nrow = 3, vars(metric)) + 
  labs( x = "Years", y = "Trait value", col = NULL, fill = NULL) 
outsPlot


#ggsave("Figures/EvoTest20Free.png", outsPlot)



##################################################

Run1 <- read_csv("Data/Evo testing/EvoTest20_1.csv", skip = 6)
Run1$outs <- paste(Run1$`evo-test-outs`[1], Run1$`[run number]`[2], sep= "")
Run1 <- Run1[1,]

Run2 <- read_csv("Data/Evo testing/EvoTest20_2.csv", skip = 6)
Run2$outs <- paste(Run2$`evo-test-outs`[1],Run2$`[run number]`[2], sep= "")
Run2 <- Run2[1,]

Run3 <- read_csv("Data/Evo testing/EvoTest20_3.csv", skip = 6)
Run3$outs <- paste(Run3$`evo-test-outs`[1],Run3$`[run number]`[2], sep= "1")
Run3 <- Run3[1,]

Run4 <- read_csv("Data/Evo testing/EvoTest20_4.csv", skip = 6) 
Run4$outs <- paste(Run4$`evo-test-outs`[1],Run4$`[run number]`[2], sep= " 1")
Run4 <- Run4[1,]

Run5 <- read_csv("Data/Evo testing/EvoTest20_5.csv", skip = 6)
Run5$outs <- paste(Run5$`evo-test-outs`[1],Run5$`[run number]`[2], sep= "")
Run5 <- Run5[1,]

Runs <- bind_rows(Run1,Run2,Run3,Run4,Run5) 

Runs <- Runs %>% 
  select('[run number]', outs) %>% 
  rename(runNum = '[run number]')  

nam <- c(as.character(1:6000)) 

Outs <- Runs %>% 
  separate(outs, nam, sep = " ") %>% 
  gather(key = "num", value = "value", - runNum) 

# fix bracketing issues
Outs$value <- str_replace(Outs$value,"\\[","")
Outs$value <- str_replace(Outs$value,"\\]","")
Outs$value <- str_replace(Outs$value,"\\[","")
Outs$value <- str_replace(Outs$value,"\\]","")

# convert to numbers
Outs$value <- as.numeric(Outs$value)
Outs$num <- as.numeric(Outs$num)

Outs <- Outs %>% 
  group_by(runNum) %>% 
  arrange(num) %>% 
  mutate(order = rep(1:6, times = (n() / 6)), point = rep(1:(n() / 6), each = 6)) %>% 
  drop_na()

Outs <- Outs %>% 
  select(-num) %>% 
  mutate(stat = ifelse(order %in% c(1,3,5), "mean", "sd"), metric = ifelse(order %in% c(1,2), "BMR", ifelse(order %in% c(3,4), "Growth", "Repro"))) %>% 
  select(-order) %>% 
  pivot_wider(names_from = stat, values_from = value) %>% 
  group_by(runNum) 

outsPlot <- ggplot(Outs, aes(x = point, y = mean, group = as.factor(runNum))) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = as.factor(runNum)), alpha = 0.25) +
  geom_line(size = 1.5, aes(col = as.factor(runNum)))+
  theme_classic() +
  ylim(0,5)+
  scale_color_manual(values=lacroix_palette("PassionFruit",type = "discrete", n=6)) +
  scale_fill_manual(values=lacroix_palette("PassionFruit",type = "discrete", n=6)) +
  #  scale_x_continuous(labels = c(0,25,50,75,100)) +
  #  facet_wrap(nrow = 3, vars(metric),   scales = "free_y") + 
  facet_wrap(nrow = 3, vars(metric)) + 
  labs( x = "Years", y = "Trait value", col = NULL, fill = NULL) 
outsPlot


#ggsave("Figures/EvoTest20Free.png", outsPlot)


50 * (60 * 60 * 24) / 1000000

1.5 * 20.1 * (60 * 24) / 1000

