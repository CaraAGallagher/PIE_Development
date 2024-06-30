# Cara Gallagher
# March 1st, 2022
# Energy Budget with Individual Variation project
# Pop level output verification

##################################################
# Packages:
library(tidyverse)
library("LaCroixColoR")
library(patchwork)
library("RColorBrewer")                            

pal <- colorRampPalette(c("#E4AA4E","#E4714E","#BC4A53","#3F3F7B"))
pal <- rev(pal(30))
textSizeAxisTitle <- 40

font_add_google(name = "Montserrat", family = "Montserrat")
showtext_auto()

##################################################

modOut <- read_csv("Data/Verification/EBIVPrototype_PopSize1.csv", skip = 17)

modOut1 <- modOut %>% 
  select(x...1, y...2, y...6) %>% 
  rename(tick = x...1, adults = y...2, pups = y...6) %>% 
  mutate(total = adults + pups)


modOut <- read_csv("Data/Verification/EBIVPrototype_PopSize2.csv", skip = 17)

modOut2 <- modOut %>% 
  select(x...1, y...2, y...6) %>% 
  rename(tick = x...1, adults = y...2, pups = y...6) %>% 
  mutate(total = adults + pups)


modOut <- read_csv("Data/Verification/EBIVPrototype_PopSize3.csv", skip = 17)

modOut3 <- modOut %>% 
  select(x...1, y...2, y...6) %>% 
  rename(tick = x...1, adults = y...2, pups = y...6) %>% 
  mutate(total = adults + pups)


modOut <- read_csv("Data/Verification/EBIVPrototype_PopSize4.csv", skip = 17)

modOut4 <- modOut %>% 
  select(x...1, y...2, y...6) %>% 
  rename(tick = x...1, adults = y...2, pups = y...6) %>% 
  mutate(total = adults + pups)


modOut <- read_csv("Data/Verification/EBIVPrototype_PopSize5.csv", skip = 17)

modOut5 <- modOut %>% 
  select(x...1, y...2, y...6) %>% 
  rename(tick = x...1, adults = y...2, pups = y...6) %>% 
  mutate(total = adults + pups)


modOut <- bind_rows(modOut1,modOut2,modOut3,modOut4,modOut5)


modOut <- modOut %>% 
  mutate(year = tick / (48 * 365)) %>% 
  group_by(year) %>% 
  summarise(meanTot = mean(total), sdTot = sd(total))

ggplot(modOut, aes(x = year, y = meanTot)) +
  geom_ribbon(aes(ymin = meanTot - sdTot, ymax = meanTot + sdTot), alpha = 0.25, fill = "grey50") +
  geom_line(col = "grey50", size = 1.5)+
  theme_classic() +
  labs( x = "Year", y = "Total vole population [N]")


### post scaling update check
modOut <- read_csv("Data/Verification/Dens-5259.csv", skip = 6)

modOut <- modOut %>% 
  select('pop-list','off-list','max-resources-base','r-growth-ts') %>% 
  rename(popList ='pop-list', offList = 'off-list', maxR = 'max-resources-base', rGrow = 'r-growth-ts') 

# Olympic, BryceCanyon, DeathValley !, Denali !,   Saguaro !, Volcanoes !, Yellowstone
pal <- natparks.pals(name="Volcanoes",n=20,type="continuous")
nam <- c(as.character(1:10000))

popOut <- modOut %>% 
  select(-offList) %>% 
  separate(popList, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -maxR, -rGrow) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         num = as.numeric(num), value = as.numeric(value)) %>% 
  rename(pop = value) %>% 
  drop_na() 

offOut <- modOut %>% 
  select(-popList) %>% 
  separate(offList, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -maxR, -rGrow) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         num = as.numeric(num), value = as.numeric(value)) %>% 
  rename(off = value) %>% 
  drop_na() 

modOut <- left_join(popOut, offOut, by = c("maxR", "rGrow", "num")) 

modOut1 <- modOut %>% 
  group_by(maxR, rGrow, num) %>% 
  summarise(popMean = mean(pop), offMean = mean(off)) %>% 
  filter(rGrow == 0.005)

ggplot(modOut1, aes(x = num, y = popMean, col = interaction(maxR, rGrow))) +
  geom_line(size = 1.5)+
  theme_classic() +
  labs( x = "Year", y = "Total vole population [N]")

# Local densities from patchset
modOut <- read_csv("Data/Verification/Dens-5341.csv", skip = 6)
modOut <- read_csv("Data/Verification/Dens-DIPupd.csv", skip = 6)

modOut <- modOut %>% 
  select('dens-list','max-resources-base','r-growth-ts', '[step]') %>% 
  rename(densList ='dens-list', maxR = 'max-resources-base', rGrow = 'r-growth-ts', step = '[step]') %>% 
  filter(step >= 77289) %>% 
  select(-step)

# min(modOut$maxR)
# max(modOut$maxR)
# min(modOut$rGrow)
# max(modOut$rGrow)

# Olympic, BryceCanyon, DeathValley !, Denali !,   Saguaro !, Volcanoes !, Yellowstone
#pal <- natparks.pals(name="Volcanoes",n=20,type="continuous")
nam <- c(as.character(1:10000))

modOut <- modOut %>% 
  separate(densList, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -maxR, -rGrow) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         num = as.numeric(num), value = as.numeric(value)) %>% 
  rename(dens = value) %>% 
  drop_na()

densOut <- modOut %>% 
  group_by(maxR, rGrow, num) %>% 
 # filter(num > 112) %>% 
  filter(num > 7 *  112) %>% 
  filter(rGrow == 0.011) %>%
  filter(maxR == 140) %>%
  summarise(densMean = mean(dens), densSD = sd(dens)) 

# ggplot(densOut, aes(x = maxR, y = densMean)) +
#   geom_point() +
#   geom_smooth(method = "loess")
# ggplot(densOut, aes(x = rGrow, y = densMean)) +
#   geom_point() +
#   geom_smooth(method = "loess")

densPlot <- ggplot(densOut, aes(x = num, y = densMean)) +
  annotate("rect", ymin = 4.4, ymax = 41.4, xmin = -Inf, xmax = Inf, alpha = 0.1, fill = "black") + 
  annotate("rect", ymin = 17.49 - 11.78, ymax = 17.49 + 11.78, xmin = -Inf, xmax = Inf, alpha = 0.1, fill = "black") + 
  geom_hline(yintercept = 14.2 , size = 2, col = "grey40", linetype = "dashed") +
  geom_ribbon(aes(ymin = densMean - densSD, ymax = densMean + densSD), fill = pal[4], alpha = 0.35) +
  geom_line(aes(col = interaction(maxR, rGrow)), col = pal[4], size = 1.5)+
  theme_classic() +
  labs( x = "Weeks (skipping winters)", y = "Vole local densities [N ha-1]") +
  theme(text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"),
        legend.position = "none")
densPlot
#ggsave("Figures/CalOuts/DensPlot.png", densPlot, width = 10)


densOutMeans <- modOut %>% 
  filter(num > 112) %>% 
  group_by(maxR, rGrow) %>% 
  mutate(diff = dens - 14.2 , in50 = ifelse(dens >= 8.9 & dens <= 21.6, 1, 0), in95 = ifelse(dens >= 4.52 & dens <= 40.23, 1, 0)) %>% 
  summarise(densMean = mean(dens), densMedian = median(dens), densSD = sd(dens), diffMean = abs(mean(diff)), in50Mean = mean(in50), in95Mean = mean(in95)) 
  
densOutMeans %>% 
  filter(abs(diffMean) == min(abs(densOutMeans$diffMean)))

densOutMeans %>% 
  arrange(abs(diffMean))

densOutMeans %>% 
  filter(abs(densMedian - 14.2) == min(abs(densOutMeans$densMedian - 14.2)))
densOutMeans %>% 
  filter(in50Mean == max(densOutMeans$in50Mean))
densOutMeans %>% 
  filter(in95Mean == max(densOutMeans$in95Mean))

densOutMeans %>% 
  #arrange(abs(diffMean), desc(in50Mean), desc(in95Mean))
  arrange(abs(diffMean))
hist(densOutMeans$densMean)
hist(densOutMeans$diffMean)

#### Survival rates  #### 
## Remember that often animals leaving the study area or not recaught are considered dead
# 22% survival from one breeding season to the next in Koskela 1998
# 63.6% for control females from beginning of July to October 12-14th in Mappes, Koskela, & Ylonen 1995 
# 24% survived from the entire breeding season (from May to September or October) in Boratynski and Koteja 2009  
# 79% monthly survival probability between spring and fall for reproductively inactive & PUUV antibody negative voles in Tersago et al. 2012 
# 55% monthly survival probability between spring and fall for reproductively active & PUUV antibody negative voles in Tersago et al. 2012 
# 79% monthly survival probability between March - October for females in both grids in Newson 1963 - mean(c(0.81,0.77,0.72,0.89,0.83,0.83,0.68))
# 98.4% 15 day survival for female voles in Haapakoski and Ylönen 2010
# 96% 7 day survival for weanlings in mid-summer in Eccard et al. 2002 - Evolutionary ecology



#### Densities for coauthor presentation

densOut <- read_csv("Data/Verification/DensForPres.csv", skip = 21)

densOut <- densOut %>% 
  select(x...21, y...22) %>% 
  rename(tick = x...21, density = y...22) %>% 
  filter(tick >= 180) %>% 
  mutate(yr = ifelse(tick <= 350, 1, ifelse(tick <= 500, 2, 3))) %>% 
  group_by(yr) %>% 
  mutate(tick = row_number())

ggplot(densOut, aes(x = tick, y = density, col = as.factor(yr))) +
  geom_line(size = 1.6) + 
  facet_grid(cols = vars(yr), scales = "free_x", space= "free") +
  labs(x = "Timestep", y = "Density [N females ha-1]") +
  theme_classic() +
  theme(text = element_text(size = 16, color = "grey10"),
        legend.position = "none") +    
  scale_colour_brewer(palette = "Dark2")












