# Cara Gallagher
# December 29th, 2022
# Energy Budget with Individual Variation project
# Home range size output verification

##################################################
# Packages:
library(tidyverse)
library(NatParksPalettes)
library(patchwork)

##################################################
# Olympic, BryceCanyon, DeathValley !, Denali !,   Saguaro !, Volcanoes !, Yellowstone
pal <- natparks.pals(name="Volcanoes",n=20,type="continuous")
nam <- c(as.character(1:10000))


HROut <- read_csv("Data/Verification/EBIV_HRcheck.csv", skip = 6)

HROut <- HROut %>% 
  select(x...1, y...1, y...6) %>% 
  rename(tick = x...1, HRr = y...1, turnRad = y...6)  %>% 
  separate(HRr, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -turnRad) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         num = as.numeric(num), value = as.numeric(value)) %>% 
  drop_na() %>% 
  mutate(HRr = HRr * 10)  


ggplot(HROut, aes(x = as.factor(turnRad), y = HRr)) +
  geom_rect(aes(ymin=11.3, ymax=32.2, xmin=0, xmax=Inf), fill = "black", alpha = 0.25) +
  geom_violin() +
  scale_fill_manual(values = pal) +
  theme_classic() +
  theme(text = element_text(size = 16, color = "grey10"),
        legend.position = "none")
