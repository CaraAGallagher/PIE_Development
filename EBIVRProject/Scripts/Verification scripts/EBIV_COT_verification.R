# Cara Gallagher
# February 21st, 2022
# Energy Budget with Individual Variation project
# Cost of transport output verification

##################################################
# Packages:
library(tidyverse)
library(patchwork)
library(showtext)


font_add_google(name = "Montserrat", family = "Montserrat")
showtext_auto()

##################################################

#### COT costs #### 

nam <- c(as.character(1:100000))
pal <- colorRampPalette(c("#BC4A53","#3F3F7B","#278192","#00B089"))


modOut <- read_csv("Data/Verification/EBIV_COT_activity_fuels.csv", skip = 6)

cotOut <- modOut %>%  
  select('[run number]', 'COT-out-list') %>% 
  rename(runNum = '[run number]', COT = 'COT-out-list')

cotOut <- cotOut %>% 
  separate(COT, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -runNum) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  arrange(runNum, num) %>% 
  drop_na() %>% 
  group_by(runNum) %>% 
  mutate(order = rep(1:3, times = (max(row_number()) / 3)), num = rep(1:(max(row_number()) / 3), each = 3)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(speed = `1`, COT = `2`, BMR = `3`) %>% 
  mutate(speed = speed / 60, COT = COT / (30 * 60), BMR = BMR / (30 * 60), COTtot = COT + BMR) %>% 
  filter(speed != 0)

cotOut <- cotOut %>% 
  mutate(speed_round = round(speed)) %>% 
  group_by(speed_round) %>% 
  summarise(mean = mean(COTtot), ci975 = quantile(COTtot, probs = 0.975), ci25 = quantile(COTtot, probs = 0.025))


Eval <- tibble(
  speed = seq(0,ceiling(max(cotOut$speed_round)),1), 
  Dlugosz_30.3g = (2.272+0.0336*speed)*20.1/60,
  Dlugosz_31.0g = (2.258+0.0365*speed)*20.1/60,
  RezendeM_40.8g = (2.873+0.0641*speed)*20.1/60,
  RezendeF_27.1g = (2.497+0.0360*speed)*20.1/60,
  Chappell_21.5g = (1.916+0.501*(speed / 16.6667))*20.1/60,
  Taylor_20g = (((10.7*(0.020^-0.316))*(speed/60)) + (6.03*(0.020^-0.303)))* 0.020,
)

Eval <- Eval %>% 
  gather(key = "Source", value = "value", -speed)

COTVerPlot <- ggplot() + 
  #geom_point(data = cotOut, aes(x = speed, y = COTtot), col = pal[1], alpha = 0.01) + 
  geom_line(data = Eval, aes(x=speed, y = value, col = Source), size = 1.5)+
  geom_linerange(data = cotOut, aes(x = speed_round, ymin = ci25, ymax = ci975), col = "#E4714E", size = 1) +
  geom_point(data = cotOut, aes(x = speed_round, y = mean), col = "#E4714E", size = 3) +
  lims(y = c(0,2)) +
  scale_colour_grey(labels = c("Chappell et al. 2004 (21.5g)", "Dlugosz et al. 2009 (30.3g)", "Dlugosz et al. 2009 (31.0g)", "Rezende et al. 2009 (Females; 27.1g)", "Rezende et al. 2009 (Males; 40.8g)", "Taylor et al. 1970 (20g)"), name = element_blank()) +
  labs(x = "Running speed [m min-1]", y = "Metabolic cost [W]") +
  theme_classic() +
  guides(color=guide_legend(nrow=3, byrow=TRUE)) +
  theme(text = element_text(size = 40, color = "grey30", family = "Montserrat"),
        axis.line = element_line(color = "grey30"),
        legend.position =  c(0.5,0.9),#"top",
        legend.text = element_text(size = 35, family = "Montserrat"),
        legend.spacing.y = unit(0.1, 'cm')) 
COTVerPlot

#ggsave("Figures/Verification/COTPlotOut.png", COTVerPlot, width = 8, height = 7)


#### Activity percent costs #### 

actOut <- modOut %>%  
  select('[run number]', 'activity-list') %>% 
  rename(runNum = '[run number]', act = 'activity-list')

actOut <- actOut %>% 
  select(-runNum) %>% 
  separate(act, nam, sep = " ") %>% 
  gather(key = "num", value = "value") %>% 
  mutate(value = str_replace(value,"\\[",""),
         value = str_replace(value,"\\]",""),
         value = as.numeric(value)) %>% 
  select(-num) %>% 
  drop_na() %>% 
  summarise(meanAct = mean(value), sdAct = sd(value))

actOut <- tibble(state = c("Moving", "Resting"), ymax = c(actOut$meanAct, 1), ymin = c(0, actOut$meanAct))

actOut$labelPosition <- (actOut$ymax + actOut$ymin) / 2
actOut$label <- c("Moving\n(29.5%)","Resting\n(70.5%)")

actVerPlot <- ggplot(actOut, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=state)) +
  geom_rect(color = "white", size = 3) +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=6, color = "white",  label.size = NA, family = "Montserrat") +
  scale_fill_manual(values = c("#DA3B0B","#E4714E"), guide = "none") +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none",
        text = element_text(size = 40, color = "grey30", family = "Montserrat"))
actVerPlot

#ggsave("Figures/Verification/ActPlotOut.png", actVerPlot, width = 6, height = 6)


