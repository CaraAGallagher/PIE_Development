# Cara Gallagher
# August 16th, 2023
# Energy Budget with Individual Variation project
# NDVI outputs 

##################################################
# Packages:
library(tidyverse)
library(MODISTools)
library(raster)
library(beepr)
library(showtext)
library(lubridate)


# load typeface and color palette
font_add_google(name = "Montserrat", family = "Montserrat")
showtext_auto()
pal <- colorRampPalette(c("#3F3F7B","#278192","#00B089","#AABC4A", "#E5DC54","#E4AA4E","#E4714E","#BC4A53"))



#### Study site coordinates #### 
study_sites <- data.frame(
  site_name = c("Stroemsund", 
           "Evenstad", 
           "Bielowieza", 
           "Pallasjarvi", 
           "Asturias", 
           "Le Quartier", 
           "Havelaue", 
           "Calabria", 
           "Frýdek-Místek", 
           "Konnovesi"),
  lat = c(64.083333,
               61.500000,
               52.666667,
               68.000000,
               43.000000,
               46.140000,
               52.840000,
               39.350000,
               49.700000,
               62.616667),
  lon = c(15.833333,
                11.166667,
                23.666667,
                24.166667,
                -6.400000,
                2.730000,
                12.100000,
                16.490000,
                18.380000,
                26.283333))



#### Download and process MODIS data: #### 

### NDVI ###
subsetsNDVI <- mt_batch_subset(df = study_sites,
                           product = "MOD13Q1",
                           band = "250m_16_days_NDVI",
                           internal = TRUE,
                           start = "2000-01-01",
                           end = "2022-12-30",
                           km_lr = 1,
                           km_ab = 1)

#print(str(subsetsNDVI))

beep(sound = 1, expr = NULL)

subsetsNDVI <- subsetsNDVI %>% 
  mutate(year = year(as.POSIXlt(calendar_date, format="%Y-%m-%d")),
         month = month(as.POSIXlt(calendar_date, format="%Y-%m-%d")),
         day = day(as.POSIXlt(calendar_date, format="%Y-%m-%d")),
         doy = yday(as.POSIXlt(calendar_date, format="%Y-%m-%d"))) %>% 
  filter(value >= -1000)

#write_csv(subsetsNDVI, "Data/Scenarios/NDVIdata.csv")


### EVI ###
subsetsEVI <- mt_batch_subset(df = study_sites,
                              product = "MOD13Q1",
                              band = "250m_16_days_EVI",
                              internal = TRUE,
                              start = "2000-01-01",
                              end = "2022-12-30",
                              km_lr = 1,
                              km_ab = 1)

#print(str(subsetsEVI))
# beep(sound = 1, expr = NULL)

subsetsEVI <- subsetsEVI %>% 
  mutate(year = year(as.POSIXlt(calendar_date, format="%Y-%m-%d")),
         month = month(as.POSIXlt(calendar_date, format="%Y-%m-%d")),
         day = day(as.POSIXlt(calendar_date, format="%Y-%m-%d")),
         doy = yday(as.POSIXlt(calendar_date, format="%Y-%m-%d"))) %>% 
  filter(value >= -1000)

#write_csv(subsetsEVI, "Data/Scenarios/EVIdata.csv")



#### Visualize outputs #### 
# if importing instead of pulling using MODIStools
subsetsNDVI <- read_csv("Data/Scenarios/VI GCM scenarios/Inputs/NDVIdata.csv")
subsetsEVI <- read_csv("Data/Scenarios/VI GCM scenarios/Inputs/EVIdata.csv")

### NDVI ###
subsetsNDVI <- subsetsNDVI %>% 
  mutate(site = fct_rev(fct_reorder(site, latitude)))

NDVITSplot <- ggplot(subsetsNDVI, aes(as.Date(calendar_date), value  * 0.0001, col = site, group = pixel)) + 
  geom_line(alpha = 0.5) +
  facet_grid(rows = vars(site)) +
  scale_color_manual(values = pal(10)) + 
  labs(x = "Year", y = "NDVI") +
  theme_bw() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"))
NDVITSplot

#ggsave("Figures/ScenInPlots/NDVIbyYear.png", height = 9, width = 7)

NDVIBSplot <- ggplot(subsetsNDVI, aes(site, value  * 0.0001, col = site, fill = site), alpha = 0.5) + 
  geom_violin(alpha = 0.5) +
  geom_boxplot(alpha = 0.5, width = 0.25) +
  scale_color_manual(values = pal(10)) + 
  scale_fill_manual(values = pal(10)) + 
  theme_bw() +
  labs(x = "Site", y = "NDVI") +
  theme(legend.position = "none")
NDVIBSplot

#ggsave("/Figures/ScenInPlots/NDVIbySite.png", NDVIBSplot)

NDVIMoplot <- ggplot(subsetsNDVI, aes(month, value  * 0.0001, col = site)) + 
  geom_smooth(method = "gam", se = FALSE) +
  annotate("rect", xmin = 1, xmax = 3.83, ymin = -0.1, ymax = 1, alpha = 0.2) + 
  annotate("rect", xmin = 9.14, xmax = 12, ymin = -0.1, ymax = 1, alpha = 0.2) + 
  scale_color_manual(values = pal(10)) + 
  labs(x = "Month", y = "NDVI", col = "Site") +
  theme_classic() +
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"))
NDVIMoplot

#ggsave("Figures/ScenInPlots/NDVImonth.png")

meanNDVI <- subsetsNDVI %>% 
  group_by(site, doy) %>% 
  summarise(mean = mean(value) * 0.0001, q25 = quantile(value, probs = 0.25) * 0.0001, q75 = quantile(value, probs = 0.75) * 0.0001)

NDVImDOYplot <- ggplot(meanNDVI, aes(doy)) + 
  annotate("rect", xmin = 1, xmax = 90, ymin = -0.1, ymax = 1, alpha = 0.2) + 
  annotate("rect", xmin = 276, xmax = 365, ymin = -0.1, ymax = 1, alpha = 0.2) + 
  geom_ribbon(aes(ymin = q25, ymax = q75, fill = site), alpha = 0.2) +
  geom_line(aes(y = mean, col = site), linewidth = 1.2) + 
  labs(x = "Day of year", y = "Observed NDVI", col = "Site", fill = "Site") +
  scale_color_manual(values = pal(10)) + 
  scale_fill_manual(values = pal(10)) + 
  theme_bw() +
  theme(text = element_text(size = 50, color = "grey30", family = "Montserrat"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
NDVImDOYplot

#ggsave("Figures/ScenInPlots/NDVIdoy.png", width = 7, height = 5)

### EVI ###
subsetsEVI <- subsetsEVI %>% 
  mutate(site = fct_rev(fct_reorder(site, latitude)))

EVITSplot <- ggplot(subsetsEVI, aes(as.Date(calendar_date), value  * 0.0001, col = site, group = pixel)) + 
  geom_line(alpha = 0.5) +
  facet_grid(rows = vars(site)) +
  labs(x = "Year", y = "EVI") +
  theme_bw() +
  scale_color_manual(values = pal(10)) + 
  theme(legend.position = "none") +
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"))
EVITSplot

#ggsave("Figures/ScenInPlots/EVIbyYear.png", height = 9, width = 7)

EVIBSplot <- ggplot(subsetsEVI, aes(site, value  * 0.0001, col = site, fill = site), alpha = 0.5) + 
  geom_violin(alpha = 0.5) +
  geom_boxplot(alpha = 0.5, width = 0.25) +
  labs(x = "Site", y = "EVI") +
  scale_color_manual(values = pal(10)) + 
  scale_fill_manual(values = pal(10)) + 
  theme_bw() +
  theme(legend.position = "none")
EVIBSplot

EVIMoplot <- ggplot(subsetsEVI, aes(month, value  * 0.0001, col = site)) + 
  geom_smooth(method = "gam", se = FALSE) +
  annotate("rect", xmin = 1, xmax = 3.83, ymin = -0.1, ymax = 1, alpha = 0.2) + 
  annotate("rect", xmin = 9.14, xmax = 12, ymin = -0.1, ymax = 1, alpha = 0.2) + 
  labs(x = "Month", y = "EVI", col = "Site") +
  scale_color_manual(values = pal(10)) + 
  theme_classic() +
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"))
EVIMoplot

#ggsave("Figures/ScenInPlots/EVImonth.png")

meanEVI <- subsetsEVI %>% 
  group_by(site, doy) %>% 
  summarise(mean = mean(value) * 0.0001, q25 = quantile(value, probs = 0.25) * 0.0001, q75 = quantile(value, probs = 0.75) * 0.0001)

EVIDOYplot <- ggplot(meanEVI, aes(doy)) + 
  geom_ribbon(aes(ymin = q25, ymax = q75, fill = site), alpha = 0.15) +
  geom_line(aes(y = mean, col = site), linewidth = 1.2) + 
  annotate("rect", xmin = 1, xmax = 116, ymin = -0.1, ymax = 1, alpha = 0.2) + 
  annotate("rect", xmin = 278, xmax = 365, ymin = -0.1, ymax = 1, alpha = 0.2) + 
  labs(x = "Day of Year", y = "EVI", col = "Site", fill = "Site") +
  scale_color_manual(values = pal(10)) + 
  scale_fill_manual(values = pal(10)) + 
  theme_classic() +
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"))
EVIDOYplot

#ggsave("Figures/ScenInPlots/EVIdoy.png")

