# Cara Gallagher
# December 6th, 2023
# Energy Budget with Individual Variation project
# GCM abiotic data for projections 

##################################################
# Packages:
library(tidyverse)
library(beepr)
library(showtext)
library(ncdf4)
library(raster)
library(terra)
library(SPEI)

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


#### Accessing GCM data ####
## Open CMIP6 data downloaded from Copernicus data store 
# File specs: 
# Models: CNRM-CM6-1-HR, EC-Earth3-CC, & AWI-CM-1-1-MR 
# Experiments: Historical, ssp245, & ssp585 
# Resolution: 0.50, 0.70, & 0.94 deg 
# Timestep: Monthly
# Variables: pr, rsds, tasmin, tasmax

ncpath <- "./Data/Scenarios/VI GCM scenarios/Inputs/GCM data"

filenames <- list.files(ncpath, pattern="*.nc", full.names=TRUE)

# import data and rasterize
# ncdata <- lapply(filenames, rast)

# plot(ncdata[[6]])

study_sites <- vect(study_sites, geom=c("lon", "lat"), crs = "+proj=longlat +datum=WGS84 +no_defs")

#### Getting netcdf info #### 

# pull relevant info for each layer using filename

filenames_short <- list.files(ncpath, pattern="*.nc", full.names=FALSE)

#### Extract data by site #### 

# create dataframe for all outputs

extracted_data_all_sites <- tibble(value = numeric(), 
                                   date = as.Date(character()), 
                                   variable = character(), 
                                   temporal_res = character(), 
                                   model = character(), 
                                   experiment = character(), 
                                   sim_id = character(), 
                                   grid_type = character(), 
                                   date_range = character(), 
                                   vers_date = character(), 
                                   site = character(), 
                                   year = numeric(), 
                                   month = numeric(), 
                                   day = numeric())


for (i in 1:length(filenames_short)) {
#  i <- 7
  
  file <- tibble(details = filenames_short[i])
  
  file <- separate_wider_delim(file, details, delim = "_", names = c("variable", "temporal_res", "model", "experiment", "sim_id", "grid_type", "date_range", "vers_date")) 

  ncdata <- rast(filenames[[i]])

  extracted_data <- terra::extract(ncdata, y = study_sites)

  extracted_data <- tibble(extracted_data)

  extracted_data <- extracted_data %>% 
    mutate(ID = study_sites$site_name) %>% 
    rename(site = ID) %>% 
    pivot_longer(!site, names_to = "date", values_to = "value") %>% 
    mutate(date = rep(time(ncdata, format=""), times = 10)) %>% 
    mutate(variable = file$variable,
           temporal_res = file$temporal_res, 
           model = file$model,
           experiment = file$experiment,
           sim_id = file$sim_id,
           grid_type = file$grid_type,
           date_range = file$date_range,
           vers_date = file$vers_date
           ) %>% 
    mutate(year = as.numeric(format(date, format = "%Y")),
           month = as.numeric(format(date, format = "%m")),
           day = as.numeric(format(date, format = "%d")))
  
  # extracted_data %>%
  #   group_by(month, year, site) %>%
  #   summarise(pr_sum = sum(value)) %>% 
  # ggplot(aes(x = month, y = pr_sum, col = site)) +
  #   geom_jitter(size = 1.1, width = 0.3, alpha = 0.5) +
  #   facet_grid(rows = vars(site), scales = "free")
  # 
  # extracted_data %>%
  #   group_by(month, year, site) %>%
  #   summarise(pr_sum = sum(value)) %>% 
  #   ungroup() %>% 
  #   group_by(site) %>% 
  #   summarise(pr_sum_mean = mean(pr_sum) * 10000) 
     
  
  extracted_data_all_sites <- bind_rows(extracted_data_all_sites, extracted_data)
  
  print(i)
}

extracted_data_all_sites <- extracted_data_all_sites %>% 
  dplyr::select(date, model, experiment, site, variable, value) %>% 
  pivot_wider(names_from = "variable", values_from = "value")

#write_csv(extracted_data_all_sites, "Data/Scenarios/VI GCM scenarios/Inputs/GCMdata.csv")

# extracted_data_all_sites <- read_csv("Data/Scenarios/VI GCM scenarios/Inputs/GCMdata.csv")



#### Visualize outputs #### 
ggplot(extracted_data_all_sites, aes(x = date, y = tasmin, col = experiment)) +
  geom_line() +
  facet_grid(rows = vars(site), cols = vars(model), scales = "fixed")

ggplot(extracted_data_all_sites, aes(x = date, y = pr, col = experiment)) +
  geom_line() +
  facet_grid(rows = vars(site), cols = vars(model), scales = "fixed")

ggplot(extracted_data_all_sites, aes(x = date, y = rsds, col = experiment)) +
  geom_line() +
  facet_grid(rows = vars(site), cols = vars(model), scales = "fixed")

ggplot(extracted_data_all_sites, aes(x = date, y = tasmin, col = experiment)) +
  geom_smooth() +
  facet_grid(rows = vars(model, experiment), scales = "fixed") 

ggplot(extracted_data_all_sites, aes(x = date, y = pr, col = experiment)) +
  geom_line() +
  facet_grid(rows = vars(experiment), scales = "fixed") 


ggplot(extracted_data_all_sites, aes(x = date, y = pr, col = experiment)) +
  geom_smooth() +
  facet_grid(rows = vars(site), scales = "free") 


ggplot(subset(extracted_data_all_sites, site == as.character(study_sites$site_name[4])), aes(x = date, y = values, col = experiment)) +
  geom_line() +
  geom_smooth(se = FALSE, linewidth = 1.2) +
  facet_grid(rows = vars(variable), cols = vars(model), scales = "free") 

extracted_data_all_sites %>% 
  group_by(site, experiment) %>% 
  filter(variable == "pr") %>% 
  summarise(mean = mean(value), sd = sd(value)) %>% 
  ggplot(aes(x= site, col = experiment)) + 
  geom_pointrange(aes(y = mean, ymin = mean - sd, ymax = mean + sd), position = position_dodge(width = 0.5))


#### SPEI & SPI calculation ####

#ext_data_pr <- extracted_data_all_sites %>% filter(variable == "pr")
ext_data_pr <- extracted_data_all_sites %>% dplyr::select(1:5)

ext_data_pr <- ext_data_pr %>% 
#  dplyr::select(model, experiment, date, site, pr) %>% 
  mutate(month = month(date), year = year(date)) %>% 
  group_by(model, experiment, site, pr, month, year) %>% 
  mutate(pr = pr * 86400 * days_in_month(date)) %>%  #convert from km m-2 s-1 to mm month-1
 # summarise(pr = sum(pr, na.rm=TRUE)) %>% 
  mutate(day = 1, 
         date = as.Date(paste(year, month, day, sep = "-"), "%Y-%m-%d")) %>% 
  ungroup() %>% 
  dplyr::select(-year, -month, -day)
  
# ggplot(ext_data_pr, aes(x = month, y = pr, col = site)) +
#   geom_jitter(size = 1.1, width = 0.3, alpha = 0.5) +
#   facet_grid(rows = vars(site), scales = "fixed")
# 
# ggplot(ext_data_pr, aes(x = site, y = value, col = site)) +
#   geom_violin(linewidth = 1.1) +
#   geom_boxplot(size = 1.1, width = 0.3, alpha = 0.5)

# non-precip variables 
# ext_data_no_pr <- extracted_data_all_sites %>% filter(variable != "pr")
# ext_data_no_pr$date <- floor_date(ext_data_no_pr$date, unit = "month")
# 
# ext_data_no_pr <- ext_data_no_pr %>% 
#   dplyr::select(model, experiment, date, site, variable, value)
# 
# ext_data_all <- bind_rows(ext_data_pr, ext_data_no_pr)
# 
# ext_data_all <- ext_data_all %>%
#   ungroup() %>%
#   pivot_wider(names_from = variable, values_from = value) 

ext_data_no_pr <- extracted_data_all_sites %>% 
  dplyr::select(-5) %>% 
  mutate(date = floor_date(date, unit = "month"))

ext_data_all <- left_join(ext_data_pr, ext_data_no_pr, by = c("model", "experiment", "site", "date"))

# Grabbing historical data and integrating it into the time series for spei calculation
ext_data_hist <- ext_data_all %>% 
  filter(experiment == "historical")

CRNM_245 <- ext_data_hist %>% 
  filter(model == "CNRM-CM6-1-HR") %>% 
  mutate(experiment = "ssp245") %>% 
  arrange(site, date)

CRNM_585 <- ext_data_hist %>% 
  filter(model == "CNRM-CM6-1-HR") %>% 
  mutate(experiment = "ssp585") %>% 
  arrange(site, date)

Earth3_245 <- ext_data_hist %>% 
  filter(model == "EC-Earth3-CC") %>% 
  mutate(experiment = "ssp245") %>% 
  arrange(site, date)

Earth3_585 <- ext_data_hist %>% 
  filter(model == "EC-Earth3-CC") %>% 
  mutate(experiment = "ssp585") %>% 
  arrange(site, date)

# since missing from CDS, AWI taken as average of other two models
pr_means <- rowMeans(cbind(CRNM_245$pr,Earth3_245$pr))

AWI_245 <- ext_data_hist %>% 
  filter(model == "AWI-CM-1-1-MR") %>% 
  mutate(experiment = "ssp245") %>% 
  arrange(site, date) %>% 
  mutate(pr = pr_means)

AWI_585 <- ext_data_hist %>% 
  filter(model == "AWI-CM-1-1-MR") %>% 
  mutate(experiment = "ssp585") %>% 
  arrange(site, date) %>% 
  mutate(pr = pr_means)

ext_data_fut <- ext_data_all %>% 
  filter(experiment != "historical")

# combine all back together to have full timeseries from historical through proj for each model and experiment
speiDatAll <- ext_data_fut %>% 
  bind_rows(CRNM_245, CRNM_585, Earth3_245, Earth3_585, AWI_245, AWI_585)  %>% 
  arrange(model, site, date) %>% 
  mutate(tasmin = tasmin - 273.15,
         tasmax = tasmax - 273.15) #%>% 
  #filter(!(model == "CNRM-CM6-1-HR" & experiment == "ssp245"))

ggplot(speiDatAll, aes(x = date, y = pr, col = experiment)) +
  geom_line() +
  facet_grid(rows = vars(site, experiment), cols = vars(model), scales = "fixed")


### spei & spi calculation
SPEIouts <- tibble(date = as.Date(character()), 
                   model = character(), 
                   experiment = character(), 
                   site = character(), 
                   pr = numeric(),
                   tasmax = numeric(), 
                   tasmin = numeric(), 
                   PET = numeric(),
                   spei = numeric(),
                   spi = numeric())
  
experiments <- unique(speiDatAll$experiment)

for (i in 1:length(study_sites$site_name)) {
  
  for (ii in 1:2) {
    
   # i <- 7
  #  ii <- 2
      
    sub <- subset(speiDatAll, site == study_sites$site_name[i] & experiment == experiments[ii])
    
    for (iii in 1:length(unique(sub$model))) {
      
   #   iii <- 2
      
      subsub <- subset(sub, model == unique(sub$model)[iii])
      
      subsub <- subsub %>% 
        arrange(date) %>% 
        mutate(PET = hargreaves(Tmin = tasmin, Tmax = tasmax, lat = geom(study_sites)[i,4])) 
      
      speidat <- subsub %>% 
        mutate(year = year(date), 
               month = month(date), 
               H2O_bal = pr - PET) %>% 
        dplyr::select(year, month, H2O_bal)
      
       spei <- spei(data = ts(speidat$H2O_bal, frequency = 12, start = c(year(min(subsub$date)), month(min(subsub$date))), end = c(year(max(subsub$date)), month(max(subsub$date)))), scale = 1, ref.start = c(2000, 1), ref.end = c(2023, 1), keep.x = TRUE)
       spi <- spi(data = ts(subsub$pr, frequency = 12, start = c(year(min(subsub$date)), month(min(subsub$date))), end = c(year(max(subsub$date)), month(max(subsub$date)))), scale = 1, ref.start = c(2000, 1), ref.end = c(2023, 1), keep.x = TRUE)
       
       plot(spei)
       plot(spi)
       
       spei_result <- as_tibble(spei$fitted)
       spi_result <- as_tibble(spi$fitted)

       full_result <- tibble(spei = spei$fitted, spi = spi$fitted)
       
       full_result <- full_result %>% 
         mutate(date = seq(as.Date(min(subsub$date)), as.Date(max(subsub$date)), "months"))
       
       back_together <- left_join(subsub, full_result, by = "date")
       
       SPEIouts <- bind_rows(SPEIouts, back_together)
       
    }
  }
}


# write_csv(SPEIouts, "Data/Scenarios/VI GCM scenarios/Inputs/GCMdataSPEI.csv")

# SPEIouts <- read.csv("Data/Scenarios/VI GCM scenarios/Inputs/GCMdataSPEI.csv")

ggplot(SPEIouts, aes(x = date, y = spi)) +
  geom_line() +
  facet_grid(rows = vars(experiment, site), cols = vars(model), scales = "fixed")



