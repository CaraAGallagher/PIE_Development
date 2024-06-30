# Cara Gallagher
# May 15th, 2024
# Energy Budget with Individual Variation project
# Observed abiotic data for projection 

##################################################
# Packages:
library(tidyverse)
library(beepr)
library(showtext)
library(ncdf4)
library(raster)
library(terra)

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


#### Accessing E-OBS data ####
# Data from the Copernicus Climate Data Store 
# E-OBS daily gridded meteorological data for Europe from 1950 to present derived from in-situ observations


ncpath <- "./Data/Scenarios/VI GCM scenarios/Inputs/EOBS data"

filenames <- list.files(ncpath, pattern="*.nc", full.names=TRUE)

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

## precipitation 

ncdata <- rast(filenames[[1]])

ncdata <- ncdata[[time(ncdata) > as.Date("2000-01-01")]]

extracted_data <- terra::extract(ncdata, y = study_sites)

extracted_data <- tibble(extracted_data)

cols <- seq.Date(as.Date("2000-01-02"), as.Date("2023-12-31"), by = "day")
cols <- c("site", as.character(cols))
 
colnames(extracted_data) <- cols 

extracted_data_pr <- extracted_data %>% 
  mutate(site = c("Stroemsund", "Evenstad", "Bielowieza", "Pallasjarvi", "Asturias", "Le Quartier", "Havelaue", "Calabria", "Frýdek-Místek", "Konnovesi"), .before = 1) %>% 
  pivot_longer(cols = -1, names_to = "date", values_to = "pr") %>% 
  mutate(date = as.Date(date), month = month(date), year = year(date)) %>% 
  group_by(site, month, year) %>% 
  summarise(pr = sum(pr)) %>% 
  mutate(date = as.Date(paste0(year, "-", month, "-01"))) %>% 
  ungroup() %>% 
  dplyr::select(-month, -year)


ggplot(extracted_data_pr, aes(x = date, y = pr, col = site)) + geom_line() + geom_smooth()
ggplot(extracted_data_pr, aes(x = month(date), y = pr, col = site)) + geom_line() + geom_smooth() + facet_grid(rows = vars(site))


## temperature 
ncdata <- rast(filenames[[2]])

ncdata <- ncdata[[time(ncdata) > as.Date("2000-01-01")]]

extracted_data <- terra::extract(ncdata, y = study_sites)

extracted_data <- tibble(extracted_data)

cols <- seq.Date(as.Date("2000-01-02"), as.Date("2023-12-31"), by = "day")
cols <- c("site", as.character(cols))

colnames(extracted_data) <- cols 

extracted_data_tasmin <- extracted_data %>% 
  mutate(site = c("Stroemsund", "Evenstad", "Bielowieza", "Pallasjarvi", "Asturias", "Le Quartier", "Havelaue", "Calabria", "Frýdek-Místek", "Konnovesi"), .before = 1) %>% 
  pivot_longer(cols = -1, names_to = "date", values_to = "tasmin") %>% 
  mutate(date = as.Date(date), month = month(date), year = year(date)) %>% 
  group_by(site, month, year) %>% 
  summarise(tasmin = mean(tasmin)) %>% 
  mutate(date = as.Date(paste0(year, "-", month, "-01"))) %>% 
  ungroup() %>% 
  dplyr::select(-month, -year)


ggplot(extracted_data_tasmin, aes(x = date, y = tasmin, col = site)) + geom_line() + geom_smooth()
ggplot(extracted_data_tasmin, aes(x = month(date), y = tasmin, col = site)) + geom_line() + geom_smooth()


extracted_data_all <- left_join(extracted_data_tasmin, extracted_data_pr, by = c("site", "date"))

write_csv(extracted_data_all, "Data/Scenarios/VI GCM scenarios/Inputs/EOBSdata.csv")

