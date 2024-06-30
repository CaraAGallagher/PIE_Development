# Cara Gallagher
# October 17th, 2023
# Energy Budget with Individual Variation project  
# Testing buffer size for scenario site selection  

##################################################
# Packages:
library(tidyverse)
library(raster)
library(sp)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(showtext)


#------------------------------------------------#

font_add_google(name = "Urbanist", family = "Normal")
showtext_auto()

##################################################

# Load forest density data 
forest_density <- raster("../../EBIVFirstApplication/Forest density data/DATA/TCD_2018_010m_eu_03035_V2_0.tif")

# Create a data frame with study site coordinates
study_sites <- data.frame(
  Site = c("Stroemsund", 
           "Evenstad", 
           "Bielowieza", 
           "Pallasjarvi", 
           "Asturias", 
           "Le Quartier", 
           "Havelaue", 
           "Calabria", 
           "Frýdek-Místek", 
           "Konnovesi"),
  Latitude = c(64.083333,
               61.500000,
               52.666667,
               68.000000,
               43.000000,
               46.140000,
               52.840000,
               39.350000,
               49.700000,
               62.616667),
  Longitude = c(15.833333,
                11.166667,
                23.666667,
                24.166667,
                -6.400000,
                2.730000,
                12.100000,
                16.490000,
                18.380000,
                26.283333)
)

# Define the LAEA coordinate system (CRS)
crs_string <- proj4string(forest_density)
crs <- CRS(crs_string)

# Create a SpatialPointsDataFrame with the Decimal Degree coordinates
coordinates(study_sites) <- c("Longitude", "Latitude")
proj4string(study_sites) <- CRS("+proj=longlat +datum=WGS84") # WGS 84


# Transform to the LAEA projection
study_sites <- spTransform(study_sites, crs)

# Print the results
print(study_sites)

study_sites <- data.frame(
  Name = study_sites$Site,
  Latitude = study_sites@coords[,1],
  Longitude = study_sites@coords[,2]
)

world <- ne_countries(scale = "medium", returnclass = "sf")
Europe <- world[which(world$continent == "Europe"),]

siteMapPlot <- ggplot(Europe) +
       geom_sf() +
       coord_sf(xlim = c(2e+06, 7400000), ylim = c(9e+05, 5500000), expand = FALSE,crs = proj4string(forest_density), datum = st_crs(forest_density)) + 
  geom_point(data = study_sites, aes(x = Latitude, y = Longitude), size = 2, col = "deeppink4") +
  theme_bw() +
  theme(text = element_text(family = "Montserrat", size = 40))
siteMapPlot
#ggsave("Figures/ScenInPlots/Site map.png", siteMapPlot, width = 7, height = 5)

plot(forest_density)

# Define a range of boundary sizes 
boundary_sizes <- c(158.1, 300, 500, 1000, 2000, 3000)
boundary_sizes <- c(500)

# Create and extract data for each site and boundary size
results <- list()
for (site_id in 1:10) {
  for (size in boundary_sizes) {
    
    site <- study_sites[site_id, ]
    boundary <- as(extent(study_sites$Latitude[site_id] - size, study_sites$Latitude[site_id] + size, study_sites$Longitude[site_id] - size, study_sites$Longitude[site_id] + size), 'SpatialPolygons')

    print(site)
    print(boundary)

    crs(boundary) <- crs(forest_density)
    print(boundary)
    
    forest_density_crop <- crop(x = forest_density, y = boundary)
    valuesM <- cellStats(forest_density_crop, 'mean') 
    valuesSD <- cellStats(forest_density_crop, 'sd') 
  
    results[[paste0(site$Name, "_Size", size)]] <- data.frame(Site = site$Name, BoundarySize = size, TreeDensityMean = valuesM, TreeDensitySD = valuesSD)
  }
}

# Combine the results into a data frame
result_df <- do.call(rbind, results)


# Create and extract data for each site and boundary size
results <- list()
boundary_sizes <- c(500)

for (site_id in 1:10) {
  for (size in boundary_sizes) {
    
    site <- study_sites[site_id, ]
    boundary <- as(extent(study_sites$Latitude[site_id] - size, study_sites$Latitude[site_id] + size, study_sites$Longitude[site_id] - size, study_sites$Longitude[site_id] + size), 'SpatialPolygons')
    
    print(site)
    print(boundary)
    
    crs(boundary) <- crs(forest_density)
    
    results[[paste0(site$Name)]] <- data.frame(Site = site$Name, xmin = xmin(boundary), xmax = xmax(boundary), ymin = ymin(boundary), ymax = ymax(boundary))
  }
}

# Combine the results into a data frame
result_df <- do.call(rbind, results)

study_sites_t <- data.frame(
  Name = result_df$Site,
  Latitude = result_df$xmax,
  Longitude = result_df$ymax
)

coordinates(study_sites_t) <- c("Longitude", "Latitude")
proj4string(study_sites_t) <- crs_string
study_sites_t@coords

world <- ne_countries(scale = "medium", returnclass = "sf")
Europe <- world[which(world$continent == "Europe"),]

siteMapPlot <- ggplot(Europe) +
  geom_sf() +
  coord_sf(xlim = c(2e+06, 7400000), ylim = c(9e+05, 5500000), expand = FALSE,crs = proj4string(forest_density), datum = st_crs(forest_density)) + 
  geom_point(data = as.data.frame(study_sites_t), aes(x = Latitude, y = Longitude), size = 2, col = "deeppink4") +
  theme_bw() +
  theme(text = element_text(family = "Montserrat"))
siteMapPlot






