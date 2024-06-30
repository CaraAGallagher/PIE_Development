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
pal <- colorRampPalette(c("#3F3F7B","#278192","#00B089","#AABC4A", "#E5DC54","#E4AA4E","#E4714E","#BC4A53"))


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
crs_string <-"+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
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

study_sites <- study_sites %>%
  mutate(
    Name = fct_relevel(
      Name,
      "Pallasjarvi",
      "Stroemsund",
      "Konnovesi",
      "Evenstad",
      "Havelaue",
      "Bielowieza",
      "Frýdek-Místek",
      "Le Quartier",
      "Asturias",
      "Calabria"
    )
  )

world <- ne_countries(scale = "medium", returnclass = "sf")
Europe <- world[which(world$continent == "Europe"),]

siteMapPlot <- ggplot(Europe) +
       geom_sf() +
       coord_sf(xlim = c(2e+06, 7400000), ylim = c(9e+05, 5500000), expand = FALSE,crs = proj4string(forest_density)) + 
  geom_point(data = study_sites, aes(x = Latitude, y = Longitude, fill = Name), shape = 21, col = "grey30", size = 2.7, stroke = 1.1, show.legend = FALSE) +
  scale_fill_manual(values = pal(10)) + 
  labs(x ="Longitude", y = "Latitude") +
  theme_bw() +
  theme(text = element_text(family = "Montserrat",  color = "grey30", size = 50))
siteMapPlot
ggsave("Figures/ScenInPlots/Site map.png", siteMapPlot, width = 7, height = 5)
