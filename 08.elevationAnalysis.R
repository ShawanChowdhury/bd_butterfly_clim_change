# Loading required libraries
library(sp)
library(raster)
library(dismo)
library(dplyr)
library(sf)
library(data.table)
library(stringr)
library(rgdal)
library(tidyverse)
library(geosphere)

#######################
# Extracting and matching elevation data for Bangladesh
# Reading elev data
elev <- raster("R:/BUTTERFL19-A1712/PhD Analysis/Second Chapter/GlobalElevationData.tif")

# Reading Bangladesh shapefile
bd <- read_rds("gadm36_BGD_1_sp.rds")

# Cropping layer
elev_bd_crop <- crop(elev, bd)
elev_bd <- mask(elev_bd_crop, bd)

# Changing extent
dist <- raster("outputs/current/Arhopala_bazaloides_current_bin.tif")
extent(elev_bd) <- extent(dist)

# Converting to points
elev_bd_point <- as.data.frame(elev_bd, xy=TRUE)
colnames(elev_bd_point)[3] <- "elevation"


#######################
# Extracting mean elevation for each suitability distribution layer

# List of rasters
tifs <- list.files(path = "outputs/sdm/",pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)

# Importing species data
spatial_data <- read.csv("outputs/range_data_merged.csv")
sp <- unique(spatial_data$species)

for (i in sp) {
  print(i)
  
  # Subset species
  tifs_species <- tifs[stringr::str_detect(tifs, i)]
  
  # Subsetting by categories
  current <- tifs_species[stringr::str_detect(tifs_species, "current")]
  ssp126 <- tifs_species[stringr::str_detect(tifs_species, "ssp126")]
  ssp245 <- tifs_species[stringr::str_detect(tifs_species, "ssp245")]
  ssp370 <- tifs_species[stringr::str_detect(tifs_species, "ssp370")]
  ssp585 <- tifs_species[stringr::str_detect(tifs_species, "ssp585")]
  
  # Importing rasters
  current <- raster(current)
  ssp126 <- raster(ssp126)
  ssp245 <- raster(ssp245)
  ssp370 <- raster(ssp370)
  ssp585 <- raster(ssp585)
  
  # Converting to points
  current_point <- as.data.frame(current, xy=TRUE)
  ssp126_point <- as.data.frame(ssp126, xy=TRUE)
  ssp245_point <- as.data.frame(ssp245, xy=TRUE)
  ssp370_point <- as.data.frame(ssp370, xy=TRUE)
  ssp585_point <- as.data.frame(ssp585, xy=TRUE)
  
  # Renaming data frames
  colnames(current_point)[3] <- "presence_current"
  colnames(ssp126_point)[3] <- "presence_ssp126"
  colnames(ssp245_point)[3] <- "presence_ssp245"
  colnames(ssp370_point)[3] <- "presence_ssp370"
  colnames(ssp585_point)[3] <- "presence_ssp585"
  
  # Subsetting presence value
  current_point_pr <- subset(current_point, presence_current == 1)
  ssp126_point_pr <- subset(ssp126_point, presence_ssp126 == 1)
  ssp245_point_pr <- subset(ssp245_point, presence_ssp245 == 1)
  ssp370_point_pr <- subset(ssp370_point, presence_ssp370 == 1)
  ssp585_point_pr <- subset(ssp585_point, presence_ssp585 == 1)
  
  # Merging with the elevation data
  current_merge <- dplyr::left_join(current_point_pr, elev_bd_point, by = c("x", "y"))
  ssp126_merge <- dplyr::left_join(ssp126_point_pr, elev_bd_point, by = c("x", "y"))
  ssp245_merge <- dplyr::left_join(ssp245_point_pr, elev_bd_point, by = c("x", "y"))
  ssp370_merge <- dplyr::left_join(ssp370_point_pr, elev_bd_point, by = c("x", "y"))
  ssp585_merge <- dplyr::left_join(ssp585_point_pr, elev_bd_point, by = c("x", "y"))
  
  # Calculating mean elevation [and converting it into a dataframe]
  mean_elev_current <- as.data.frame(mean(current_merge$elevation, na.rm=TRUE))
  colnames(mean_elev_current) <- "mean_elev_current"
  
  mean_elev_ssp126 <- as.data.frame(mean(ssp126_merge$elevation, na.rm=TRUE))
  colnames(mean_elev_ssp126) <- "mean_elev_ssp126"
  
  mean_elev_ssp245 <- as.data.frame(mean(ssp245_merge$elevation, na.rm=TRUE))
  colnames(mean_elev_ssp245) <- "mean_elev_ssp245"
  
  mean_elev_ssp370 <- as.data.frame(mean(ssp370_merge$elevation, na.rm=TRUE))
  colnames(mean_elev_ssp370) <- "mean_elev_ssp370"
  
  mean_elev_ssp585 <- as.data.frame(mean(ssp585_merge$elevation, na.rm=TRUE))
  colnames(mean_elev_ssp585) <- "mean_elev_ssp585"
  
  
  # Merging dataframes
  df <- cbind(mean_elev_current, mean_elev_ssp126, mean_elev_ssp245, mean_elev_ssp370, mean_elev_ssp585)
  
  # Mutating species name and rearranging columns
  df <- df %>% 
    mutate(species = i) %>% 
    select(species, mean_elev_current, mean_elev_ssp126, mean_elev_ssp245, mean_elev_ssp370, mean_elev_ssp585)
  
  # Exporting output
  write_csv(df, file = paste0("outputs/elevation/", "/", i, "_mean_elevation.csv"), row.names = FALSE)
  
}


