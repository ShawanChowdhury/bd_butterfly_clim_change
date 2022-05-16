# Loading required libraries
library(sp)
library(raster)
library(dismo)
library(dplyr)
library(sf)
library(stringr)
library(rgdal)
library(tidyverse)
library(geosphere)

# List of rasters
tifs <- list.files(path = "outputs/sdm/",pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)

# Importing species data
spatial_data <- read_csv("data/combinedRecords_thinned.csv")
sp <- unique(spatial_data$species)

for (i in sp) try({
  print(i)
  
  # Subset species
  tifs_species <- tifs[stringr::str_detect(tifs, i)]
  
  # Running analysis for species with suitability maps
  if(NROW(tifs_species > 0)){
    
    # Creating folder by species name
    output_dir <- paste0("outputs/RangeCalculation/", i)
    dir.create(output_dir)
    
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
    
    # Calculating area
    area_current <- cellStats(current, "sum") * 21.625
    area_ssp126 <- cellStats(ssp126, "sum") * 21.625
    area_ssp245 <- cellStats(ssp245, "sum") * 21.625
    area_ssp370 <- cellStats(ssp370, "sum") * 21.625
    area_ssp585 <- cellStats(ssp585, "sum") * 21.625
    
    # Niche overlap
    niche_overlap_ssp126 <- nicheOverlap(current, ssp126, stat = "I", mask = TRUE, checkNegatives = TRUE)
    niche_overlap_ssp245 <- nicheOverlap(current, ssp245, stat = "I", mask = TRUE, checkNegatives = TRUE)
    niche_overlap_ssp370 <- nicheOverlap(current, ssp370, stat = "I", mask = TRUE, checkNegatives = TRUE)
    niche_overlap_ssp585 <- nicheOverlap(current, ssp585, stat = "I", mask = TRUE, checkNegatives = TRUE)
    
    # Calculating centroid
    centroid_current <- colMeans(xyFromCell(current, which(current[]==1)))
    centroid_current_c <- str_c(centroid_current, collapse = ",")
    
    centroid_ssp126 <- colMeans(xyFromCell(ssp126, which(ssp126[]==1)))
    centroid_ssp126_c <- str_c(centroid_ssp126, collapse = ",")
    
    centroid_ssp245 <- colMeans(xyFromCell(ssp245, which(ssp245[]==1)))
    centroid_ssp245_c <- str_c(centroid_ssp245, collapse = ",")
    
    centroid_ssp370 <- colMeans(xyFromCell(ssp370, which(ssp370[]==1)))
    centroid_ssp370_c <- str_c(centroid_ssp370, collapse = ",")
    
    centroid_ssp585 <- colMeans(xyFromCell(ssp585, which(ssp585[]==1)))
    centroid_ssp585_c <- str_c(centroid_ssp585, collapse = ",")
    
    # Distance between centroids
    centroid_distance_ssp126 <- distm(centroid_current, centroid_ssp126, fun = distHaversine)/1000
    centroid_distance_ssp245 <- distm(centroid_current, centroid_ssp245, fun = distHaversine)/1000
    centroid_distance_ssp370 <- distm(centroid_current, centroid_ssp370, fun = distHaversine)/1000
    centroid_distance_ssp585 <- distm(centroid_current, centroid_ssp585, fun = distHaversine)/1000
    
    # Angle between centroids
    centroid_shift_angle_ssp126 <- bearing(centroid_current, centroid_ssp126)
    centroid_shift_angle_ssp126 <- (centroid_shift_angle_ssp126+360) %% 360
    
    centroid_shift_angle_ssp245 <- bearing(centroid_current, centroid_ssp245)
    centroid_shift_angle_ssp245 <- (centroid_shift_angle_ssp245+360) %% 360
    
    centroid_shift_angle_ssp370 <- bearing(centroid_current, centroid_ssp370)
    centroid_shift_angle_ssp370 <- (centroid_shift_angle_ssp370+360) %% 360
    
    centroid_shift_angle_ssp585 <- bearing(centroid_current, centroid_ssp585)
    centroid_shift_angle_ssp585 <- (centroid_shift_angle_ssp585+360) %% 360
    
    ##################
    # Finding range expansion/contraction/overlap regions
    # ssp126
    # Range expansion
    expansion_ssp126 <- current - ssp126
    expansion_ssp126[expansion_ssp126 > -1]<-NA
    writeRaster(expansion_ssp126, paste0(output_dir, "/", i, "_expansion_ssp126.tif"), 
                NAflag=-9999, overwrite = TRUE)
    expansion_area_ssp126 <- cellStats(expansion_ssp126, "sum") * 21.625
    
    # Range contraction
    contraction_ssp126 <- ssp126 - current
    contraction_ssp126[contraction_ssp126 > -1]<-NA
    writeRaster(contraction_ssp126, paste0(output_dir, "/", i, "_contraction_ssp126.tif"), 
                NAflag=-9999, overwrite = TRUE)
    contraction_area_ssp126 <- cellStats(contraction_ssp126, "sum") * 21.625
    
    # Overlapped region
    overlap_ssp126 <- current + ssp126
    overlap_ssp126[overlap_ssp126 < 2]<-NA
    writeRaster(overlap_ssp126, paste0(output_dir, "/", i, "_overlap_ssp126.tif"), 
                NAflag=-9999, overwrite = TRUE)
    overlap_area_ssp126 <- cellStats(overlap_ssp126, "sum") * 21.625
    
    #################
    # ssp245
    # Range expansion
    expansion_ssp245 <- current - ssp245
    expansion_ssp245[expansion_ssp245 > -1]<-NA
    writeRaster(expansion_ssp245, paste0(output_dir, "/", i, "_expansion_ssp245.tif"), 
                NAflag=-9999, overwrite = TRUE)
    expansion_area_ssp245 <- cellStats(expansion_ssp245, "sum") * 21.625
    
    # Range contraction
    contraction_ssp245 <- ssp245 - current
    contraction_ssp245[contraction_ssp245 > -1]<-NA
    writeRaster(contraction_ssp245, paste0(output_dir, "/", i, "_contraction_ssp245.tif"), 
                NAflag=-9999, overwrite = TRUE)
    contraction_area_ssp245 <- cellStats(contraction_ssp245, "sum") * 21.625
    
    # Overlapped region
    overlap_ssp245 <- current + ssp245
    overlap_ssp245[overlap_ssp245 < 2]<-NA
    writeRaster(overlap_ssp245, paste0(output_dir, "/", i, "_overlap_ssp245.tif"), 
                NAflag=-9999, overwrite = TRUE)
    overlap_area_ssp245 <- cellStats(overlap_ssp245, "sum") * 21.625
    
    #############
    # ssp370
    # Range expansion
    expansion_ssp370 <- current - ssp370
    expansion_ssp370[expansion_ssp370 > -1]<-NA
    writeRaster(expansion_ssp370, paste0(output_dir, "/", i, "_expansion_ssp370.tif"), 
                NAflag=-9999, overwrite = TRUE)
    expansion_area_ssp370 <- cellStats(expansion_ssp370, "sum") * 21.625
    
    # Range contraction
    contraction_ssp370 <- ssp370 - current
    contraction_ssp370[contraction_ssp370 > -1]<-NA
    writeRaster(contraction_ssp370, paste0(output_dir, "/", i, "_contraction_ssp370.tif"), 
                NAflag=-9999, overwrite = TRUE)
    contraction_area_ssp370 <- cellStats(contraction_ssp370, "sum") * 21.625
    
    # Overlapped region
    overlap_ssp370 <- current + ssp370
    overlap_ssp370[overlap_ssp370 < 2]<-NA
    writeRaster(overlap_ssp370, paste0(output_dir, "/", i, "_overlap_ssp370.tif"), 
                NAflag=-9999, overwrite = TRUE)
    overlap_area_ssp370 <- cellStats(overlap_ssp370, "sum") * 21.625
    
    #################
    # ssp585
    # Range expansion
    expansion_ssp585 <- current - ssp585
    expansion_ssp585[expansion_ssp585 > -1]<-NA
    writeRaster(expansion_ssp585, paste0(output_dir, "/", i, "_expansion_ssp585.tif"), 
                NAflag=-9999, overwrite = TRUE)
    expansion_area_ssp585 <- cellStats(expansion_ssp585, "sum") * 21.625
    
    # Range contraction
    contraction_ssp585 <- ssp585 - current
    contraction_ssp585[contraction_ssp585 > -1]<-NA
    writeRaster(contraction_ssp585, paste0(output_dir, "/", i, "_contraction_ssp585.tif"), 
                NAflag=-9999, overwrite = TRUE)
    contraction_area_ssp585 <- cellStats(contraction_ssp585, "sum") * 21.625
    
    # Overlapped region
    overlap_ssp585 <- current + ssp585
    overlap_ssp585[overlap_ssp585 < 2]<-NA
    writeRaster(overlap_ssp585, paste0(output_dir, "/", i, "_overlap_ssp585.tif"), 
                NAflag=-9999, overwrite = TRUE)
    overlap_area_ssp585 <- cellStats(overlap_ssp585, "sum") * 21.625
    
    #############
    df <- as.data.frame(i)
    colnames(df) <- "species"
    
    merged_data <- df %>% 
      dplyr::mutate(area_current = area_current,
                    area_ssp126 = area_ssp126,
                    area_ssp245 = area_ssp245,
                    area_ssp370 = area_ssp370,
                    area_ssp585 = area_ssp585,
                    niche_overlap_ssp126 = niche_overlap_ssp126,
                    niche_overlap_ssp245 = niche_overlap_ssp245,
                    niche_overlap_ssp370 = niche_overlap_ssp370,
                    niche_overlap_ssp585 = niche_overlap_ssp585,
                    centroid_current_c = centroid_current_c,
                    centroid_ssp126_c = centroid_ssp126_c,
                    centroid_ssp245_c = centroid_ssp245_c,
                    centroid_ssp370_c = centroid_ssp370_c,
                    centroid_ssp585_c = centroid_ssp585_c,
                    centroid_distance_ssp126 = centroid_distance_ssp126,
                    centroid_distance_ssp245 = centroid_distance_ssp245,
                    centroid_distance_ssp370 = centroid_distance_ssp370,
                    centroid_distance_ssp585 = centroid_distance_ssp585,
                    centroid_shift_angle_ssp126 = centroid_shift_angle_ssp126,
                    centroid_shift_angle_ssp245 = centroid_shift_angle_ssp245,
                    centroid_shift_angle_ssp370 = centroid_shift_angle_ssp370,
                    centroid_shift_angle_ssp585 = centroid_shift_angle_ssp585,
                    expansion_area_ssp126 = expansion_area_ssp126,
                    contraction_area_ssp126 = contraction_area_ssp126,
                    overlap_area_ssp126 = overlap_area_ssp126,
                    expansion_area_ssp245 = expansion_area_ssp245,
                    contraction_area_ssp245 = contraction_area_ssp245,
                    overlap_area_ssp245 = overlap_area_ssp245,
                    expansion_area_ssp370 = expansion_area_ssp370,
                    contraction_area_ssp370 = contraction_area_ssp370,
                    overlap_area_ssp370 = overlap_area_ssp370,
                    expansion_area_ssp585 = expansion_area_ssp585,
                    contraction_area_ssp585 = contraction_area_ssp585,
                    overlap_area_ssp585 = overlap_area_ssp585)
    
    write_csv(merged_data, file = paste0(output_dir, "/", i, "_merged_data.csv"))
    
    rm(area_current, area_ssp126, area_ssp245, area_ssp370, area_ssp585,
       niche_overlap_ssp126, niche_overlap_ssp245, niche_overlap_ssp370, niche_overlap_ssp585,
       centroid_current_c, centroid_ssp126_c, centroid_ssp245_c, centroid_ssp370_c, centroid_ssp585_c,
       centroid_distance_ssp126, centroid_distance_ssp245, centroid_distance_ssp370, centroid_distance_ssp585,
       centroid_shift_angle_ssp126, centroid_shift_angle_ssp245, centroid_shift_angle_ssp370, centroid_shift_angle_ssp585,
       expansion_area_ssp126, contraction_area_ssp126, overlap_area_ssp126,
       expansion_area_ssp245, contraction_area_ssp245, overlap_area_ssp245,
       expansion_area_ssp370, contraction_area_ssp370, overlap_area_ssp370,
       expansion_area_ssp585, contraction_area_ssp585, overlap_area_ssp585)
  }
  
}, silent = FALSE)