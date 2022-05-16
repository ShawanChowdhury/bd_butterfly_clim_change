# Loading required libraries
library(raster)
library(dismo)
library(spThin)
library(tidyverse)
library(dplyr)

data <- read_csv("data/SpatialRecords_combined.csv")
head(data)
colnames(data) <- c("species", "decimalLongitude", "decimalLatitude")

sp <- unique(data$species)

for (i in sp) try({
  print(i)
  
  sp_data <- data %>% 
    filter(species == i)
  
  # Spatial thinning
  thinned_dataset_full <-
    thin( loc.data = sp_data, 
          lat.col = "decimalLatitude", long.col = "decimalLongitude", 
          spec.col = "species", 
          thin.par = 4.65, reps = 10000, 
          locs.thinned.list.return = TRUE, 
          write.files = FALSE, 
          write.log.file = FALSE)
  
  max_idx <- which.max(sapply(thinned_dataset_full, nrow))
  thinned_dataset_max_rows <- thinned_dataset_full [[max_idx]]
  colnames(thinned_dataset_max_rows) <- c("decimalLongitude", "decimalLatitude")
  
  thin_data <- thinned_dataset_max_rows %>%
    dplyr::select("decimalLongitude", "decimalLatitude") %>% 
    dplyr::mutate(species = i)
  
  write_csv(thin_data, paste0("data/sp_thin/", i, "_thin.csv"), row.names = FALSE)
  
}, silent = FALSE)


# Merge species
input_folder <- "data/sp_thin/"
list <- dir(input_folder, "^.*\\.csv$", full.names = TRUE)
ov <- plyr::ldply(list, readr::read_csv)

write_csv(ov, "data/combinedRecords_thinned.csv")

