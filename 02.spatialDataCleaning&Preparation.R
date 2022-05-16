# Maximising memory
options(java.parameters = "-Xmx6g")

# Loading required libraries
library(dplyr)
library(ggplot2)
library(countrycode)
library(CoordinateCleaner)
library(tidyverse)
library(rworldmap)

##############################################
# Cleaning GBIF data
# Reading GBIF data
gbif_data <- read_csv("data/gbif_data.csv")

# Remove duplicate records
gbif_data1 <- gbif_data[!duplicated(gbif_data),]

# Convert country code from ISO2c to ISO3c
gbif_data1$countryCode <-  countrycode(gbif_data1$countryCode, origin =  'iso2c', destination = 'iso3c')

# Flag problems
gbif_data1 <- data.frame(gbif_data1)
flags <- clean_coordinates(x = gbif_data1, lon = "decimalLongitude", lat = "decimalLatitude",
                           countries = "countryCode", 
                           species = "species",
                           tests = c("centroids", "gbif",
                                     "zeros", "countries")) # most test are on by default

# summary(flags)
# plot(flags, lon = "decimalLongitude", lat = "decimalLatitude")

# Exclude problematic records
gbif_data_cl <- gbif_data1[flags$.summary,]

# Writing output
write_csv(gbif_data_cl, "data/cleanedRecords_GBIF.csv")

# Clearing memory
rm(gbif_data, gbif_data1, flags)

##############################################
# Cleaning Facebook records
# Reading Facebook data
fb_data <- read_csv("data/facebookDataUp.csv")

# Changing column names
colnames(fb_data) <- c("class", "order", "family", "species", "commonName", "iucn", "lifeStage", "date",
                       "month", "year", "location", "decimalLatitude", "decimalLongitude", "photographer",
                       "comment", "status")

# Selecting columns of interests
fb_data <- fb_data %>% 
  dplyr::select(species, decimalLongitude, decimalLatitude)

# Remove blank cells
fb_data <- fb_data[!(is.na(fb_data$species) | fb_data$species == ""),]
fb_data <- fb_data[!(is.na(fb_data$decimalLongitude) | fb_data$decimalLongitude == ""),]
fb_data <- fb_data[!(is.na(fb_data$decimalLatitude) | fb_data$decimalLatitude == ""),]

# Writing output
write_csv(fb_data, "data/cleanedRecords_FB.csv")

##############################################
# Combining dataframes
##############################################
# Selecting columns of interests
gbif <- gbif_data_cl %>% 
  dplyr::select(species, decimalLongitude, decimalLatitude)
fb <- fb_data %>% 
  dplyr::select(species, decimalLongitude, decimalLatitude)

# Combining dataframes
combined_data <- rbind(fb, gbif)

# Remove species with low occurrence records
combined_data <- combined_data %>%
  group_by(species) %>%
  filter(n() > 3) %>%
  ungroup()

# Writing output
write_csv(combined_data, "data/cleanedRecords_combined.csv")

# ##############################################
# gbif <- gbif_data_cl %>% 
#   dplyr::select(species, decimalLongitude, decimalLatitude)
# clean <- read.csv("data/cleanedRecords_prev.csv", header = T)
# combined_data <- rbind(clean, gbif)
# 
# # Remove species with low occurrence records
# combined_data <- combined_data %>%
#   group_by(species) %>%
#   filter(n() > 3) %>%
#   ungroup()
# 
# write_csv(combined_data, "data/cleanedRecords.csv")