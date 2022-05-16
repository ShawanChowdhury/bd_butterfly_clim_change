# Setting working directories
setwd("R:/BUTTERFL19-A1712/IMPORTANT/SideProjects/Bangladesh/NationalAssessment/Butterflies_ClimateChange")

# Loading required libraries
library(rgbif)
library(tidyverse)

# # Reading species list
# sp_list <- read_csv("data/gbifSpecies.csv", header = T)
# 
# sp <- unique(sp_list$species)
# 
# 
# for (i in sp) try({
#   print(i)
#   
#   occ_data <- occ_search(scientificName = i, hasCoordinate = T,
#                          fields=c("species", "decimalLongitude", "decimalLatitude", "countryCode", 
#                                   "gbifID", "coordinateUncertaintyInMeters", "year",
#                                   "basisOfRecord", "institutionCode", "datasetName"), limit=100000, country = "BD")$data
#   
#   if(nrow(occ_data > 0)) {
#     speciesname <- gsub(" ", "_", i)
#     # occ_data <- occ_data %>%
#     #   dplyr::mutate(species = speciesname)
#     
#     sp <- sp_list %>% 
#       filter(species == i)
#     
#     group <- sp$group
#     class <- sp$class
#     order <- sp$order
#     family <- sp$family
#     iucn <- sp$iucn
#     
#     occ_data <- occ_data %>%
#       dplyr::mutate(group = group,
#                     class = class,
#                     order = order,
#                     family = family,
#                     iucn = iucn,
#                     species = speciesname)
#     
#     write_csv(occ_data, file = paste0("data/gbif/", speciesname, ".csv"))
#   }
# }, silent = FALSE)
# 
# # Merging GBIF data
# input_folder = "data/gbif"
# gbif = dir(input_folder, "^.*\\.csv$", full.names = TRUE)
# gbif_data <- plyr::ldply(gbif, data.table::fread)
# 
# write_csv(gbif_data, "data/gbif_data.csv")
# 
# # Inspecting GBIF data
# gbif <- fread("data/gbif_data.csv")
# head(gbif)
# unique(gbif$species)

#########################
# This is a part of 'Conserving Bangladeshi biodiversity' project, where I collected species occurrence records
# using the above lines of codes. I missed the citation information of the download, therefore, I re-collected the 
# 'DOI of the download' using the following lines of codes. Thanks to Daniel Noesgaard (from GBIF) for his help.

# Downloading GBIF download DOI
rgbif::derived_dataset(citation_data = dd_shawan, user = "shawan_zl", pwd = "nabolakothaFB89",
                       title = "Spatial distribution records for Bangladeshi IUCN-listed species",
                       description = "Extracted butterfly data",
                       source_url = "https://zenodo.org/record/6440577#.YlN-8zV_WUk")

# DOI: https://doi.org/10.15468/dd.rghepr
