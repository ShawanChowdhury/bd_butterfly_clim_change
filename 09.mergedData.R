# Merge range size analysis CSVs
input_folder <- "outputs/RangeData_CSVs/"
range <- dir(input_folder, ".csv$", full.names = TRUE)
range_merged <- plyr::ldply(range, data.table::fread)
head(range_merged)
write_csv(range_merged, "outputs/range_data_merged.csv")

#################
# Adding lines between current and the future range centroids
centroid <- read_csv("outputs/centroids.csv")
head(centroid)

centroid_current <- as.data.frame(centroid$centroid_current_c)
colnames(centroid_current) <- c("current")
centroid_future <- as.data.frame(centroid$centroid_ssp126_c)
colnames(centroid_future) <- c("future")

centroid_current_sep <- centroid_current %>% separate(current, c('lon', 'lat'), sep = ",")
centroid_current_sep$lat <- as.numeric(centroid_current_sep$lat)
centroid_current_sep$lon <- as.numeric(centroid_current_sep$lon)

centroid_future_sep <- centroid_future %>% separate(future, c('lon', 'lat'), sep = ",")
centroid_future_sep$lat <- as.numeric(centroid_future_sep$lat)
centroid_future_sep$lon <- as.numeric(centroid_future_sep$lon)


# Create list of simple feature geometries (linestrings)
l_sf <- vector("list", nrow(centroid_current_sep))

for (i in seq_along(l_sf)){
  l_sf[[i]] <- st_linestring(as.matrix(rbind(centroid_current_sep[i, ], centroid_future_sep[i,])))
}

# Create simple feature geometry list column
l_sfc <- st_sfc(l_sf, crs = "+proj=longlat +datum=WGS84")

# Convert to `sp` object if needed
lines_sp <- as(l_sfc, "Spatial")

###################
# Centroid shift angle
centroid_shift_angle_ssp126 <- read_csv("outputs/centroid_shift_angle_ssp126.csv")
head(centroid_shift_angle_ssp126)

# Spatial records
spatial_record <- read_csv("data/cleanedRecords_up.csv")
head(spatial_record)

sp_thrt <- spatial_record[,c(5,11)]
sp_thrt <- unique(sp_thrt)

# Combined with the centroid data
data <- dplyr::left_join(centroid_shift_angle_ssp126, sp_thrt, by = "species")
head(data)

write_csv(data, "outputs/centroid_angle_ssp126_thrt.csv")

#####################
# Changes in range size and niche
# Importing species data
range <- read_csv("outputs/shift_centroid_range_niche.csv")

# Importing species data to extract taxonomic group and threat status
spatial_data <- read_csv("data/cleanedRecords_up.csv")
head(spatial_data)

taxa_thrt <- spatial_data %>% 
  select(group, species, thrt_status) %>% 
  unique()

range_merged <- dplyr::left_join(range, taxa_thrt, by = "species")
write_csv(range_merged, "outputs/range_shift_taxa_thrt.csv")

#####################
# Adding classification [centroid shift]
sp_data <- read_csv("data/cleanedRecords_up.csv")
head(sp_data)

sp_data <- sp_data %>% 
  select(species, family) %>% 
  unique()

centroid_shift <- read_csv("outputs/centroid_angle_ssp126_thrt.csv")
head(centroid_shift)

shift_classification <- dplyr::left_join(centroid_shift, sp_data, by = c("species"))

write_csv(shift_classification, "outputs/centroid_shift_classification.csv")

#####################
# Adding classification [overall shift]
sp_data <- read_csv("data/cleanedRecords_up.csv")
head(sp_data)

sp_data <- sp_data %>% 
  select(species, family) %>% 
  unique()

centroid_shift <- read_csv("outputs/range_shift_taxa_thrt.csv")
head(centroid_shift)

shift_classification <- dplyr::left_join(centroid_shift, sp_data, by = c("species"))

write_csv(shift_classification, "outputs/range_shift_taxa_thrt_classification.csv")

###################
# Summary-direction
data <- read_csv("outputs/centroid_shift_classification.csv")
head(data)

sum_data <- data %>% 
  group_by(direction, thrt_status) %>% 
  summarise(n = NROW(direction))


##################
# Merging elevation data
# Merge range size analysis CSVs
input_folder <- "outputs/elevation/"
elev <- dir(input_folder, ".csv$", full.names = TRUE)
elev_merged <- plyr::ldply(elev, data.table::fread)
head(elev_merged)

# Merging with threat status
spatial_data <- read_csv("data/cleanedRecords_up.csv")
head(spatial_data)

taxa_thrt <- spatial_data %>% 
  select(species, thrt_status) %>% 
  unique()

elev_merged_thrt <- dplyr::left_join(elev_merged, taxa_thrt, by = "species")

write_csv(elev_merged_thrt, "outputs/elev_merged_thrt.csv", row.names = FALSE)
