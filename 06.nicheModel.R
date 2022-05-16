# Loading required libraries
library(sp)
library(raster)
library(dismo)
library(dplyr)
library(sf)
library(ENMeval)
library(stringr)
library(rgdal)
library(tidyverse)

# Reading data file
data <- read_csv("data/combinedRecords_thinned.csv")
head(data)

data <- data %>%
  dplyr::select("species", "decimalLongitude", "decimalLatitude")

# # Remove records without coordinates
# dc_cl <- dc_cl%>%
#   filter(!is.na(decimalLon))%>%
#   filter(!is.na(decimalLat))%>%
#   filter(!is.na(species))

# Remove species with low occurrence records
data_filter <- data %>%
  group_by(species) %>%
  filter(n() > 3) %>%
  ungroup()

# 7,534 records for 247 species

##########
# Present
#attaching climatic layers [present]
vars<-c("1.tif","3.tif","9.tif","11.tif","14.tif","15.tif","17.tif","18.tif")
clim.stack <- stack(paste(getwd(),"/inputs/clim/", vars, sep=""))

##########
# Future
#attaching climatic layers [ssp126]
clim.stack_ssp126 <- stack(paste(getwd(),"/inputs/clim_future/ssp126/", vars, sep=""))

#attaching climatic layers [ssp245]
clim.stack_ssp245 <- stack(paste(getwd(),"/inputs/clim_future/ssp245/", vars, sep=""))

#attaching climatic layers [ssp370]
clim.stack_ssp370 <- stack(paste(getwd(),"/inputs/clim_future/ssp370/", vars, sep=""))

#attaching climatic layers [ssp585]
clim.stack_ssp585 <- stack(paste(getwd(),"/inputs/clim_future/ssp585/", vars, sep=""))

species <- unique(data_filter$species)

for (i in species) try( {
  print(i)
  speciesname <- gsub(" ", "_", i)
  dc <- data_filter %>% filter(species==i)
  d.c<- dc[,2:3]
  d.c <- as.data.frame(d.c)
  d.c.sp <- SpatialPoints(d.c)
  bb <- bbox(d.c.sp)
  bb.buf <- extent(bb[1]-10, bb[3]+10, bb[2]-10, bb[4]+10)
  #clim.stack <- crop(clim.stack, bb.buf)
  bg<-randomPoints(clim.stack[[8]], n=10000)
  bg <- as.data.frame(bg)
  pred.mod.allyr <- ENMevaluate(d.c, clim.stack, bg, method='checkerboard2', 
                                RMvalues=seq(0.5, 4, 0.5), fc=c("L", "LQ", "H", "LQH", "LQHP", "LQHPT"), 
                                parallel=TRUE, kfolds = 10, algorithm='maxent.jar')
  
  # Selecting the best model
  aic.opt <- pred.mod.allyr@models[[which.max((pred.mod.allyr@results$avg.test.AUC))]]
  
  # Creating folder by species name
  
  output_dir <- paste0("outputs/", speciesname)
  dir.create(output_dir)
  
  # Extracting model output
  ModelOutput <- 
    as.data.frame(aic.opt@results) %>% 
    rownames_to_column(var = "param") %>%
    spread(key = param, value = V1) %>%
    mutate(species = speciesname) %>%
    dplyr::select(species, everything())
  
  write_csv(ModelOutput, file = paste0(output_dir, "/", speciesname, "_ModelOutput.csv"))
  
  # Variable contribution
  VariableContribution <- var.importance((aic.opt))
  write_csv(VariableContribution, file = paste0(output_dir, "/", speciesname, "_VariableContribution.csv"))
  
  # Suitability map
  r <- dismo::predict(aic.opt, clim.stack)
  
  # Model thresholding
  threshold <- ModelOutput$Maximum.training.sensitivity.plus.specificity.Cloglog.threshold
  r_bin <- r >= threshold
  writeRaster(r_bin, paste0(output_dir, "/", speciesname, "_current_bin.tif"), NAflag=-9999, overwrite = TRUE)
  
  
  # Future climate
  # ssp126
  future_ssp126 <- predict(aic.opt, clim.stack_ssp126)
  future_ssp126_bin <- future_ssp126 > threshold
  writeRaster(future_ssp126_bin, paste0(output_dir, "/", speciesname, "_ssp126_bin.tif"), 
              NAflag=-9999, overwrite = TRUE)
  
  # ssp245
  future_ssp245 <- predict(aic.opt, clim.stack_ssp245)
  future_ssp245_bin <- future_ssp245 > threshold
  writeRaster(future_ssp245_bin, paste0(output_dir, "/", speciesname, "_ssp245_bin.tif"), 
              NAflag=-9999, overwrite = TRUE)
  
  # ssp370
  future_ssp370 <- predict(aic.opt, clim.stack_ssp370)
  future_ssp370_bin <- future_ssp370 > threshold
  writeRaster(future_ssp370_bin, paste0(output_dir, "/", speciesname, "_ssp370_bin.tif"), 
              NAflag=-9999, overwrite = TRUE)
  
  # ssp585
  future_ssp585 <- predict(aic.opt, clim.stack_ssp585)
  future_ssp585_bin <- future_ssp585 >  threshold
  writeRaster(future_ssp585_bin, paste0(output_dir, "/", speciesname, "_ssp585_bin.tif"), 
              NAflag=-9999, overwrite = TRUE)
  
  
}, silent = FALSE)