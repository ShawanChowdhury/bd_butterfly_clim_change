# Loading required libraries
library(raster)
library(dismo)
library(sp)

# Bangladesh boundary
bd <- readRDS("gadm36_BGD_1_sp.rds")

########################
# ssp126
########################
r <- stack("clim_future/CNRM-CM6-1_ssp126_2081-2100.tif") # stack for multiband raster

# Cropping layer
r_bd <- crop(r, bd)
r_mask <- mask(r_bd, bd)

for (i in 1:nlayers(r_mask)){
  print(i)
  bio <- r_mask[[i]]
  writeRaster(bio, filename = file.path("clim_future/ssp126/", i), format='GTiff')
}

########################
# ssp245
########################
r <- stack("clim_future/CNRM-CM6-1_ssp245_2081-2100.tif") # stack for multiband raster

# Cropping layer
r_bd <- crop(r, bd)
r_mask <- mask(r_bd, bd)

for (i in 1:nlayers(r_mask)){
  print(i)
  bio <- r_mask[[i]]
  writeRaster(bio, filename = file.path("clim_future/ssp245/", i), format='GTiff')
}

########################
# ssp370
########################
r <- stack("clim_future/CNRM-CM6-1_ssp370_2081-2100.tif") # stack for multiband raster

# Cropping layer
r_bd <- crop(r, bd)
r_mask <- mask(r_bd, bd)

for (i in 1:nlayers(r_mask)){
  print(i)
  bio <- r_mask[[i]]
  writeRaster(bio, filename = file.path("clim_future/ssp370/", i), format='GTiff')
}

########################
# ssp585
########################
r <- stack("clim_future/CNRM-CM6-1_ssp585_2081-2100.tif") # stack for multiband raster

# Cropping layer
r_bd <- crop(r, bd)
r_mask <- mask(r_bd, bd)

for (i in 1:nlayers(r_mask)){
  print(i)
  bio <- r_mask[[i]]
  writeRaster(bio, filename = file.path("clim_future/ssp585/", i), format='GTiff')
}

# I removed the highly correlated variables, which I obtained using the current climatic data.