# Loading required libraries
library(raster)
library(dismo)
library(sp)

# I downloaded both the current and the future climatic data from the WorldClim (www.worldclim.org) database.

########################
# Cropping climatic layers to Bangladesh extent
########################
# Bangladesh boundary
bd <- getData('GADM', country='BGD', level=1)

# Current climate
vars<-c("1.tif","2.tif","3.tif","4.tif","5.tif","6.tif","7.tif","8.tif","9.tif","10.tif","11.tif","12.tif","13.tif","14.tif","15.tif","16.tif","17.tif","18.tif","19.tif")
clim.stack <- stack(paste(getwd(),"/inputs/bio/", vars, sep=""))

# Cropping layer
r_bd <- crop(clim.stack, bd)
r_mask <- mask(r_bd, bd)

# Writing rasters
for (i in 1:nlayers(r_mask)){
  print(i)
  bio <- r_mask[[i]]
  writeRaster(bio, filename = file.path("inputs/clim/", i), format='GTiff')
}

########################
# Predictor variable collinearity
########################

#attaching climatic layers
vars<-c("1.tif","2.tif","3.tif","4.tif","5.tif","6.tif","7.tif","8.tif","9.tif","10.tif","11.tif","12.tif","13.tif","14.tif","15.tif","16.tif","17.tif","18.tif","19.tif")
clim.stack <- stack(paste(getwd(),"/clim/", vars, sep=""))

# Assesing correlations between predictor variables
pairs(clim.stack, maxpixels=20000)

# Highly correlated variables [r > 0.7]

# Predictor variables
vars<-c("1.tif","3.tif","9.tif","11.tif","14.tif","15.tif","17.tif","18.tif")
