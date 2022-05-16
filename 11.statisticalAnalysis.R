library(readr)
library(lme4)
library(glmmTMB)
library(ggplot2)
library(tidyverse)
library(DHARMa)
library(car)
library(emmeans)

data <- read_csv("outputs/glm_data.csv")
head(data)

# Numeric - numeric value
data$range_shift <- as.numeric(data$range_shift)
data$elevation <- as.numeric(data$elevation)
data$centroid_distance <- as.numeric(data$centroid_distance, na.rm = T)

# Factor - categorical value
data$migr_status <- as.factor(data$migr_status)
data$thrt_status <- as.factor(data$thrt_status)


# Histogram
hist(data$range_shift)
hist(data$elevation)
hist(data$centroid_distance)

##########################
### Threat status [thrt vs non-thrt]
### Centroid shift
# Build model
m1 <- glmmTMB(centroid_distance ~ thrt_status + 
                (1|family), family = nbinom2, data = data)

# Summary results
summary(m1)

residsim <- DHARMa::simulateResiduals(m1, n = 1000)
DHARMa::testResiduals(residsim)

Anova(m1, type = 3)

### Elevation shift
# Build model
head(data)
data <- data %>% 
  mutate(elevation_shift_up = elevation_shift+37.75231195) # lowest (-) elevation shift + 1

m3 <- glmmTMB(elevation_shift_up ~ thrt_status + 
                (1|family), family = nbinom2, data = data)

# Summary results
summary(m3)

residsim <- DHARMa::simulateResiduals(m3, n = 1000)
DHARMa::testResiduals(residsim)

Anova(m3, type = 3)


##########################
### Elevation and centroid shift
# Rescaling
data$elevation_rescale <- scale(data$elevation)

# Build model
m1 <- glmmTMB(centroid_distance ~ elevation_rescale + migr_status + elevation_rescale*migr_status + 
                (1|family), family = nbinom2, data = data)

# Summary results
summary(m1)
testDispersion(m1)
simulationOutput <- simulateResiduals(fittedModel = m1, plot = F)
residuals(simulationOutput)
plot(simulationOutput)
Anova(m1, type = 3)

## Post-hoc pair-wise comparison
em1 <- emmeans(m1, pairwise~elevation_rescale*migr_status, type = "response")
em1

##########################
### Area and centroid shift
# Build model
data$centroid_distance_rescale <- scale(data$centroid_distance)
data$area_current_rescale <- scale(data$area_current)

hist(data$area_current)

m2 <- glmmTMB(centroid_distance ~ area_current_rescale + migr_status + area_current_rescale*migr_status + 
                (1|family), family = nbinom2, data = data)

# Summary results
summary(m2)
testDispersion(m2)
simulationOutput <- simulateResiduals(fittedModel = m2, plot = F)
residuals(simulationOutput)
plot(simulationOutput)
Anova(m2, type = 3)

## Post-hoc pair-wise comparison
em2 <- emmeans(m2, pairwise~area_current_rescale*migr_status, type = "response")
em2

plot(em2)

