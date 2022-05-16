# Loading required libraries
library(tidyverse)
library(ggplot2)

##########
# Centroid shift angle
data <- read_csv("outputs/centroid_angle_ssp126_thrt.csv")
head(data)

ggplot(data, aes(centroid_shift_angle_ssp126, fill = thrt_status)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  # This add the bars with a blue color
  geom_histogram(stat="bin", col = "black") +
  # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
  xlim(0,360) +
  # Custom the theme: no axis title and no cartesian grid
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "top",
        text = element_text(size = 14)) +
  # This makes the coordinate polar instead of cartesian.
  coord_polar(start = 0) + scale_fill_manual(values = c("blue", "orange")) +
  scale_x_continuous(limits=c(0,360), breaks = seq(0,360,45)) +
  xlab("Range shift in degrees") + ylab("Number of species")
ggsave("figures/range_shift_degrees.png")

#####################################
# Changes in range size and niche
# Importing data
range <- read_csv("outputs/range_shift_taxa_thrt.csv")
head(range)

range <- range %>% 
  mutate(niche_overlap_ssp126 = niche_overlap_ssp126*100)

# Centroid shift
ggplot(range, aes(centroid_distance_ssp126, fill = thrt_status)) +
  geom_histogram(bins = 50) + 
  xlab("") + ylab("") + theme_bw() +
  scale_fill_manual(values = c("blue", "orange"))  +
  theme(legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.background = element_blank())
ggsave("figures/centroid_shift.png")

# Changes in range size
ggplot(range, aes(area_change_ssp126, fill = thrt_status)) +
  geom_histogram(bins = 50) + 
  xlab("") + ylab("") + theme_bw() +
  scale_fill_manual(values = c("blue", "orange"))  +
  theme(legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.background = element_blank()) + geom_vline(xintercept = 0, lty = "dashed")
ggsave("figures/range_change.png")

# Changes in range size
ggplot(range, aes(niche_overlap_ssp126, fill = thrt_status)) +
  geom_histogram(bins = 50) + 
  xlab("") + ylab("") + theme_bw() +
  scale_fill_manual(values = c("blue", "orange"))  +
  theme(legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.background = element_blank())
ggsave("figures/niche_comparison.png")

##########################
# Elevation
elev <- read_csv("outputs/elev_merged_thrt.csv")
head(elev)
elev <- elev[,c(1:3, 7)]
colnames(elev) <- c("species", "Current", "Future", "thrt_status")

# # Pivot longer
# elev_longer <- pivot_longer(elev,
#                             cols = c(Current, Future),
#                             names_to = "Condition",
#                             values_to = "Elevation")

elev_change <- elev %>% 
  mutate(Change = (Future-Current))

write_csv(elev_change, "outputs/elevation_shift_ssp126.csv")

ggplot(elev_change, aes(Change, fill = thrt_status)) +
  geom_histogram(bins = 50) +
  xlab("") + ylab("") + theme_bw() + 
  scale_fill_manual(values = c("blue", "orange")) + 
  scale_color_manual(values = c("blue", "orange"))  +
  theme(legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.background = element_blank()) + geom_vline(xintercept = 0, lty = "dashed")
ggsave("figures/elevation_shift.png")
