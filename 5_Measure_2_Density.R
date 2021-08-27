# Measure 2: Density

###### Packages Required #####
require(dplyr)
library(scales)
library(sf)

# creating subset dataframe and cleaning data
density = subset(total, select = c(physicalid, shape_leng.y, num_bldgs, full_stree, st_name))

density <- density %>% rename(street_length = shape_leng.y) %>% st_drop_geometry()

###### CALCULATING SCORE #####
density <- density %>% 
  group_by(physicalid) %>% 
  summarise(num_bldgs = sum(num_bldgs))

street_length <- subset(total, select = c(physicalid, lot_area, shape_leng.y, full_stree, st_name)) %>%
  st_drop_geometry()

density <- merge(density, street_length ,by=c("physicalid"))

density <- density%>%
  mutate(density_e=num_bldgs/shape_leng.y) %>%
  filter(., density_e != 0)

###### DATA DISTRIBUTION #####
density$density <- log(density$density_e)
density$density <- rescale(density$density, to = c(0, 1), from = range(density$density, na.rm = TRUE, finite = TRUE))
