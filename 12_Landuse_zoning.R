# Landuse Zoning Map

###### Packages Required #####
library(sf)
library(stringr)
library(dplyr)
library(here)

###### DATASETS ######
zone_districts <- st_read (here::here("DATA",
                                 "zoning",
                                 "nyzd.shp")) 

###### DATACLEANING ######
MN_Zoning <- st_join(st_make_valid(zone_districts), Borough_MN, left = FALSE, largest = TRUE)

# filtering out zones that are out of the study area
MN_Zoning<-MN_Zoning[!(MN_Zoning$ZONEDIST=="PARK"),]
MN_Zoning<-MN_Zoning[!(MN_Zoning$ZONEDIST=="PLAYGROUND"),]
MN_Zoning<-MN_Zoning[!(MN_Zoning$Shape_Area== 14586715),]
MN_Zoning<-MN_Zoning[!(MN_Zoning$Shape_Area==15048277),]
MN_Zoning<-MN_Zoning[!(MN_Zoning$Shape_Area==474794.2),]
MN_Zoning<-MN_Zoning[!(MN_Zoning$Shape_Area==645845),]
MN_Zoning<-MN_Zoning[!(MN_Zoning$Shape_Area==486721.6),]
MN_Zoning<-MN_Zoning[!(MN_Zoning$Shape_Area==697095.0),]
MN_Zoning<-MN_Zoning[!(MN_Zoning$Shape_Area==3401605.4),]
MN_Zoning<-MN_Zoning[!(MN_Zoning$Shape_Area==33515192),]
MN_Zoning<-MN_Zoning[!(MN_Zoning$Shape_Area==76612005.4),]
MN_Zoning<-MN_Zoning[!(MN_Zoning$Shape_Area==1599478.6),]
MN_Zoning<-MN_Zoning[!(MN_Zoning$Shape_Area==3048040.8),]
MN_Zoning<-MN_Zoning[!(MN_Zoning$Shape_Area==4239284),]

# simplifying the zone names
MN_Zoning[MN_Zoning$ZONEDIST=="M1-2", "ZONEDIST"] <- "M1"
MN_Zoning[MN_Zoning$ZONEDIST=="M1-1", "ZONEDIST"] <- "M1"
MN_Zoning[MN_Zoning$ZONEDIST=="M1-4", "ZONEDIST"] <- "M1"
MN_Zoning[MN_Zoning$ZONEDIST=="M1-4/R7A", "ZONEDIST"] <- "M1"
MN_Zoning[MN_Zoning$ZONEDIST=="M1-4/R9A", "ZONEDIST"] <- "M1"
MN_Zoning[MN_Zoning$ZONEDIST=="M1-5/R7-2", "ZONEDIST"] <- "M1"
MN_Zoning[MN_Zoning$ZONEDIST=="M1-5/R7X", "ZONEDIST"] <- "M1"
MN_Zoning[MN_Zoning$ZONEDIST=="M1-5", "ZONEDIST"] <- "M1"
MN_Zoning[MN_Zoning$ZONEDIST=="M1-5A", "ZONEDIST"] <- "M1"
MN_Zoning[MN_Zoning$ZONEDIST=="M1-5B", "ZONEDIST"] <- "M1"
MN_Zoning[MN_Zoning$ZONEDIST=="M1-5M", "ZONEDIST"] <- "M1"
MN_Zoning[MN_Zoning$ZONEDIST=="M1-6", "ZONEDIST"] <- "M1"
MN_Zoning[MN_Zoning$ZONEDIST=="M1-6/R10", "ZONEDIST"] <- "M1"
MN_Zoning[MN_Zoning$ZONEDIST=="M1-6/R9", "ZONEDIST"] <- "M1"
MN_Zoning[MN_Zoning$ZONEDIST=="M1-6D", "ZONEDIST"] <- "M1"
MN_Zoning[MN_Zoning$ZONEDIST=="M2-1", "ZONEDIST"] <- "M2"
MN_Zoning[MN_Zoning$ZONEDIST=="M2-2", "ZONEDIST"] <- "M2"
MN_Zoning[MN_Zoning$ZONEDIST=="M2-3", "ZONEDIST"] <- "M2"
MN_Zoning[MN_Zoning$ZONEDIST=="M2-4", "ZONEDIST"] <- "M2"
MN_Zoning[MN_Zoning$ZONEDIST=="M3-1", "ZONEDIST"] <- "M3"
MN_Zoning[MN_Zoning$ZONEDIST=="M3-2", "ZONEDIST"] <- "M3"
MN_Zoning[MN_Zoning$ZONEDIST=="C1-6", "ZONEDIST"] <- "C1"
MN_Zoning[MN_Zoning$ZONEDIST=="C1-6A", "ZONEDIST"] <- "C1"
MN_Zoning[MN_Zoning$ZONEDIST=="C1-7", "ZONEDIST"] <- "C1"
MN_Zoning[MN_Zoning$ZONEDIST=="C1-7A", "ZONEDIST"] <- "C1"
MN_Zoning[MN_Zoning$ZONEDIST=="C1-8", "ZONEDIST"] <- "C1"
MN_Zoning[MN_Zoning$ZONEDIST=="C1-8A", "ZONEDIST"] <- "C1"
MN_Zoning[MN_Zoning$ZONEDIST=="C1-8X", "ZONEDIST"] <- "C1"
MN_Zoning[MN_Zoning$ZONEDIST=="C1-9", "ZONEDIST"] <- "C1"
MN_Zoning[MN_Zoning$ZONEDIST=="C1-9A", "ZONEDIST"] <- "C1"
MN_Zoning[MN_Zoning$ZONEDIST=="C2-6", "ZONEDIST"] <- "C2"
MN_Zoning[MN_Zoning$ZONEDIST=="C2-6A", "ZONEDIST"] <- "C2"
MN_Zoning[MN_Zoning$ZONEDIST=="C2-7", "ZONEDIST"] <- "C2"
MN_Zoning[MN_Zoning$ZONEDIST=="C2-7A", "ZONEDIST"] <- "C2"
MN_Zoning[MN_Zoning$ZONEDIST=="C2-8", "ZONEDIST"] <- "C2"
MN_Zoning[MN_Zoning$ZONEDIST=="C2-8A", "ZONEDIST"] <- "C2"
MN_Zoning[MN_Zoning$ZONEDIST=="C3", "ZONEDIST"] <- "C3"
MN_Zoning[MN_Zoning$ZONEDIST=="C4-1", "ZONEDIST"] <- "C4"
MN_Zoning[MN_Zoning$ZONEDIST=="C4-2F", "ZONEDIST"] <- "C4"
MN_Zoning[MN_Zoning$ZONEDIST=="C4-4", "ZONEDIST"] <- "C4"
MN_Zoning[MN_Zoning$ZONEDIST=="C4-4A", "ZONEDIST"] <- "C4"
MN_Zoning[MN_Zoning$ZONEDIST=="C4-4D", "ZONEDIST"] <- "C4"
MN_Zoning[MN_Zoning$ZONEDIST=="C4-5", "ZONEDIST"] <- "C4"
MN_Zoning[MN_Zoning$ZONEDIST=="C4-5A", "ZONEDIST"] <- "C4"
MN_Zoning[MN_Zoning$ZONEDIST=="C4-5D", "ZONEDIST"] <- "C4"
MN_Zoning[MN_Zoning$ZONEDIST=="C4-5X", "ZONEDIST"] <- "C4"
MN_Zoning[MN_Zoning$ZONEDIST=="C4-6", "ZONEDIST"] <- "C4"
MN_Zoning[MN_Zoning$ZONEDIST=="C4-6A", "ZONEDIST"] <- "C4"
MN_Zoning[MN_Zoning$ZONEDIST=="C4-7", "ZONEDIST"] <- "C4"
MN_Zoning[MN_Zoning$ZONEDIST=="C4-6A", "ZONEDIST"] <- "C4"
MN_Zoning[MN_Zoning$ZONEDIST=="C4-6A", "ZONEDIST"] <- "C4"
MN_Zoning[MN_Zoning$ZONEDIST=="C4-6A", "ZONEDIST"] <- "C4"
MN_Zoning[MN_Zoning$ZONEDIST=="C4-6A", "ZONEDIST"] <- "C4"
MN_Zoning[MN_Zoning$ZONEDIST=="C4-6A", "ZONEDIST"] <- "C4"
MN_Zoning[MN_Zoning$ZONEDIST=="C4-6A", "ZONEDIST"] <- "C4"
MN_Zoning[MN_Zoning$ZONEDIST=="C4-6A", "ZONEDIST"] <- "C4"
MN_Zoning[MN_Zoning$ZONEDIST=="C5-1", "ZONEDIST"] <- "C5"
MN_Zoning[MN_Zoning$ZONEDIST=="C5-1A", "ZONEDIST"] <- "C5"
MN_Zoning[MN_Zoning$ZONEDIST=="C5-2", "ZONEDIST"] <- "C5"
MN_Zoning[MN_Zoning$ZONEDIST=="C5-2.5", "ZONEDIST"] <- "C5"
MN_Zoning[MN_Zoning$ZONEDIST=="C5-3", "ZONEDIST"] <- "C5"
MN_Zoning[MN_Zoning$ZONEDIST=="C5-5", "ZONEDIST"] <- "C5"
MN_Zoning[MN_Zoning$ZONEDIST=="C5-P", "ZONEDIST"] <- "C5"
MN_Zoning[MN_Zoning$ZONEDIST=="C6-1", "ZONEDIST"] <- "C6"
MN_Zoning[MN_Zoning$ZONEDIST=="C6-1G", "ZONEDIST"] <- "C6"
MN_Zoning[MN_Zoning$ZONEDIST=="C6-2A", "ZONEDIST"] <- "C6"
MN_Zoning[MN_Zoning$ZONEDIST=="C6-2G", "ZONEDIST"] <- "C6"
MN_Zoning[MN_Zoning$ZONEDIST=="C6-2M", "ZONEDIST"] <- "C6"
MN_Zoning[MN_Zoning$ZONEDIST=="C6-3", "ZONEDIST"] <- "C6"
MN_Zoning[MN_Zoning$ZONEDIST=="C6-3A", "ZONEDIST"] <- "C6"
MN_Zoning[MN_Zoning$ZONEDIST=="C6-3X", "ZONEDIST"] <- "C6"
MN_Zoning[MN_Zoning$ZONEDIST=="C6-4", "ZONEDIST"] <- "C6"
MN_Zoning[MN_Zoning$ZONEDIST=="C6-4.5", "ZONEDIST"] <- "C6"
MN_Zoning[MN_Zoning$ZONEDIST=="C6-4A", "ZONEDIST"] <- "C6"
MN_Zoning[MN_Zoning$ZONEDIST=="C6-4M", "ZONEDIST"] <- "C6"
MN_Zoning[MN_Zoning$ZONEDIST=="C6-4X", "ZONEDIST"] <- "C6"
MN_Zoning[MN_Zoning$ZONEDIST=="C6-5", "ZONEDIST"] <- "C6"
MN_Zoning[MN_Zoning$ZONEDIST=="C6-5.5", "ZONEDIST"] <- "C6"
MN_Zoning[MN_Zoning$ZONEDIST=="C6-6", "ZONEDIST"] <- "C6"
MN_Zoning[MN_Zoning$ZONEDIST=="C6-2", "ZONEDIST"] <- "C6"
MN_Zoning[MN_Zoning$ZONEDIST=="C6-6.5", "ZONEDIST"] <- "C6"
MN_Zoning[MN_Zoning$ZONEDIST=="C6-7", "ZONEDIST"] <- "C6"
MN_Zoning[MN_Zoning$ZONEDIST=="C6-7T", "ZONEDIST"] <- "C6"
MN_Zoning[MN_Zoning$ZONEDIST=="C6-9", "ZONEDIST"] <- "C6"
MN_Zoning[MN_Zoning$ZONEDIST=="C8-3", "ZONEDIST"] <- "C8"
MN_Zoning[MN_Zoning$ZONEDIST=="C8-4", "ZONEDIST"] <- "C8"
MN_Zoning[MN_Zoning$ZONEDIST=="R3-2", "ZONEDIST"] <- "R3"
MN_Zoning[MN_Zoning$ZONEDIST=="R6A", "ZONEDIST"] <- "R6"
MN_Zoning[MN_Zoning$ZONEDIST=="R1-2", "ZONEDIST"] <- "R1"
MN_Zoning[MN_Zoning$ZONEDIST=="R7-1", "ZONEDIST"] <- "R7"
MN_Zoning[MN_Zoning$ZONEDIST=="R7-2", "ZONEDIST"] <- "R7"
MN_Zoning[MN_Zoning$ZONEDIST=="R7A", "ZONEDIST"] <- "R7"
MN_Zoning[MN_Zoning$ZONEDIST=="R7B", "ZONEDIST"] <- "R7"
MN_Zoning[MN_Zoning$ZONEDIST=="R7D", "ZONEDIST"] <- "R7"
MN_Zoning[MN_Zoning$ZONEDIST=="R7X", "ZONEDIST"] <- "R7"
MN_Zoning[MN_Zoning$ZONEDIST=="R8A", "ZONEDIST"] <- "R8"
MN_Zoning[MN_Zoning$ZONEDIST=="R8B", "ZONEDIST"] <- "R8"
MN_Zoning[MN_Zoning$ZONEDIST=="R8X", "ZONEDIST"] <- "R8"
MN_Zoning[MN_Zoning$ZONEDIST=="R9", "ZONEDIST"] <- "R9"
MN_Zoning[MN_Zoning$ZONEDIST=="R9-1", "ZONEDIST"] <- "R9"
MN_Zoning[MN_Zoning$ZONEDIST=="R9A", "ZONEDIST"] <- "R9"
MN_Zoning[MN_Zoning$ZONEDIST=="R9X", "ZONEDIST"] <- "R9"
MN_Zoning[MN_Zoning$ZONEDIST=="R10A", "ZONEDIST"] <- "R910"
MN_Zoning[MN_Zoning$ZONEDIST=="R10H", "ZONEDIST"] <- "R910"
MN_Zoning[MN_Zoning$ZONEDIST=="R10", "ZONEDIST"] <- "R910"

# creating new datasets for each zone
zone_manufacturing  <- MN_Zoning %>%
  filter(str_detect(MN_Zoning$ZONEDIST, "M+"))

zone_commercial  <- MN_Zoning %>%
  filter(str_detect(MN_Zoning$ZONEDIST, "C.+"))

zone_residential  <- MN_Zoning %>%
  filter(str_detect(MN_Zoning$ZONEDIST, "R.+"))

###### PLOTTING ######
# specifying the colors for each zone
residential_colors <- c("#deebf7", "#c6dbef", "#9ecae1" , "#6baed6", "#4292c6", "#2171b5", "#08519c")
commercial_colors <- c("#e5f5e0", "#c7e9c0", "#a1d99b" , "#74c476", "#41ab5d", "#238b45", "#006d2c")
manufacturing_colors <- c("#fc9272" , "#ef3b2c", "#a50f15")

# plotting
tmap_mode("plot")
tm_shape(Borough_MN) +
  tm_borders(col = NA, lwd = 0.2, lty = "solid", alpha = 1)+
  tm_legend(show = TRUE) +
  tm_shape(CSCL) +
  tm_lines(col = "grey", alpha = 1, scale = 0.1, size = 100)+
  tm_legend(show = FALSE)+
  tm_shape(zone_manufacturing) +
  tm_fill(col = "ZONEDIST", title = "Manufacturing Districts", scale = 1, size = 100, palette = manufacturing_colors, contrast = 1) +
  tm_legend(show = TRUE, title = "Landuse Districts") +
  tm_shape(zone_commercial) +
  tm_fill(col = "ZONEDIST" , title = "Commercial Districts", scale = 1, size = 100, palette = commercial_colors, contrast = 1) +
  tm_legend(show = TRUE, title = "Landuse Districts") +
  tm_shape(zone_residential) +
  tm_fill(col = "ZONEDIST" , title = "Residential Districts", scale = 1, size = 100, palette = residential_colors, contrast = 1) +
  tm_legend(show = TRUE) +
  tm_layout("Zoning Districts",
            title.size=1.1,
            legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 0.1) +
  tm_scale_bar(position=c("right", "bottom"), color.dark="black", lwd = 0.3)