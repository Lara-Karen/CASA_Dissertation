# Measure 6: Height

###### Packages Required #####
require(dplyr)
library(scales)
library(sf)

# creating subset dataframe and cleaning data
height = subset(total, select = c(physicalid, num_floors, st_width, id, num_bldgs))

###### CALCULATING SCORE #####
height <- height %>%
  mutate(bldg_height=num_floors*12.14)%>%
  filter(.,num_bldgs != 0)

## Save for plotting later
all_heights <- height

height <- height %>%
  group_by(physicalid) %>%
  dplyr::summarise(num_bldgs = sum(num_bldgs),
            bldg_height = sum(bldg_height),
            st_width = mean(st_width),
            count = n()) %>%
  filter(.,st_width != 0) %>%
  filter(.,bldg_height != 0)

height <- height %>%
  mutate(bldg_height_average=bldg_height/num_bldgs) %>%
  mutate(height_score=bldg_height_average/st_width)

###### DATA DISTRIBUTION #####
height$height_score_nozero <- (case_when (height$height_score == 0 ~ 0.0001,
                                          TRUE ~ height$height_score))
height$height_score_log <- log(height$height_score_nozero)
height$height_score <- rescale(height$height_score, to = c(0, 1), from = range(height$height_score, na.rm = TRUE, finite = FALSE))

###### PLOTTING BUILDING HEIGHT & STREET WIDTH  #####
# building height
color_height_width <- c("#e5f5e0", "#a1d99b", "#74c476", "#41ab5d", "#005a32")
cut_bldg_height <- c(0, 250, 500, 750, 1000, 1500)

tmap_mode("plot")
tm_shape(Borough_MN) +
  tm_borders(col = NA, lwd = 0.2, lty = "solid", alpha = 1)+
  tm_legend(show = TRUE) +
  tm_shape(CSCL) +
  tm_lines(col = "grey", alpha = 1, scale = 0.1, size = 100)+
  tm_legend(show = FALSE)+
  tm_shape(all_heights) +
  tm_style("white", title.size = 1.5, title.fontfamily = "Arial" ) +
  tm_fill(col = "bldg_height", scale = 2, palette = color_height_width, breaks = cut_bldg_height, title="Height in ft", contrast = 1) +
  tm_borders(col = NA, lwd = 0.01, lty = "solid", alpha = 1) +
  tm_legend(show = TRUE) +
  tm_scale_bar(position=c("right", "bottom"), color.dark="black", lwd = 1)+
  tm_layout("Building Height",
            title.size= 1.2,
            legend.title.size = 1,
            legend.text.size = 0.8,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 0.1) +
  tm_scale_bar(position=c("right", "bottom"), color.dark="black", lwd = 0.2)

## Street Width 
color_width <- c("#e5f5e0", "#a1d99b", "#41ab5d", "#238b45", "#005a32")
CSCL$st_width_thickness <- (case_when (CSCL$st_width >= 80 ~ 1000,
                                       CSCL$st_width >= 60 ~ 700,
                                       CSCL$st_width >= 40 ~ 250,
                                       CSCL$st_width >= 20 ~ 150,
                                       CSCL$st_width >= 0 ~ 100))
tm_shape(Borough_MN) +
  tm_polygons(col = NA, fill = NA, scale = 2.5, size = 1000, border.col = "grey", lwd = 0.5, lty = "solid", alpha = 0)+
  tm_legend(show = TRUE) +
  tm_shape(CSCL) + 
  tm_style("white", title.size = 1.5, title.fontfamily = "Arial" ) +
  tm_lines(col = "st_width", scale = 3, size = 1000, alpha = 0.8, palette = color_width, lwd= "st_width_thickness",   legend.lwd.show = FALSE, title.col="Width in ft") +
  tm_legend(show = TRUE) +
  tm_scale_bar(position=c("right", "bottom"), color.dark="black", lwd = 1)+
  tm_layout("Street Width",
            title.size= 1.2,
            legend.title.size = 1,
            legend.text.size = 0.8,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 0.1) +
  tm_scale_bar(position=c("right", "bottom"), color.dark="black", lwd = 0.2)

