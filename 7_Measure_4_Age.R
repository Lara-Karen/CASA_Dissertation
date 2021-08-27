# Measure 4: Age

###### Packages Required #####
require(dplyr)
library(scales)
library(sf)

# creating subset dataframe and cleaning data
age = subset(total, select = c(physicalid, year_built, id, num_bldgs))

###### CALCULATING SCORE #####
building_per_street <- age %>% 
  group_by(physicalid) %>% 
  summarise(num_bldgs_per_street = sum(num_bldgs)) 

building_per_street <- building_per_street %>% st_drop_geometry()

age <- age%>%
  filter(., year_built > 1799) %>%
  filter(.,num_bldgs != 0)

age <- age %>%
  mutate(`age` = case_when(
    year_built < 1916  ~ "Old",
    year_built > 1916  ~ "New",
    year_built == 1916 ~ "Old"))

age <- merge(age, building_per_street ,by=c("physicalid"), all = TRUE)

age <- age %>%
  group_by(physicalid, age) %>%
  summarise(num_bldgs = sum(num_bldgs),
            count = n(),
            num_bldgs_per_street = mean(num_bldgs_per_street))

age <- age %>%
  mutate(age_score=count/num_bldgs_per_street)%>%
  filter(., age=="Old") %>%
  filter(.,num_bldgs != 0) %>%
  filter(., age_score > 0)

###### DATA DISTRIBUTION #####
age$age_score <- rescale(age$age_score, to = c(0, 1), from = range(age$age_score, na.rm = TRUE, finite = TRUE))


###### PLOTTING #####

# Plotting Historic Districts
here::here()
historic_district <- st_read(here::here("DATA",
                            "historic_district",
                            "historic_district.shp")) %>%
  filter(., borough == "MN") %>%
  filter(., lp_number != "LP-01946") %>%
  filter(., lp_number != "LP-01902") %>%
  st_transform(., 2263) %>%
  clean_names()

colors_age <- c("#bcbddc", "#9e9ac8", "#807dba","#6a51a3", "#4a1486")

tmap_mode("plot")
tm_shape(Borough_MN) +
  tm_borders(col = NA, lwd = 0.2, lty = "solid", alpha = 1)+
  tm_legend(show = TRUE) +
  tm_shape(CSCL) +
  tm_lines(col = "grey", alpha = 1, scale = 0.75, size = 100)+
  tm_legend(show = FALSE)+
  tm_legend(show = TRUE, fontfamily= "Arial") +
  tm_shape(historic_district) +
  tm_polygons(col = "#807dba", alpha = 0.75, legend.show = TRUE, title="Historic District") +
  tm_layout("Historic District Boundaries",
            title.size= 1.2,
            legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 0.1) +
  tm_scale_bar(position=c("right", "bottom"), color.dark="black", lwd = 0.2)


# Plotting building ages "New Versus Old Buildings"
building_age = subset(total, select = c(physicalid, year_built, id, num_bldgs))%>%
  filter(.,num_bldgs != 0) %>%
  filter(.,physicalid != 3762) %>%
  filter(.,physicalid != 185582) %>%
  filter(.,physicalid != 3110) %>%
  filter(.,physicalid != 3125) %>%
  filter(.,physicalid != 3129) %>%
  filter(.,physicalid != 172180) %>%
  filter(.,physicalid != 3182) %>%
  filter(.,physicalid != 3213) %>%
  filter(.,physicalid != 3199) %>%
  filter(.,physicalid != 26946) %>%
  filter(.,physicalid != 186386) %>%
  filter(.,physicalid != 24933) %>%
  filter(.,physicalid != 89419) %>%
  filter(.,physicalid != 3285) %>%
  filter(.,physicalid != 3281) %>%
  filter(.,physicalid != 344) %>%
  filter(.,physicalid != 341) %>%
  filter(.,physicalid != 338) %>%
  filter(.,physicalid != 336) %>%
  filter(.,physicalid != 334) %>%
  filter(.,physicalid != 332) %>%
  filter(.,physicalid != 331) %>%
  filter(.,physicalid != 327) %>%
  filter(.,physicalid != 324) %>%
  filter(.,physicalid != 320) %>%
  filter(.,physicalid != 318) %>%
  filter(.,physicalid != 316) %>%
  filter(.,physicalid != 315) %>%
  filter(.,physicalid != 132055) %>%
  filter(.,physicalid != 313) %>%
  filter(.,physicalid != 147097) %>%
  filter(.,physicalid != 308) %>%
  mutate(`age` = case_when(
    year_built < 1916  ~ "Built Before 1916",
    year_built > 1916  ~ "Built after 1916",
    year_built == 1916 ~ "Built Before 1916"))

colors_age_building <- c("#fed9a6", "#bf5b17")

tmap_mode("plot")
tm_shape(Borough_MN) +
  tm_borders(col = NA, lwd = 0.2, lty = "solid", alpha = 1) +
  tm_legend(show = TRUE) +
  tm_shape(CSCL) +
  tm_lines(col = "grey", alpha = 1, scale = 0.1, size = 100)+
  tm_legend(show = FALSE)+
  tm_shape(building_age) +
  tm_fill("age", title="Building Age", palette = colors_age_building) +
  tm_borders(lwd=0.01, lty="solid", col = NA) +
  tm_legend(show = TRUE)+
  tm_layout("New Versus Old Buildings",
            title.size= 1.1,
            legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 0.1) +
  tm_scale_bar(position=c("right", "bottom"), color.dark="black", lwd = 0.2)
