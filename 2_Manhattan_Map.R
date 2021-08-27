# Manhattan Map

###### Packages Required #####
library(dplyr)
library(tmap)
library(grid)
library(tmap)
library(tigris)
library(sp)
library(rgeos)
library(here)
library(sf)


###### DATASETS ######

## All Five Boroughs of New York 
Borough <- st_read(here::here("DATA",
                            "Borough",
                            "borough_boundaries.shp")) %>%
  st_transform(., 2263)

## Boroughs of New York without Manhattan
Borough_No_MN <- st_read(here::here("DATA",
                                "Borough",
                                "borough_boundaries.shp")) %>%
    filter(., boro_name!="Manhattan") %>%
  st_transform(., 2263)

## Borough of Manhattan Only
Borough_MN <- st_read(here::here("DATA",
                              "Borough",
                              "borough_boundaries.shp")) %>%
  filter(., boro_name=="Manhattan") %>%
  st_transform(., 2263)

## Neighborhoods of Manhattan 
Neighborhood_MN <- st_read(here::here("DATA",
                                 "Neighborhood",
                                 "NTA.shp")) %>%
  filter(., boro_name=="Manhattan") %>%
  st_transform(., 2263)

###### DATACLEANING FOR NEIGHBORHOODS ######

# cleaning the names
Neighborhood_MN[Neighborhood_MN$ntaname=="Murray Hill-Kips Bay", "ntaname"] <- "Murray Hill"
Neighborhood_MN[Neighborhood_MN$ntaname=="Turtle Bay-East Midtown", "ntaname"] <- "East Midtown"
Neighborhood_MN[Neighborhood_MN$ntaname=="East Harlem North", "ntaname"] <- "Harlem"
Neighborhood_MN[Neighborhood_MN$ntaname=="Stuyvesant Town-Cooper Village", "ntaname"] <- "Cooper Village"
Neighborhood_MN[Neighborhood_MN$ntaname=="Battery Park City-Lower Manhattan", "ntaname"] <- "Lower Manhattan"
Neighborhood_MN[Neighborhood_MN$ntaname=="East Harlem South", "ntaname"] <- "Harlem"
Neighborhood_MN[Neighborhood_MN$ntaname=="Lenox Hill-Roosevelt Island", "ntaname"] <- "Roosevelt Island"
Neighborhood_MN[Neighborhood_MN$ntaname=="Upper East Side-Carnegie Hill", "ntaname"] <- "Upper East Side"
Neighborhood_MN[Neighborhood_MN$ntaname=="SoHo-TriBeCa-Civic Center-Little Italy", "ntaname"] <- "Soho & TriBeCa"
Neighborhood_MN[Neighborhood_MN$ntaname=="Midtown-Midtown South", "ntaname"] <- "Midtown"
Neighborhood_MN[Neighborhood_MN$ntaname=="Washington Heights North", "ntaname"] <- "Washington Heights"
Neighborhood_MN[Neighborhood_MN$ntaname=="Washington Heights South", "ntaname"] <- "Washington Heights"
Neighborhood_MN[Neighborhood_MN$ntaname=="Marble Hill-Inwood", "ntaname"] <- "Marble Hill"
Neighborhood_MN[Neighborhood_MN$ntaname=="Central Harlem North-Polo Grounds", "ntaname"] <- "Harlem"
Neighborhood_MN[Neighborhood_MN$ntaname=="Central Harlem South", "ntaname"] <- "Harlem"
Neighborhood_MN[Neighborhood_MN$ntaname=="Hudson Yards-Chelsea-Flatiron-Union Square", "ntaname"] <- "Union Square"
Neighborhood_MN[Neighborhood_MN$ntaname=="park-cemetery-etc-Manhattan", "ntaname"] <- "Park"

# creating subset with only the required attributes
N_MN = subset(Neighborhood_MN, select = c(ntaname))

# grouping neighborhoods with the same name
N_MN %>% 
  group_by(ntaname) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()

# matching numbers to the neighborhood names in order to display in the plot
N_MN$numbers <- match(N_MN$ntaname, unique(N_MN$ntaname))
N_MN<-N_MN[!(N_MN$numbers=="25"),]
as.integer(factor(Neighborhood_MN$ntaname))

###### PLOTS ######

## Study area: Plotting the 5 Boroughs of New York highlighting Manhattan
tmap_mode("view")
tm_shape(Borough_No_MN) +
  tm_polygons(col = NA, alpha = 0.4, scale = 2, size = 100) +
  tm_text("boro_name", col= NA , palette="Dark2",
          title.size = NA, title.col="boro_name") +
  tm_shape(Borough_MN) +
  tm_polygons(col = "boro_name", title = "Study Area", alpha = 1, scale = 2, size = 100)


## Map of Neighborhoods
# specifying colors
neighborhood_colors <- c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3", "#fdb462" ,"#b3de69", "#fccde5", "#d9d9d9", "#bc80bd", "#ccebc5" ,"#ffed6f")
# setting mode
tmap_mode("plot")
# plotting
tm_shape(N_MN) +
    tm_polygons(col = "ntaname", title = "Neighborhoods", alpha = 0.5, scale = 1, size = 100)+
    tm_legend(show = TRUE) +
    tm_text("numbers", size=0.8)+
  tm_layout("Manhattan Neighborhoods",
            legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1) +
  tm_scale_bar(position=c("right", "bottom"), color.dark="black", lwd = 1)



