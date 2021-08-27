# Data Cleaning

###### Packages Required #####
library(janitor)
library(sf)
library(dplyr)
library(here)
library(rgdal)

###### DATASETS ######

here::here()

## TAX LOTS
# data downloaded from: https://www1.nyc.gov/site/planning/data-maps/open-data/dwn-pluto-mappluto.page
PLUTO <- st_read(here::here("DATA",
                            "MapPLUTO",
                            "MapPLUTO.shp")) %>%
  filter(., borough=="MN") %>%
  st_transform(., 2263) %>%
  clean_names()

PLUTO = subset(PLUTO, select = -c(borough)) %>%
  mutate(PLUTO, id = rownames(PLUTO))

## STREET NETWORK
# data downloaded from: https://data.cityofnewyork.us/City-Government/NYC-Street-Centerline-CSCL-/exjm-f27b


CSCL <- st_read(here::here("DATA",
                           "CSCL",
                           "geo_export.shp")) 

## GEOMETRY DATAFRAME; creating a dataframe with the geography and ID to be used throughout the analysis 
geo = subset(CSCL, select = c(physicalid, full_stree))
