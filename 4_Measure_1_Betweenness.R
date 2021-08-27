# Measure 1: Betweenness Centrality

###### Packages Required #####
library(sf)
library(mapview)
library(geojsonsf)
library(scales)
library(janitor)
library(here)
library(dplyr)
library(base)
library(utils)

###### DATASETS #####
# the betweenness was calculated using NetworkX on Python
centrality_csv <- read.csv(here::here("DATA",
                                    "betweenness.csv")) %>%
  clean_names()

centrality <- merge(CSCL, centrality_csv ,by=c("physicalid")) %>%
  filter(., betweenness_e > 0)

rm(centrality_csv)

###### DATA DISTRIBUTION #####
# plotting distribution before
centrality$betweenness_ee <- rescale(centrality$betweenness_e, to = c(0, 1), from = range(centrality$betweenness_e, na.rm = TRUE, finite = TRUE))
hist(centrality$betweenness_ee)

# normalizing and calculating distribution
centrality$betweenness <- log(centrality$betweenness_e)

centrality$betweenness <- rescale(centrality$betweenness, to = c(0, 1), from = range(centrality$betweenness, na.rm = TRUE, finite = TRUE))
centrality$betweenness  <- round(centrality$betweenness, digits=10)

# checking if distribution is normal
hist(centrality$betweenness)
