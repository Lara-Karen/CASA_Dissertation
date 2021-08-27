# Measure 5: Alignment

###### Packages Required #####
require(dplyr)
library(scales)
library(sf)
library(plyr)

# creating subset dataframe and cleaning data
alignment = subset(total, select = c(physicalid, lot_depth, bldg_depth, block, id, num_bldgs))

###### CALCULATING SCORE #####
alignment <- alignment %>%
  mutate(depth_no_building=lot_depth - bldg_depth)%>%
  filter(.,num_bldgs != 0) %>%
  filter(.,bldg_depth != 0)

alignment$depth_no_building_round  <- round_any(alignment$depth_no_building, 5, round)

alignment <- alignment %>%
  group_by(physicalid, block, depth_no_building_round) %>%
  dplyr::summarise(count = n(),
                   num_bldgs = sum(num_bldgs))%>%
  filter(.,num_bldgs != 0)

alignment <- alignment %>%
  group_by(physicalid, block) %>%
  dplyr::summarise(num_bldgs = sum(num_bldgs),
                   count = max(count),
                   num_bldgs = sum(num_bldgs))%>%
  filter(.,num_bldgs != 0)

alignment <- alignment %>%
  mutate(alignment_score = count/num_bldgs)

# inverting score to reflect desired outcome
alignment <- alignment%>%
  mutate(alignment_score = 1 - alignment_score)

