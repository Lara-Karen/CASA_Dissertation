# Measure 7: Use

###### Packages Required #####
require(dplyr)
library(scales)
library(sf)

# creating subset dataframe and cleaning data
use = subset(total, select = c(physicalid, retail_area, office_area, res_area, com_area, bldg_area, id, num_bldgs))

###### CALCULATING SCORE #####
use <- use %>%
  mutate(total_area=retail_area + office_area + res_area + com_area) %>%
  mutate(commercial_total= retail_area + office_area + com_area) %>%
  mutate(residential_total = res_area) %>%
  filter(.,num_bldgs != 0)

#### calculating landuse for chloropeth map####
landuse <- use %>%
  mutate(residential_ratio = residential_total/total_area) %>%
  mutate(commercial_ratio = commercial_total/total_area) %>%
  filter(.,num_bldgs != 0)

landuse$residential_ratio[is.na(landuse$residential_ratio)] <- 0
landuse$commercial_ratio[is.na(landuse$commercial_ratio)] <- 0

#### calculating landuse for street score####
use <- use %>%
  group_by(physicalid) %>%
  dplyr::summarise(num_bldgs = sum(num_bldgs),
            commercial_total = sum(commercial_total),
            residential_total = sum(residential_total),
            total_area = sum(total_area),
            count = n()) %>%
  filter(.,num_bldgs != 0)

use <- use %>%
  mutate(residential_ratio = residential_total/total_area) %>%
  mutate(commercial_ratio = commercial_total/total_area) %>%
  filter(.,num_bldgs != 0)

# Mixed Score
use <- use %>%
  filter(.,num_bldgs != 0) %>%
  mutate(`mixed_score` = case_when(
    residential_ratio ==  1  ~ 0,
    residential_ratio ==  0.5  ~ 1,
    residential_ratio >= 0.9  ~ 0.2,
    residential_ratio >= 0.8  ~ 0.4,
    residential_ratio >= 0.7  ~ 0.6,
    residential_ratio >= 0.6  ~ 0.8,
    residential_ratio >= 0.4  ~ 0.8,
    residential_ratio >= 0.3  ~ 0.6,
    residential_ratio >= 0.2  ~ 0.4,
    residential_ratio >= 0.1  ~ 0.2,
    residential_ratio >= 0  ~ 0))

