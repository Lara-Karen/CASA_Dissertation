# Measure 3: Dimension

###### Packages Required #####
require(dplyr)
library(scales)
library(sf)

# creating subset dataframe and cleaning data
dimension = subset(total, select = c(physicalid, lot_front, lot_area, block))

###### CALCULATING SCORE #####
dimension <- dimension %>% 
  group_by(physicalid) %>% 
  dplyr::summarise(perimeter = sum(lot_front),
            block_area = sum(lot_area))

dimension$perimeter[is.na(dimension$perimeter)] <- 0
dimension$block_area[is.na(dimension$block_area)] <- 0

block_area <- subset(total, select = c(physicalid, lot_area)) %>%
  st_drop_geometry()

block_area <- block_area %>% 
  group_by_all() %>% 
  summarise(total_area = sum(lot_area))

# total block area = 407171441

dimension <- dimension%>%
  mutate(dimension_score = perimeter * (block_area/407171441))

# inverting the score
dimension <- dimension%>%
  mutate(dimension_score_inverse = 1 - dimension_score)

###### DATA DISTRIBUTION #####
dimension$dimension_score_trial <- (case_when (dimension$dimension_score == 0 ~ 0.0001,
                                               TRUE ~ dimension$dimension_score))
dimension$dimension_score_log <- log(dimension$dimension_score_trial)
dimension$dimension_score <- rescale(dimension$dimension_score_log, to = c(0, 1), from = range(dimension$dimension_score_log, na.rm = TRUE, finite = FALSE))

