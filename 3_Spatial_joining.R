# Spatial Join

###### Packages Required #####
library(sf)
library(dplyr)
library(mapview)
library(geojsonsf)

###### JOINING USING SF_NEAREST_FEATURE #####
# using sf to get centroid of each plot
sf_cent <- st_centroid(PLUTO)

# getting the nearest feature attributes to the pluto centroid dataframe 
join = st_join(sf_cent, CSCL, join = st_nearest_feature)
join = subset(join, select = c(id, physicalid, st_width, st_name, st_label))

# changing sf to dataframe so we can merge
joined <- join %>% st_drop_geometry()
class(joined)

# setting an ID to each street
total <- merge(PLUTO, joined ,by=c("id"))

# filtering streets
total <- total %>%
  filter(.,id != 24840) %>%
  filter(.,id != 37241) %>%
  filter(.,id != 12695) %>%
  filter(.,id != 29240) %>%
  filter(.,id != 29243) %>%
  filter(.,id != 29266) %>%
  filter(.,id != 19397) %>% 
  filter(.,id != 29241) %>%
  filter(.,id != 19386) %>%
  filter(.,id != 28314) %>%
  filter(.,id != 29271) %>%
  filter(.,id != 19387)%>%
  filter(.,id != 31129) %>%
  filter(.,id != 28327) %>%
  filter(.,id != 28329) %>%
  filter(.,id != 19395) %>%
  filter(.,id != 31141) %>%
  filter(.,id != 12707) %>%
  filter(.,id != 28305) %>%
  filter(.,id != 31150) %>%
  filter(.,id != 19399) %>%
  filter(.,id != 12694) %>%
  filter(.,id != 29242) %>%
  filter(.,id != 28304) %>%
  filter(.,id != 39525) %>%
  filter(.,id != 34737) %>%
  filter(.,id != 25955) %>%
  filter(.,id != 34723) %>%
  filter(.,id != 37266) %>%
  filter(.,id != 37267) %>%
  filter(.,id != 25946) %>%
  filter(.,id != 19898) %>%
  filter(.,id != 42214) %>%
  filter(.,id != 31682) %>%
  filter(.,id != 38635) %>%
  filter(.,id != 42213) %>%
  filter(.,id != 41173) %>%
  filter(.,id != 41165) %>%
  filter(.,id != 41164) %>%
  filter(.,id != 31670) %>%
  filter(.,id != 42215) %>%
  filter(.,id != 41271) %>%
  filter(.,id != 28372) %>%
  filter(.,id != 41258) %>%
  filter(.,id != 42255) %>%
  filter(.,id != 42254) %>%
  filter(.,id != 41255) %>%
  filter(.,id != 41256) %>%
  filter(.,id != 31839) %>%
  filter(.,id != 28373) %>%
  filter(.,id != 41262) %>%
  filter(.,id != 42247) %>%
  filter(.,id != 38627) %>%
  filter(.,id != 42249) %>%
  filter(.,id != 31822) %>%
  filter(.,id != 31827) %>%
  filter(.,id != 42250) %>%
  filter(.,id != 41252) %>%
  filter(.,id != 38622) %>%
  filter(.,id != 41253) %>%
  filter(.,id != 41252) %>%
  filter(.,id != 38621) %>%
  filter(.,id != 41251) %>%
  filter(.,id != 31821) %>%
  filter(.,id != 41250) %>%
  filter(.,id != 41249) %>%
  filter(.,id != 41247) %>%
  filter(.,id != 41245) %>%
  filter(.,id != 31820) %>%
  filter(.,id != 31816) %>%
  filter(.,id != 31815) %>%
  filter(.,id != 41244) %>%
  filter(.,id != 42248) %>%
  filter(.,id != 42251) %>%
  filter(.,id != 31828) %>%
  filter(.,id != 42252) %>%
  filter(.,id != 38628) %>%
  filter(.,id != 41254) %>%
  filter(.,id != 31836) %>%
  filter(.,id != 42253) %>%
  filter(.,id != 38629) %>%
  filter(.,id != 38633) %>%
  filter(.,id != 38634) %>%
  filter(.,id != 28363) %>%
  filter(.,id != 31837) %>%
  filter(.,id != 31838) %>%
  filter(.,id != 31747) %>%
  filter(.,id != 31746) %>%
  filter(.,id != 31748) %>%
  filter(.,id != 31750) %>%
  filter(.,id != 31751) %>%
  filter(.,id != 42225) %>%
  filter(.,id != 42226) %>%
  filter(.,id != 42227) %>%
  filter(.,id != 31745) %>%
  filter(.,id != 38581) %>%
  filter(.,id != 31744) %>%
  filter(.,id != 41200) %>%
  filter(.,id != 31761) %>%
  filter(.,id != 31759) %>%
  filter(.,id != 41201) %>%
  filter(.,id != 41203) %>%
  filter(.,id != 31752) %>%
  filter(.,id != 41225) %>%
  filter(.,id != 31798) %>%
  filter(.,id != 41228) %>%
  filter(.,id != 41223) %>%
  filter(.,id != 41222) %>%
  filter(.,id != 42237) %>%
  filter(.,id != 38604) %>%
  filter(.,id != 38603) %>%
  filter(.,id != 38599) %>%
  filter(.,id != 31797) %>%
  filter(.,id != 31790) %>%
  filter(.,id != 31789) %>%
  filter(.,id != 31788) %>%
  filter(.,id != 41220) %>%
  filter(.,id != 41219) %>%
  filter(.,id != 41217) %>%
  filter(.,id != 41215) %>%
  filter(.,id != 42236) %>%
  filter(.,id != 42235) %>%
  filter(.,id != 31799) %>%
  filter(.,id != 41229) %>%
  filter(.,id != 41230) %>%
  filter(.,id != 31800) %>%
  filter(.,id != 38605) %>%
  filter(.,id != 38609) %>%
  filter(.,id != 38610) %>%
  filter(.,id != 38611) %>%
  filter(.,id != 38614) %>%
  filter(.,id != 42238) %>%
  filter(.,id != 42239) %>%
  filter(.,id != 41260) %>%
  filter(.,id != 38831) %>%
  filter(.,id != 31683) %>%
  filter(.,id != 41175) %>%
  filter(.,id != 41176) %>%
  filter(.,id != 41178) %>%
  filter(.,id != 31689) %>%
  filter(.,id != 42216) %>%
  filter(.,id != 31690) %>%
  filter(.,id != 31691) %>%
  filter(.,id != 31694) %>%
  filter(.,id != 41269) %>%
  filter(.,id != 31696) %>%
  filter(.,id != 31697) %>%
  filter(.,id != 41179) %>%
  filter(.,id != 31813) %>%
  filter(.,id != 31806) %>%
  filter(.,id != 31805) %>%
  filter(.,id != 28354) %>%
  filter(.,id != 41237) %>%
  filter(.,id != 38618) %>%
  filter(.,id != 42244) %>%
  filter(.,id != 41236) %>%
  filter(.,id != 41234) %>%
  filter(.,id != 42243) %>%
  filter(.,id != 42242) %>%
  filter(.,id != 28343) %>%
  filter(.,id != 42241) %>%
  filter(.,id != 41232) %>%
  filter(.,id != 42240) %>%
  filter(.,id != 41231) %>%
  filter(.,id != 38619) %>%
  filter(.,id != 42245) %>%
  filter(.,id != 42246) %>%
  filter(.,id != 28355) %>%
  filter(.,id != 31814) %>%
  filter(.,id != 41239) %>%
  filter(.,id != 41240) %>%
  filter(.,id != 42256) %>%
  filter(.,id != 38620) %>%
  filter(.,id != 41242) %>%
  filter(.,id != 38616) %>%
  filter(.,id != 42232) %>%
  filter(.,id != 31778) %>%
  filter(.,id != 38592) %>%
  filter(.,id != 38591) %>%
  filter(.,id != 38589) %>%
  filter(.,id != 31772) %>%
  filter(.,id != 31771) %>%
  filter(.,id != 31843) %>%
  filter(.,id != 31770) %>%
  filter(.,id != 41210) %>%
  filter(.,id != 41209) %>%
  filter(.,id != 41209) %>%
  filter(.,id != 42231) %>%
  filter(.,id != 42230) %>%
  filter(.,id != 42229) %>%
  filter(.,id != 42228) %>%
  filter(.,id != 31763) %>%
  filter(.,id != 41206) %>%
  filter(.,id != 41205) %>%
  filter(.,id != 31762) %>%
  filter(.,id != 31787) %>%
  filter(.,id != 38586) %>%
  filter(.,id != 38585) %>%
  filter(.,id != 31779) %>%
  filter(.,id != 42233) %>%
  filter(.,id != 42234) %>%
  filter(.,id != 41212) %>%
  filter(.,id != 41213) %>%
  filter(.,id != 31780) %>%
  filter(.,id != 31786) %>%
  filter(.,id != 38593) %>%
  filter(.,id != 42257) %>%
  filter(.,id != 38597) %>%
  filter(.,id != 38598) %>%
  filter(.,id != 41189) %>%
  filter(.,id != 41190) %>%
  filter(.,id != 41188) %>%
  filter(.,id != 42221) %>%
  filter(.,id != 42220) %>%
  filter(.,id != 41186) %>%
  filter(.,id != 31721) %>%
  filter(.,id != 31720) %>%
  filter(.,id != 31712) %>%
  filter(.,id != 41184) %>%
  filter(.,id != 42219) %>%
  filter(.,id != 42218) %>%
  filter(.,id != 42217) %>%
  filter(.,id != 31711) %>%
  filter(.,id != 31709) %>%
  filter(.,id != 38576) %>%
  filter(.,id != 38575) %>%
  filter(.,id != 41192) %>%
  filter(.,id != 41194) %>%
  filter(.,id != 31722) %>%
  filter(.,id != 31723) %>%
  filter(.,id != 41195) %>%
  filter(.,id != 31729) %>%
  filter(.,id != 38578) %>%
  filter(.,id != 31730) %>%
  filter(.,id != 31731) %>%
  filter(.,id != 31736) %>%
  filter(.,id != 28341) %>%
  filter(.,id != 42222) %>%
  filter(.,id != 42223) %>%
  filter(.,id != 38580) %>%
  filter(.,id != 31737) %>%
  filter(.,id != 38640) %>%
  filter(.,id != 31708) %>%
  filter(.,id != 31706) %>%
  filter(.,id != 31707) %>%
  filter(.,id != 41183) %>%
  filter(.,id != 41181) %>%
  filter(.,id != 41198) %>%
  filter(.,id != 41196) %>%
  filter(.,id != 42224) %>%
  filter(.,id != 31743) %>%
  filter(.,id != 38572) %>%
  filter(.,id != 41167) %>%
  filter(.,id != 31671) %>%
  filter(.,id != 41168) %>%
  filter(.,id != 41170) %>%
  filter(.,id != 41171) %>%
  filter(.,id != 31672) %>%
  filter(.,id != 31674) %>%
  filter(.,id != 28334) %>%
  filter(.,id != 31675) %>%
  filter(.,id != 19643) %>%
  filter(.,id != 41208) %>%
  filter(.,id != 1) %>%
  filter(.,id != 3) %>%
  filter(.,id != 2) 

# adding Length of building back in using the common attribute of physicalid
st_length = subset(CSCL, select = c(physicalid, shape_leng, full_stree))
st_length <- st_length %>% st_drop_geometry()
total <- merge(total, st_length ,by=c("physicalid"))

# removing unnecessary datasets
rm(sf_cent)
rm(joined)
rm(join)
rm(st_length)