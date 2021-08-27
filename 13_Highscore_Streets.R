# Packages Required
library(viridis)
library(dplyr)
library(tmap)
library(ggplot2)
library(tidyr)

#mapping high scores top 25
score_high <- score %>%
  filter(., total_score > 4.4)

tmap_mode("view")
tm_shape(score_high) +
  tm_lines(col = NA, lwd = 10, scale = 1, size = 100, palette = "-viridis", contrast = 1, title.col="Street Score") +
  tm_legend(show = TRUE, title = "Total Street Score") 


######### Bar Chart for Streets with a High Success Potential for Pedestrian-Oriented Initiatives

score_high_1 <- score_high

score_high <- subset(score_high_1, select = -c(total_score))

score_high <- score_high[!duplicated(score_high), ]

colnames(score_high)[colnames(score_high) == "full_stree"] <- "Street"

tmap_mode("view")
tm_shape(score_high) +
  tm_lines(col = NA, lwd = 10, lty = "solid", alpha = 1)

score_high[score_high$physicalid=="19387", "Street"] <- "Lenox Ave (Between 119th & 120th St)"
score_high[score_high$physicalid=="2726", "Street"] <- "1st Ave (Between 116th & 117th St)"
score_high[score_high$physicalid=="2437", "Street"] <- "Park Ave (Between 102nd & 103rd St)"
score_high[score_high$physicalid=="2135", "Street"] <- "2nd Ave (Between 57th & 58th St)"
score_high[score_high$physicalid=="2128", "Street"] <- "2nd Ave (Between 50th & 51st St)"
score_high[score_high$physicalid=="2223", "Street"] <- "2nd Ave (Between 22nd & 23rd St)"
score_high[score_high$physicalid=="2233", "Street"] <- "2nd Ave (Between 11th & 12th St)"
score_high[score_high$physicalid=="2586", "Street"] <- "1st Ave (Between 10th & 11th St)"
score_high[score_high$physicalid=="4350", "Street"] <- "A Ave (Between 12th & 13th St)"
score_high[score_high$physicalid=="2096", "Street"] <- "Loisaida Ave (Between 7th & 8th St)"
score_high[score_high$physicalid=="159631", "Street"] <- "Bleecker St (Between Bowery & Lafayette St)"
score_high[score_high$physicalid=="191166", "Street"] <- "Mac Dougal St (Between 3rd & Bleecker St)"
score_high[score_high$physicalid=="978", "Street"] <- "6th Ave (Between 3rd & Bleecker St"
score_high[score_high$physicalid=="1078", "Street"] <- "Bleecker St (Between Jones & Cornelia St)"
score_high[score_high$physicalid=="1076", "Street"] <- "Bleecker St (Between Arrow St & Jones St)"
score_high[score_high$physicalid=="700", "Street"] <- "Barrow St (Between Arrow St & Hudson St)"
score_high[score_high$physicalid=="1069", "Street"] <- "Bleecker St (Between 10th & Charles St)"
score_high[score_high$physicalid=="1071", "Street"] <- "Bleecker St (Between Perry St & 11th St)"
score_high[score_high$physicalid=="79942", "Street"] <- "West 11th St (Between Hudson St & Washington St)"
score_high[score_high$physicalid=="1215", "Street"] <- "Greenwich Ave (Between Jane St & 12th St)"
score_high[score_high$physicalid=="720", "Street"] <- "West 10th St (Between 6th St & Greenwich Ave)"
score_high[score_high$physicalid=="1433", "Street"] <- "9th Ave (Between 21st & 22nd St)"

score_high<-score_high[!(score_high$physicalid=="2586"),]

colnames(score_high)[colnames(score_high) == "betweenness"] <- "Accessibility "
colnames(score_high)[colnames(score_high) == "dimension_score_inverse"] <- "Block Dimension"
colnames(score_high)[colnames(score_high) == "age_score"] <- "Building Age"
colnames(score_high)[colnames(score_high) == "alignment_score"] <- "Building Alignment"
colnames(score_high)[colnames(score_high) == "mixed_score"] <- "Use"
colnames(score_high)[colnames(score_high) == "height_score"] <- "Building Height"
colnames(score_high)[colnames(score_high) == "density"] <- "Plot Density"

colors_barplot = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f")

score_high <- subset(score_high, select = -c(physicalid)) %>% st_drop_geometry()

final <- gather(score_high, Measures, Score, -Street)

ggplot(data = final, aes(x = Street, y = Score, fill = Measures)) +
  geom_col(position = position_dodge(), width = 0.8, breaks = waiver()) +
  scale_fill_manual (values = colors_barplot, labels = waiver()) +
  coord_flip() +
  labs(x="Street Names", y="Score of Urban Morphology Measure",
       title="Streets with a High Success Potential for Pedestrian-Oriented Initiatives",
       subtitle="The following streets scored higher than 4.4/7 for the combined morphology measures")+
  theme_ipsum()




