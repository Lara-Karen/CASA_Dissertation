####Scoreboard####

### Measure 1: betweenness
centrality_nogeo <- subset(centrality, select = c(physicalid, betweenness)) %>% st_drop_geometry()

score <- merge(geo, centrality_nogeo ,by=c("physicalid"), all = TRUE)

score$betweenness[is.na(score$betweenness)] <- 0

### Measure 2: Density

density_nogeo <- subset(density, select = c(physicalid, density))

score <- merge(score, density_nogeo ,by=c("physicalid"), all = TRUE)

score$density[is.na(score$density)] <- 0

### Measure 4: Dimension

dimension_nogeo <- subset(dimension, select = c(physicalid, dimension_score_inverse))  %>% st_drop_geometry()

score <- merge(score, dimension_nogeo ,by=c("physicalid"), all = TRUE)

score$dimension_score_inverse[is.na(score$dimension_score_inverse)] <- 0

### Measure 4: Age

age_nogeo <- subset(age, select = c(physicalid, age_score))   %>% st_drop_geometry()

score <- merge(score, age_nogeo ,by=c("physicalid"), all = TRUE) 

score$age_score[is.na(score$age_score)] <- 0


### Measure 5: Alignment

alignment_nogeo <- subset(alignment, select = c(physicalid, alignment_score)) %>% st_drop_geometry()

score <- merge(score, alignment_nogeo ,by=c("physicalid"), all = TRUE)

score$alignment_score[is.na(score$alignment_score)] <- 0

### Measure 6: Height

height_nogeo <- subset(height, select = c(physicalid, height_score)) %>% st_drop_geometry()

score <- merge(score, height_nogeo ,by=c("physicalid"), all = TRUE)

score$height_score[is.na(score$height_score)] <- 0

### Measure 7: Use

use_nogeo <- subset(use, select = c(physicalid, mixed_score)) %>% st_drop_geometry()

score <- merge(score, use_nogeo ,by=c("physicalid"), all = TRUE)

score$mixed_score[is.na(score$mixed_score)] <- 0

### Adding the scores

score <- score %>%
  mutate(total_score = score$betweenness + score$density + score$age_score + score$mixed_score + score$alignment_score + score$dimension_score_inverse + score$height_score)

score <- score[!duplicated(score), ]

### PLOTS ####
tmap_mode("plot")
cut <- c(0, 0.2, 0.4, 0.6, 0.8, 1)

### 1. Betweenness ####
score$bet_thickness <- (case_when (score$betweenness >= 0.9 ~ 1000,
                                   score$betweenness >= 0.7 ~ 400,
                                   score$betweenness >= 0.5 ~ 100,
                                   score$betweenness >= 0.3 ~ 50,
                                   score$betweenness >= 0.1 ~ 10,
                                   score$betweenness >= 0 ~ 0))

tm_shape(Borough_MN) +
  tm_borders(col = NA, lwd = 0.2, lty = "solid", alpha = 1)+
  tm_legend(show = TRUE) +
  tm_shape(CSCL) +
  tm_lines(col = "grey", alpha = 1, scale = 0.1, size = 100)+
  tm_legend(show = FALSE)+
  tm_shape(score) +
  tm_lines(col = "betweenness", scale = 2, palette = "Blues", lwd = "bet_thickness", legend.lwd.show = FALSE, title.col="Street Score") +
  tm_legend(show = TRUE, fontfamily= "Arial") +
  tm_layout("Street Betweenness Centrality",
            title.size= 1.1,
            legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 0.1) +
  tm_scale_bar(position=c("right", "bottom"), color.dark="black", lwd = 0.2)


### 2. Density ####

colors_density <- c("#fef0d9", "#fdd49e", "#fdbb84", "#fc8d59", "#ef6548", "#d7301f", "#990000")

score$den_thickness <- (case_when (score$density >= 0.9 ~ 1000,
                                   score$density >= 0.7 ~ 700,
                                   score$density >= 0.5 ~ 500,
                                   score$density >= 0.3 ~ 100,
                                   score$density >= 0.1 ~ 50,
                                   score$density >= 0 ~ 0))
tm_shape(Borough_MN) +
  tm_borders(col = NA, lwd = 0.2, lty = "solid", alpha = 1)+
  tm_legend(show = TRUE) +
  tm_shape(CSCL) +
  tm_lines(col = "grey", alpha = 1, scale = 0.1, size = 100)+
  tm_legend(show = FALSE)+
  tm_shape(score) +
  tm_lines(col = "density", scale = 2, palette = colors_density, lwd = "den_thickness", legend.lwd.show = FALSE, title.col="Street Score") +
  tm_legend(show = TRUE, fontfamily= "Arial") +
  tm_layout("Street Plot Density",
            title.size= 1.1,
            legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 0.1) +
  tm_scale_bar(position=c("right", "bottom"), color.dark="black", lwd = 0.2)

### 3. Size of Street Block####

dimension_colors <- c("#ffffcc", "#7fcdbb", "#1d91c0", "#225ea8", "#0c2c84")

score$dimension_thickness <- (case_when (score$dimension_score_inverse >= 0.9 ~ 1000,
                                   score$dimension_score_inverse >= 0.7 ~ 1000,
                                   score$dimension_score_inverse >= 0.5 ~ 900,
                                   score$dimension_score_inverse >= 0.3 ~ 400,
                                   score$dimension_score_inverse >= 0.1 ~ 20,
                                   score$dimension_score_inverse >= 0 ~ 0))
tm_shape(Borough_MN) +
  tm_borders(col = NA, lwd = 0.2, lty = "solid", alpha = 1)+
  tm_legend(show = TRUE) +
  tm_shape(CSCL) +
  tm_lines(col = "grey", alpha = 1, scale = 0.1, size = 100)+
  tm_legend(show = FALSE)+
  tm_shape(score) +
  tm_lines(col = "dimension_score_inverse", scale = 1, palette = dimension_colors, lwd = "dimension_thickness", legend.lwd.show = FALSE, title.col="Street Score") +
  tm_legend(show = TRUE, fontfamily= "Arial") +
  tm_layout("Size of Street Block",
            title.size= 1.1,
            legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 0.1) +
  tm_scale_bar(position=c("right", "bottom"), color.dark="black", lwd = 0.2)

### 4. Age ####

colors_age <- c("#bcbddc", "#9e9ac8", "#807dba","#6a51a3", "#4a1486")

score$age_thickness <- (case_when (score$age_score >= 0.9 ~ 700,
                                   score$age_score >= 0.7 ~ 600,
                                   score$age_score >= 0.5 ~ 500,
                                   score$age_score >= 0.3 ~ 300,
                                   score$age_score >= 0.1 ~ 200,
                                   score$age_score >= 0 ~ 200))
tm_shape(Borough_MN) +
  tm_borders(col = NA, lwd = 0.2, lty = "solid", alpha = 1)+
  tm_legend(show = TRUE) +
  tm_shape(CSCL) +
  tm_lines(col = "grey", alpha = 1, scale = 0.1, size = 100)+
  tm_legend(show = FALSE)+
  tm_shape(score) +
  tm_lines(col = "age_score", scale = 1, palette = colors_age, lwd = "age_thickness", legend.lwd.show = FALSE, title.col="Street Score") +
  tm_legend(show = TRUE, fontfamily= "Arial") +
  tm_legend(show = TRUE, fontfamily= "Arial") +
  tm_layout("Street Building Age",
            title.size= 1.1,
            legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 0.1) +
  tm_scale_bar(position=c("right", "bottom"), color.dark="black", lwd = 0.2)

### 5. Alignment ####

colors_alignment <- c("#feebe2", "#fa9fb5", "#f768a1","#dd3497", "#7a0177")

score$alignment_thickness <- (case_when (score$alignment_score >= 0.9 ~ 800,
                                   score$alignment_score >= 0.7 ~ 700,
                                   score$alignment_score >= 0.5 ~ 300,
                                   score$alignment_score >= 0.3 ~ 150,
                                   score$alignment_score >= 0.1 ~ 100,
                                   score$alignment_score >= 0 ~ 20))
tmap_mode("plot")
tm_shape(Borough_MN) +
  tm_borders(col = NA, lwd = 0.2, lty = "solid", alpha = 1)+
  tm_legend(show = TRUE) +
  tm_shape(CSCL) +
  tm_lines(col = "grey", alpha = 1, scale = 0.1, size = 100)+
  tm_legend(show = FALSE)+
  tm_shape(score) +
  tm_lines(col = "alignment_score", scale = 1.2, palette = colors_alignment, lwd = "alignment_thickness", legend.lwd.show = FALSE, title.col="Street Score") +
  tm_legend(show = TRUE, fontfamily= "Arial") +
  tm_layout("Street Building Alignment",
            title.size= 1.1,
            legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 0.1) +
  tm_scale_bar(position=c("right", "bottom"), color.dark="black", lwd = 0.2)

### 6. Height ####
colors_height <- c("#d9f0a3", "#addd8e", "#78c679", "#41ab5d", "#238443", "#005a32")

score$height_thickness <- (case_when (score$height_score >= 0.9 ~ 800,
                                      score$height_score >= 0.7 ~ 500,
                                      score$height_score >= 0.5 ~ 200,
                                      score$height_score >= 0.3 ~ 50,
                                      score$height_score >= 0.1 ~ 10,
                                      score$height_score >= 0 ~ 0))

tm_shape(Borough_MN) +
  tm_borders(col = NA, lwd = 0.2, lty = "solid", alpha = 1)+
  tm_legend(show = TRUE) +
  tm_shape(CSCL) +
  tm_lines(col = "grey", alpha = 1, scale = 0.1, size = 100)+
  tm_legend(show = FALSE)+
  tm_shape(score) +
  tm_lines(col = "height_score", scale = 2, palette = colors_height, lwd = "height_thickness", legend.lwd.show = FALSE, title.col="Street Score") +
  tm_legend(show = TRUE, fontfamily= "Arial") +
  tm_layout("Ratio of Building Height to Street Width",
            title.size= 1.1,
            legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 0.1) +
  tm_scale_bar(position=c("right", "bottom"), color.dark="black", lwd = 0.2)

### 7. Use ####

colors_use <- c("#ffffb2", "#feb24c", "#fd8d3c", "#fc4e2a","#e31a1c", "#b10026")
score$use_thickness <- (case_when (score$mixed_score >= 0.9 ~ 1000,
                                      score$mixed_score >= 0.7 ~ 700,
                                      score$mixed_score >= 0.5 ~ 500,
                                      score$mixed_score >= 0.3 ~ 200,
                                      score$mixed_score >= 0.1 ~ 100,
                                      score$mixed_score >= 0 ~ 0))

tm_shape(Borough_MN) +
  tm_borders(col = NA, lwd = 0.2, lty = "solid", alpha = 1)+
  tm_legend(show = TRUE) +
  tm_shape(CSCL) +
  tm_lines(col = "grey", alpha = 1, scale = 0.1, size = 100)+
  tm_legend(show = FALSE)+
  tm_shape(score) +
  tm_lines(col = "mixed_score", scale = 2, palette = colors_use, lwd = "use_thickness", legend.lwd.show = FALSE, title.col="Street Score") +
  tm_legend(show = TRUE, fontfamily= "Arial") +
  tm_layout("Street Use",
            title.size= 1.1,
            legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 0.1) +
  tm_scale_bar(position=c("right", "bottom"), color.dark="black", lwd = 0.2)

### Mapping Total Score ####

score$total_score[is.na(score$total_score)] <- 0

score$total_thickness <- (case_when (score$total_score >= 5 ~ 800,
                                     score$total_score >= 4 ~ 700,
                                     score$total_score >= 3 ~ 400,
                                     score$total_score >= 2 ~ 300,
                                     score$total_score >= 1 ~ 300,
                                     score$total_score >= 0 ~ 300))

tmap_mode("plot")
tm_shape(Borough_MN) +
  tm_borders(col = NA, lwd = 0.2, lty = "solid", alpha = 1)+
  tm_legend(show = TRUE) +
  tm_shape(CSCL) +
  tm_lines(col = "grey", alpha = 1, scale = 0.1, size = 100)+
  tm_legend(show = FALSE)+
  tm_shape(score) +
  tm_lines(col = "total_score", scale = 1.5, size = 100, palette = "-viridis", lwd = "total_thickness", legend.lwd.show = FALSE, contrast = 1, title.col="Street Score") +
  tm_legend(show = TRUE, fontfamily= "Arial") +
  tm_layout("Walking Potential Compound Score",
            title.size= 1.1,
            legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 0.1) +
  tm_scale_bar(position=c("right", "bottom"), color.dark="black", lwd = 0.2)

  
