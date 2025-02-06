################################################################################
##################### Forest plot - Figure 3 ###################################
################################################################################

df_summary_snow_peak <-
  read_xlsx("Data/Summary_tables/Final/df_summary_peak_snow_temp_final_new.xlsx")

df_summary_temp_peak <-
  read_xlsx("Data/Summary_tables/Final/df_summary_peak_temp_snow_final_new.xlsx")



# Define a separate y-position for the meta-analysis

#Slope diff

meta_analysis_snow <- data.frame(
  SpeciesID = "Meta-analysis",  # Label for meta-analysis
  Slopediff_snow = 0.54,
  CI_max_slopediff_snow = 0.21,          # Standard error of slope difference
  CI_min_slopediff_snow = 0.87
)

meta_analysis_snow$SpeciesID <- "Meta-analysis"

df_summary_snow_peak$SpeciesID <- factor(
  df_summary_snow_peak$SpeciesID,
  levels = c(
    "Acari",
    "Collembola",
    "Aphidoidea",
    "Coccoidea",
    "Chalcidoidea",
    "Ichneumonidae",
    "Chironomidae",
    "Culicidae",
    "Muscidae",
    "Nymphalidae",
    "Phoridae",
    "Sciaridae",
    "Linyphiidae",
    "Lycosidae",
    "Thomisidae",
    "Meta-analysis"  # Add meta-analysis at the end
  )
)

# Ensure Meta-analysis is re-leveled appropriately
meta_analysis_snow$SpeciesID <- factor(
  meta_analysis_snow$SpeciesID,
  levels = levels(df_summary_snow_peak$SpeciesID)  # Match levels
)


# Create the plot
Plot2 <- ggplot(df_summary_snow_peak, aes(y = SpeciesID, x = Slopediff)) +
  geom_vline(
    xintercept = 0,
    color = "grey",
    linetype = "dashed",
    cex = 1,
    alpha = 0.5
  ) +
  geom_errorbarh(
    aes(
      xmin = Slopediff - SEslopediff,
      xmax = Slopediff + SEslopediff,
      group = Habitat
    ),
    height = 0,  # Removes vertical lines
    position = position_dodge(width = .9),
    linewidth = 0.4
  ) +
  geom_point(
    aes(
      shape = Habitat,
      color = "black",
      fill = SpeciesID
    ),
    size = 2,
    position = position_dodge(width = .9)
  ) +
  geom_crossbar(
    data = meta_analysis_snow,
    aes(
      x = Slopediff_snow,  # Specify the x aesthetic explicitly
      y = SpeciesID,
      xmin = CI_min_slopediff_snow,
      xmax = CI_max_slopediff_snow,
    ),
    fill = "red",
    color = "red",
    alpha = 0.6
  ) +
  geom_point(
    data = meta_analysis_snow,
    aes(
      x = Slopediff_snow,
      y = SpeciesID
    ),
    size = 3,
    stroke = 1,
    shape = 9,  # Diamond shape
    color = "black"
  ) +
  facet_grid(SpeciesID ~ ., scales = "free", space = "free") +
  scale_shape_manual(values = c(22, 21, 24, 23, 25)) +
  scale_fill_manual(
    values = c(
      "darkseagreen3",
      "darkseagreen4",
      "chartreuse",
      "chartreuse3",
      "brown3",
      "brown4",
      "darkorange",
      "#FC4E07",
      "darkgoldenrod1",
      "darkorange3",
      "yellow",
      "gold",
      "dodgerblue",
      "blue",
      "dodgerblue4",
      "red"  # Color for meta-analysis
    )
  ) +
  scale_color_manual(values = c("black", "red")) +
  coord_cartesian(xlim = c(-4, 4)) +
  xlab(" ") +
  ylab(" ") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x.bottom = element_text(size = 10, colour = "black"),
    axis.title.x = element_text(size = 10, colour = "black", face = "bold"),
    panel.spacing = unit(0, "lines"),
    strip.text.y = element_blank(),
    legend.position = "none",
    plot.margin = unit(c(5,20,5,0), "mm")
  )


#Slope 1

meta_analysis_snow <- data.frame(
  SpeciesID = "Meta-analysis",  # Label for meta-analysis
  Slope1_snow <- -0.05,
  CI_min_slope1_snow <- -0.34,           # Standard error of slope difference
  CI_max_slope1_snow <- 0.24
)

meta_analysis_snow$SpeciesID <- "Meta-analysis"

df_summary_snow_peak$SpeciesID <- factor(
  df_summary_snow_peak$SpeciesID,
  levels = c(
    "Acari",
    "Collembola",
    "Aphidoidea",
    "Coccoidea",
    "Chalcidoidea",
    "Ichneumonidae",
    "Chironomidae",
    "Culicidae",
    "Muscidae",
    "Nymphalidae",
    "Phoridae",
    "Sciaridae",
    "Linyphiidae",
    "Lycosidae",
    "Thomisidae",
    "Meta-analysis"  # Add meta-analysis at the end
  )
)

# Ensure Meta-analysis is re-leveled appropriately
meta_analysis_snow$SpeciesID <- factor(
  meta_analysis_snow$SpeciesID,
  levels = levels(df_summary_snow_peak$SpeciesID)  # Match levels
)


Plot1 <- ggplot(df_summary_snow_peak, aes(y = SpeciesID, x = Slope1)) +
  geom_vline(
    xintercept = 0,
    color = "grey",
    linetype = "dashed",
    cex = 1,
    alpha = 0.5
  ) +
  geom_errorbarh(
    aes(
      xmin = Slope1 - SEslope,
      xmax = Slope1 + SEslope,
      group = Habitat
    ),
    height = 0,  # Removes vertical lines
    position = position_dodge(width = .9),
    linewidth = 0.4
  ) +
  geom_point(
    aes(
      shape = Habitat,
      color = "black",
      fill = SpeciesID
    ),
    size = 2,
    position = position_dodge(width = .9)
  ) +
  # Add the meta-analysis point and error bar
  geom_crossbar(
    data = meta_analysis_snow,
    aes(
      x = Slope1_snow,  # Specify the x aesthetic explicitly
      y = SpeciesID,
      xmin = CI_min_slope1_snow,
      xmax = CI_max_slope1_snow,
    ),
    fill = "red",
    color = "red",
    alpha = 0.6
  ) +
  geom_point(
    data = meta_analysis_snow,
    aes(
      x = Slope1_snow,
      y = SpeciesID
    ),
    size = 3,
    stroke = 1,
    shape = 9,
    color = "black"
  ) +
  facet_grid(SpeciesID ~ ., scales = "free", space = "free") +
  scale_shape_manual(values = c(22, 21, 24, 23, 25)) +
  scale_fill_manual(
    values = c(
      "darkseagreen3",
      "darkseagreen4",
      "chartreuse",
      "chartreuse3",
      "brown3",
      "brown4",
      "darkorange",
      "#FC4E07",
      "darkgoldenrod1",
      "darkorange3",
      "yellow",
      "gold",
      "dodgerblue",
      "blue",
      "dodgerblue4"
    )
  ) +
  scale_color_manual(values = c("black")) +
  #scale_y_continuous(name = "", breaks=1:14, labels = df_summary_snow$SpeciesIDf, trans = "reverse") +
  coord_cartesian(xlim=c(-4, 4))+
  xlab(" ") +
  ylab(" ") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text.y = element_text(size = 8, colour = "black", face = "bold"),
    axis.text.x.bottom = element_text(size = 10, colour = "black"),
    axis.title.x = element_text(size = 10, colour = "black", face = "bold"),
    #axis.ticks.y = element_blank(),
    panel.spacing = unit(0, "lines"),
    strip.text.y = element_blank(),
    legend.position = "none",
    plot.margin = unit(c(5,0,5,0), "mm")
  )


#### Temperature

#Slope diff

meta_analysis <- data.frame(
  SpeciesID = "Meta-analysis",  # Label for meta-analysis
  Slopediff <- -1.94,
  CI_min_slopediff <- -5.02,          
  CI_max_slopediff <- 1.14          
)
meta_analysis$SpeciesID <- "Meta-analysis"

df_summary_temp_peak$SpeciesID <- factor(
  df_summary_temp_peak$SpeciesID,
  levels = c(
    "Acari",
    "Collembola",
    "Aphidoidea",
    "Coccoidea",
    "Chalcidoidea",
    "Ichneumonidae",
    "Chironomidae",
    "Culicidae",
    "Muscidae",
    "Nymphalidae",
    "Phoridae",
    "Sciaridae",
    "Linyphiidae",
    "Lycosidae",
    "Thomisidae",
    "Meta-analysis"  # Add meta-analysis at the end
  )
)

# Ensure Meta-analysis is re-leveled appropriately
meta_analysis$SpeciesID <- factor(
  meta_analysis$SpeciesID,
  levels = levels(df_summary_temp_peak$SpeciesID)  # Match levels
)


# Create the plot
Plot4 <- ggplot(df_summary_temp_peak, aes(y = SpeciesID, x = Slopediff)) +
  geom_vline(
    xintercept = 0,
    color = "grey",
    linetype = "dashed",
    cex = 1,
    alpha = 0.5
  ) +
  geom_errorbarh(
    aes(
      xmin = Slopediff - SEslopediff,
      xmax = Slopediff + SEslopediff,
      group = Habitat
    ),
    height = 0.00,
    position = position_dodge(width = .9),
    linewidth = 0.4
  ) +
  geom_point(
    aes(
      shape = Habitat,
      color = "black",
      fill = SpeciesID
    ),
    size = 2,
    position = position_dodge(width = .9)
  ) +
  geom_crossbar(
    data = meta_analysis,
    aes(
      x = Slopediff,  # Specify the x aesthetic explicitly
      y = SpeciesID,
      xmin = CI_min_slopediff,
      xmax = CI_max_slopediff,
    ),
    fill = "red",
    color = "red",
    alpha = 0.6
  ) +
  geom_point(
    data = meta_analysis,
    aes(
      x = Slopediff,
      y = SpeciesID
    ),
    size = 3,
    stroke = 1,
    shape = 9,  # Diamond shape
    color = "black"
  ) +
  facet_grid(SpeciesID ~ ., scales = "free", space = "free") +
  scale_shape_manual(values = c(22, 21, 24, 23, 25)) +
  scale_fill_manual(
    values = c(
      "darkseagreen3",
      "darkseagreen4",
      "chartreuse",
      "chartreuse3",
      "brown3",
      "brown4",
      "darkorange",
      "#FC4E07",
      "darkgoldenrod1",
      "darkorange3",
      "yellow",
      "gold",
      "dodgerblue",
      "blue",
      "dodgerblue4",
      "red"  # Color for meta-analysis
    )
  ) +
  scale_color_manual(values = c("black", "red")) +
  coord_cartesian(xlim=c(-50, 50))+
  xlab(" ") +
  ylab(" ") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x.bottom = element_text(size = 10, colour = "black"),
    axis.title.x = element_text(size = 10, colour = "black", face = "bold"),
    panel.spacing = unit(0, "lines"),
    strip.text.y = element_blank(),
    legend.position = "none",
    #plot.margin = unit(c(0.5,0,1,0.1), "mm")
    plot.margin = unit(c(5,20,5,0), "mm")
  )

#Slope 1

meta_analysis <- data.frame(
  SpeciesID = "Meta-analysis",  # Label for meta-analysis
  Slope1 <- -1.23,
  CI_min_slope1 <- -2.56,          
  CI_max_slope1 <- 0.10         
)

meta_analysis$SpeciesID <- "Meta-analysis"

df_summary_temp_peak$SpeciesID <- factor(
  df_summary_temp_peak$SpeciesID,
  levels = c(
    "Acari",
    "Collembola",
    "Aphidoidea",
    "Coccoidea",
    "Chalcidoidea",
    "Ichneumonidae",
    "Chironomidae",
    "Culicidae",
    "Muscidae",
    "Nymphalidae",
    "Phoridae",
    "Sciaridae",
    "Linyphiidae",
    "Lycosidae",
    "Thomisidae",
    "Meta-analysis"  # Add meta-analysis at the end
  )
)

# Ensure Meta-analysis is re-leveled appropriately
meta_analysis$SpeciesID <- factor(
  meta_analysis$SpeciesID,
  levels = levels(df_summary_temp_peak$SpeciesID)  # Match levels
)


Plot3 <- ggplot(df_summary_temp_peak, aes(y = SpeciesID, x = Slope1)) +
  geom_vline(
    xintercept = 0,
    color = "grey",
    linetype = "dashed",
    cex = 1,
    alpha = 0.5
  ) +
  geom_errorbarh(
    aes(
      xmin = Slope1 - SEslope,
      xmax = Slope1 + SEslope,
      group = Habitat
    ),
    height = 0.00,
    position = position_dodge(width = .9),
    linewidth = 0.4
  ) +
  geom_crossbar(
    data = meta_analysis,
    aes(
      x = Slope1,  # Specify the x aesthetic explicitly
      y = SpeciesID,
      xmin = CI_min_slope1,
      xmax = CI_max_slope1,
    ),
    fill = "red",
    color = "red",
    alpha = 0.6
  ) +
  geom_point(
    data = meta_analysis,
    aes(
      x = Slope1,
      y = SpeciesID
    ),
    size = 3,
    stroke = 1,
    shape = 9,
    color = "black"
  ) +
  geom_point(
    aes(
      shape = Habitat,
      color = "black",
      fill = SpeciesID
    ),
    size = 2,
    position = position_dodge(width = .9)
  ) +
  facet_grid(SpeciesID ~ ., scales = "free", space = "free") +
  scale_shape_manual(values = c(22, 21, 24, 23, 25)) +
  scale_fill_manual(
    values = c(
      "darkseagreen3",
      "darkseagreen4",
      "chartreuse",
      "chartreuse3",
      "brown3",
      "brown4",
      "darkorange",
      "#FC4E07",
      "darkgoldenrod1",
      "darkorange3",
      "yellow",
      "gold",
      "dodgerblue",
      "blue",
      "dodgerblue4"
    )
  ) +
  scale_color_manual(values = c("black")) +
  #scale_y_continuous(name = "", breaks=1:14, labels = df_summary_snow$SpeciesIDf, trans = "reverse") +
  coord_cartesian(xlim=c(-50, 50))+
  xlab(" ") +
  ylab(" ") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    #axis.text.y = element_blank(),
    axis.text.y = element_text(size = 8, colour = "black", face = "bold"),
    axis.text.x.bottom = element_text(size = 10, colour = "black"),
    axis.title.x = element_text(size = 10, colour = "black", face = "bold"),
    panel.spacing = unit(0, "lines"),
    strip.text.y = element_blank(),
    legend.position = "none",
    plot.margin = unit(c(5,0,5,0), "mm")
  )

require(ggpubr)

ggarrange(
  Plot1,
  Plot2,
  Plot3,
  Plot4,
  ncol = 2,
  nrow = 2,
  legend = "none"
) +
  theme(plot.margin = unit(c(1,1,1,1), "cm"))
#theme(plot.margin = margin(0.5, 0.2, 0.2, 0.2, "cm"))


