library(tidyverse)
library(readxl)
library(metafor)
library(gridExtra)


df_summary_snow_onset <-
  read_xlsx("Data/Summary_tables/Final/df_summary_onset_snow_final.xlsx")
df_summary_temp_onset <-
  read_xlsx("Data/Summary_tables/Final/df_summary_onset_temp_final.xlsx")
df_summary_snow_peak <-
  read_xlsx("Data/Summary_tables/Final/df_summary_peak_snow_final.xlsx")
df_summary_temp_peak <-
  read_xlsx("Data/Summary_tables/Final/df_summary_peak_temp_final.xlsx")
df_summary_snow_end <-
  read_xlsx("Data/Summary_tables/Final/df_summary_end_snow_final.xlsx")
df_summary_temp_end <-
  read_xlsx("Data/Summary_tables/Final/df_summary_end_temp_final.xlsx")

df_summary_snow_onset$SpeciesID <-
  factor(
    df_summary_snow_onset$SpeciesID,
    # Relevel group factor
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
      "Thomisidae"
    )
  )

df_summary_temp_onset$SpeciesID <-
  factor(
    df_summary_temp_onset$SpeciesID,
    # Relevel group factor
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
      "Thomisidae"
    )
  )

df_summary_snow_peak$SpeciesID <-
  factor(
    df_summary_snow_peak$SpeciesID,
    # Relevel group factor
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
      "Thomisidae"
    )
  )

df_summary_temp_peak$SpeciesID <-
  factor(
    df_summary_temp_peak$SpeciesID,
    # Relevel group factor
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
      "Thomisidae"
    )
  )

df_summary_snow_end$SpeciesID <-
  factor(
    df_summary_snow_end$SpeciesID,
    # Relevel group factor
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
      "Thomisidae"
    )
  )

df_summary_temp_end$SpeciesID <-
  factor(
    df_summary_temp_end$SpeciesID,
    # Relevel group factor
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
      "Thomisidae"
    )
  )




Plot1 <-
  ggplot(df_summary_snow_onset, aes(y = SpeciesID, x = Slopediff)) +
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
    height = 0.25,
    position = position_dodge(width = .9)
  ) +
  geom_point(
    aes(
      shape = Habitat,
      color = "black",
      fill = SpeciesID
    ),
    size = 6,
    position = position_dodge(width = .9)
  ) +
  facet_grid(SpeciesID ~ ., scales = "free", space = "free") +
  scale_shape_manual(values = c(22, 21, 24, 23)) +
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
  #scale_colour_manual(values=c("black", "black", "black", "black"))+
  #scale_y_continuous(name = "", breaks=1:14, labels = df_summary_snow$SpeciesIDf, trans = "reverse") +
  coord_cartesian(xlim=c(-4, 4))+
  xlab(" ") +
  ylab(" ") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x.bottom = element_blank(),
    axis.title.x = element_blank(),
    panel.spacing = unit(0, "lines"),
    strip.text.y = element_blank(),
    legend.position = "none",
    plot.margin = unit(c(0.5,3,1,0.5), "cm")
  )

Plot2 <- ggplot(df_summary_snow_onset, aes(y = SpeciesID, x = Slope1)) +
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
    height = 0.25,
    position = position_dodge(width = .9)
  ) +
  geom_point(
    aes(
      shape = Habitat,
      color = "black",
      fill = SpeciesID
    ),
    size = 6,
    position = position_dodge(width = .9)
  ) +
  facet_grid(SpeciesID ~ ., scales = "free", space = "free") +
  scale_shape_manual(values = c(22, 21, 24, 23)) +
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
    axis.text.y = element_blank(),
    axis.text.x.bottom = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.spacing = unit(0, "lines"),
    strip.text.y = element_blank(),
    legend.position = "none",
    plot.margin = unit(c(0.5,1.5,1,2.3), "cm")
  )

################################################################################


Plot3 <-
  ggplot(df_summary_temp_onset, aes(y = SpeciesID, x = Slopediff)) +
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
    height = 0.25,
    position = position_dodge(width = .9)
  ) +
  geom_point(
    aes(
      shape = Habitat,
      color = "black",
      fill = SpeciesID
    ),
    size = 6,
    position = position_dodge(width = .9)
  ) +
  facet_grid(SpeciesID ~ ., scales = "free", space = "free") +
  scale_shape_manual(values = c(22, 21, 24, 23)) +
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
  #scale_colour_manual(values=c("black", "black", "black", "black"))+
  #scale_y_continuous(name = "", breaks=1:14, labels = df_summary_snow$SpeciesIDf, trans = "reverse") +
  coord_cartesian(xlim=c(-80, 80))+
  xlab("Slope difference") +
  ylab(" ") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x.bottom = element_blank(),
    axis.title.x = element_blank(),
    panel.spacing = unit(0, "lines"),
    strip.text.y = element_blank(),
    legend.position = "none",
    plot.margin = unit(c(0.6,1,1,2), "cm")
  )


Plot4 <- ggplot(df_summary_temp_onset, aes(y = SpeciesID, x = Slope1)) +
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
    height = 0.25,
    position = position_dodge(width = .9)
  ) +
  geom_point(
    aes(
      shape = Habitat,
      color = "black",
      fill = SpeciesID
    ),
    size = 6,
    position = position_dodge(width = .9)
  ) +
  facet_grid(SpeciesID ~ ., scales = "free", space = "free") +
  scale_shape_manual(values = c(22, 21, 24, 23)) +
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
  coord_cartesian(xlim=c(-80, 80))+
  xlab("") +
  ylab(" ") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text.y = element_text(size = 18, colour = "black"),
    axis.text.x.bottom = element_blank(),
    axis.title.x = element_blank(),
    panel.spacing = unit(0, "lines"),
    strip.text.y = element_blank(),
    legend.position = "none",
    plot.margin = unit(c(0.6,0.1,1,-0.6), "cm")
  )


Plot5 <-
  ggplot(df_summary_snow_peak, aes(y = SpeciesID, x = Slopediff)) +
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
    height = 0.25,
    position = position_dodge(width = .9)
  ) +
  geom_point(
    aes(
      shape = Habitat,
      color = "black",
      fill = SpeciesID
    ),
    size = 6,
    position = position_dodge(width = .9)
  ) +
  facet_grid(SpeciesID ~ ., scales = "free", space = "free") +
  scale_shape_manual(values = c(22, 21, 24, 23)) +
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
  #scale_colour_manual(values=c("black", "black", "black", "black"))+
  #scale_y_continuous(name = "", breaks=1:14, labels = df_summary_snow$SpeciesIDf, trans = "reverse") +
  coord_cartesian(xlim=c(-4, 4))+
  xlab(" ") +
  ylab(" ") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x.bottom = element_blank(),
    axis.title.x = element_blank(),
    panel.spacing = unit(0, "lines"),
    strip.text.y = element_blank(),
    legend.position = "none",
    plot.margin = unit(c(0.5,3,1,0.5), "cm")
  )


Plot6 <- ggplot(df_summary_snow_peak, aes(y = SpeciesID, x = Slope1)) +
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
    height = 0.25,
    position = position_dodge(width = .9)
  ) +
  geom_point(
    aes(
      shape = Habitat,
      color = "black",
      fill = SpeciesID
    ),
    size = 6,
    position = position_dodge(width = .9)
  ) +
  facet_grid(SpeciesID ~ ., scales = "free", space = "free") +
  scale_shape_manual(values = c(22, 21, 24, 23)) +
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
  xlab("") +
  ylab(" ") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text.y = element_blank(),
    axis.text.x.bottom = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    panel.spacing = unit(0, "lines"),
    strip.text.y = element_blank(),
    legend.position = "none",
    plot.margin = unit(c(0.5,1.5,1,2.3), "cm")
  )


Plot7 <-
  ggplot(df_summary_temp_peak, aes(y = SpeciesID, x = Slopediff)) +
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
    height = 0.25,
    position = position_dodge(width = .9)
  ) +
  geom_point(
    aes(
      shape = Habitat,
      color = "black",
      fill = SpeciesID
    ),
    size = 6,
    position = position_dodge(width = .9)
  ) +
  facet_grid(SpeciesID ~ ., scales = "free", space = "free") +
  scale_shape_manual(values = c(22, 21, 24, 23)) +
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
  #scale_colour_manual(values=c("black", "black", "black", "black"))+
  #scale_y_continuous(name = "", breaks=1:14, labels = df_summary_snow$SpeciesIDf, trans = "reverse") +
  coord_cartesian(xlim=c(-80, 80))+
  xlab("Slope difference") +
  ylab(" ") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x.bottom = element_blank(),
    axis.title.x = element_blank(),
    panel.spacing = unit(0, "lines"),
    strip.text.y = element_blank(),
    legend.position = "none",
    plot.margin = unit(c(0.6,1,1,2), "cm")
  )

Plot8 <- ggplot(df_summary_temp_peak, aes(y = SpeciesID, x = Slope1)) +
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
    height = 0.25,
    position = position_dodge(width = .9)
  ) +
  geom_point(
    aes(
      shape = Habitat,
      color = "black",
      fill = SpeciesID
    ),
    size = 6,
    position = position_dodge(width = .9)
  ) +
  facet_grid(SpeciesID ~ ., scales = "free", space = "free") +
  scale_shape_manual(values = c(22, 21, 24, 23)) +
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
  coord_cartesian(xlim=c(-80, 80))+
  xlab("") +
  ylab(" ") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text.y = element_text(size = 18, colour = "black"),
    axis.text.x.bottom = element_blank(),
    axis.title.x = element_blank(),
    panel.spacing = unit(0, "lines"),
    strip.text.y = element_blank(),
    legend.position = "none",
    plot.margin = unit(c(0.6,0.1,1,-0.6), "cm")
  )


Plot9 <-
  ggplot(df_summary_snow_end, aes(y = SpeciesID, x = Slopediff)) +
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
    height = 0.25,
    position = position_dodge(width = .9)
  ) +
  geom_point(
    aes(
      shape = Habitat,
      color = "black",
      fill = SpeciesID
    ),
    size = 6,
    position = position_dodge(width = .9)
  ) +
  facet_grid(SpeciesID ~ ., scales = "free", space = "free") +
  scale_shape_manual(values = c(22, 21, 24, 23)) +
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
  #scale_colour_manual(values=c("black", "black", "black", "black"))+
  #scale_y_continuous(name = "", breaks=1:14, labels = df_summary_snow$SpeciesIDf, trans = "reverse") +
  coord_cartesian(xlim=c(-4, 4))+
  xlab("Slope difference") +
  ylab(" ") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x.bottom = element_text(size = 20, colour = "black"),
    axis.title.x = element_text(
      size = 22,
      colour = "black",
      vjust = -2
    ),
    panel.spacing = unit(0, "lines"),
    strip.text.y = element_blank(),
    legend.position = "none",
    plot.margin = unit(c(0.5, 3, 0.8, 0.5), "cm")
  )


Plot10 <- ggplot(df_summary_snow_end, aes(y = SpeciesID, x = Slope1)) +
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
    height = 0.25,
    position = position_dodge(width = .9)
  ) +
  geom_point(
    aes(
      shape = Habitat,
      color = "black",
      fill = SpeciesID
    ),
    size = 6,
    position = position_dodge(width = .9)
  ) +
  facet_grid(SpeciesID ~ ., scales = "free", space = "free") +
  scale_shape_manual(values = c(22, 21, 24, 23)) +
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
  xlab("Slope 1") + 
  ylab(" ") + 
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x.bottom = element_text(size = 20, colour = "black"),
        axis.title.x = element_text(size = 22, colour = "black", vjust = -2),
        panel.spacing = unit(0, "lines"),
        strip.text.y = element_blank(),
        legend.position="none",
        plot.margin = unit(c(0.5,1.5,0.8,2.3), "cm"))


Plot11 <-
  ggplot(df_summary_temp_end, aes(y = SpeciesID, x = Slopediff)) +
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
    height = 0.25,
    position = position_dodge(width = .9)
  ) +
  geom_point(
    aes(
      shape = Habitat,
      color = "black",
      fill = SpeciesID
    ),
    size = 6,
    position = position_dodge(width = .9)
  ) +
  facet_grid(SpeciesID ~ ., scales = "free", space = "free") +
  scale_shape_manual(values = c(22, 21, 24, 23)) +
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
  #scale_colour_manual(values=c("black", "black", "black", "black"))+
  #scale_y_continuous(name = "", breaks=1:14, labels = df_summary_snow$SpeciesIDf, trans = "reverse") +
  coord_cartesian(xlim=c(-80, 80))+
  xlab("Slope difference") + 
  ylab(" ") + 
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x.bottom = element_text(size = 20, colour = "black"),
        axis.title.x = element_text(size = 22, colour = "black", vjust = -2),
        panel.spacing = unit(0, "lines"),
        strip.text.y = element_blank(),
        legend.position="none",
        plot.margin = unit(c(0.6,1,0.8,2), "cm"))


Plot12 <- ggplot(df_summary_temp_end, aes(y = SpeciesID, x = Slope1)) +
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
    height = 0.25,
    position = position_dodge(width = .9)
  ) +
  geom_point(
    aes(
      shape = Habitat,
      color = "black",
      fill = SpeciesID
    ),
    size = 6,
    position = position_dodge(width = .9)
  ) +
  facet_grid(SpeciesID ~ ., scales = "free", space = "free") +
  scale_shape_manual(values = c(22, 21, 24, 23)) +
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
  coord_cartesian(xlim=c(-80, 80))+
  xlab("Slope 1") + 
  ylab(" ") + 
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(colour = "black"),
        #axis.text.y = element_blank(),
        #axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 18, colour = "black"),
        axis.text.x.bottom = element_text(size = 20, colour = "black"),
        axis.title.x = element_text(size = 22, colour = "black", vjust = -2),
        panel.spacing = unit(0, "lines"),
        strip.text.y = element_blank(),
        legend.position="none",
        plot.margin = unit(c(0.5,0.1,0.8,-0.6), "cm")
  )
        



require(ggpubr)
snow <- ggarrange(
  Plot2,
  Plot1,
  Plot6,
  Plot5,
  Plot10,
  Plot9,
  labels = c(
    "b. Onset - Snowmelt",
    "",
    "d. Peak - Snowmelt",
    "",
    "f. End - Snowmelt",
    ""
  ),
  hjust = -0.2,
  vjust = 0.5,
  ncol = 2,
  nrow = 3,
  legend = "none",
  font.label = list(color = "black", size = 20)
) +
  theme(plot.margin = margin(0.5, 0.2, 0.2, 0.2, "cm"))

temp <- ggarrange(
  Plot4,
  Plot3,
  Plot8,
  Plot7,
  Plot12,
  Plot11,
  labels = c(
    "a. Onset - Temperature",
    "",
    "c. Peak - Temperature",
    "",
    "e. End - Temperature",
    ""
  ),
  hjust = -0.4,
  vjust = 0.5,
  ncol = 2,
  nrow = 3,
  legend = "none",
  font.label = list(color = "black", size = 20)
) +
  theme(plot.margin = margin(0.5, 0.2, 0.2, 0.2, "cm"))


ggarrange(temp,
          #labels = c("a. Onset", "", "b. Peak", "", "c. End", ""),
          ggarrange(snow),
          ncol = 2)
#theme(plot.margin = margin(0.5,0.2,0.2,0.2, "cm"))
