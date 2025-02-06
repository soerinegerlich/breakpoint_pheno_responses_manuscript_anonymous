library(tidyverse)
library(readxl)
library(gridExtra)


df_summary_snow_onset <-
  read_xlsx("Data/Summary_tables/Final/df_summary_onset_snow_final.xlsx")
df_summary_temp_onset <-
  read_xlsx("Data/Summary_tables/Final/df_summary_onset_temp_final.xlsx")

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
    linewidth = 0.3,
    position = position_dodge(width = .9)
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
    axis.text.x.bottom = element_text(size = 10, colour = "black"),
    axis.title.x = element_text(size = 10, colour = "black", face = "bold"),
    panel.spacing = unit(0, "lines"),
    strip.text.y = element_blank(),
    legend.position = "none",
    plot.margin = unit(c(5,20,5,0), "mm")
    #plot.margin = marginFig4
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
    linewidth = 0.3,
    height = 0.25,
    position = position_dodge(width = .9)
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
    #plot.margin = marginFig3
  )


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
    linewidth = 0.3,
    height = 0.25,
    position = position_dodge(width = .9)
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
    axis.text.x.bottom = element_text(size = 10, colour = "black"),
    axis.title.x = element_text(size = 10, colour = "black", face = "bold"),
    panel.spacing = unit(0, "lines"),
    strip.text.y = element_blank(),
    legend.position = "none",
    #plot.margin = unit(c(0.5,0,1,0.1), "mm")
    plot.margin = unit(c(5,20,5,0), "mm")
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
    linewidth = 0.3,
    height = 0.25,
    position = position_dodge(width = .9)
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


ggarrange(Plot4, Plot3, Plot2, Plot1, ncol = 2,
          nrow = 2, labels = c(
            "a. Onset - Temperature",
            "",
            "b. Onset - Snowmelt"
          ), hjust = c(-0.7,0,-0.8), 
          font.label = list(color = "black", size = 10))+
  theme(plot.margin = unit(c(1,1,1,1), "cm"))

