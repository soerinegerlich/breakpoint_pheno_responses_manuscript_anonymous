#Figure 2 with restrictions on standard errors - only significant breakpoints

library(tidyverse)
library(readxl)

#Read datasets

df_summary_all_onset <-
  read_xlsx("Data/Summary_tables/Final/df_summary_onset_snow_temp_final_new.xlsx")
df_summary_all_temp_onset <-
  read_xlsx("Data/Summary_tables/Final/df_summary_onset_temp_snow_final_new.xlsx")
df_summary_all_peak <-
  read_xlsx("Data/Summary_tables/Final/df_summary_peak_snow_temp_final_new.xlsx")
df_summary_all_temp_peak <-
  read_xlsx("Data/Summary_tables/Final/df_summary_peak_temp_snow_final_new.xlsx")
df_summary_all_end <-
  read_xlsx("Data/Summary_tables/Final/df_summary_end_snow_temp_final_new.xlsx")
df_summary_all_temp_end <-
  read_xlsx("Data/Summary_tables/Final/df_summary_end_temp_snow_final_new.xlsx")

#Onset snow

df_summary_all_onset$Significance_level <- ifelse(df_summary_all_onset$Pvalue<0.06, "True", "False")

df_significant_onset_snow <- subset(df_summary_all_onset, Significance_level == "True")

df_significant_onset_snow$SpeciesID <-
  factor(
    df_significant_onset_snow$SpeciesID,
    # Relevel group factor
    levels = c(
      "Acari",
      "Ichneumonidae",
      "Chironomidae",
      "Muscidae",
      "Phoridae",
      "Sciaridae",
      "Linyphiidae",
      "Lycosidae"
    )
  )

Onset_snow <- ggplot(df_significant_onset_snow) +
  geom_point(
    mapping = aes(
      x = Slope1,
      y = Slopediff,
      group = interaction(Plot, SpeciesID),
      shape = Habitat,
      color = SpeciesID,  # Controls the outline
      fill = SpeciesID       # Controls the fill
    ),
    color = "white",  # Set border color to white
    size = 4,
    alpha = 0.8,
    stroke = 0.1
  ) +
  ylab("") +
  xlab("") +
  scale_shape_manual(values = c(22, 21, 24, 23, 25)) +
  scale_fill_manual(
    values = c(
      "darkseagreen3",
      "brown4",
      "darkorange",
      "darkgoldenrod1",
      "yellow",
      "gold",
      "dodgerblue",
      "blue"
    )
  ) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  xlim(-3, 3) +
  ylim(-3, 5) +
  theme_bw() +
  labs(shape = "Habitat", colour = "Taxa", fill = "Habitat") +
  theme(
    axis.text.x = element_text(
      face = "bold",
      size = 8,
      color = "black"
    ),
    axis.text.y = element_text(
      face = "bold",
      size = 8,
      color = "black"
    ),
    axis.title.x = element_text(
      face = "bold",
      size = 10,
      color = "black",
      vjust = 0.5
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 10,
      color = "black"
    )
  )



#peak snow

df_summary_all_peak$Significance_level <- ifelse(df_summary_all_peak$Pvalue<0.06, "True", "False")

df_significant_peak_snow <- subset(df_summary_all_peak, Significance_level == "True")

df_significant_peak_snow$SpeciesID <-
  factor(
    df_significant_peak_snow$SpeciesID,
    # Relevel group factor
    levels = c(
      "Acari",
      "Collembola",
      "Ichneumonidae",
      "Chironomidae",
      "Phoridae",
      "Sciaridae",
      "Linyphiidae",
      "Lycosidae"
    )
  )

Peak_snow <- ggplot(df_significant_peak_snow) +
  geom_point(
    mapping = aes(
      x = Slope1,
      y = Slopediff,
      group = interaction(Plot, SpeciesID),
      shape = Habitat,
      color = SpeciesID,  # Controls the outline
      fill = SpeciesID       # Controls the fill
    ),
    color = "white",  # Set border color to white
    size = 4,
    alpha = 0.8,
    stroke = 0.1
  ) +
  ylab("") +
  xlab("") +
  scale_shape_manual(values = c(22, 21)) +
  scale_fill_manual(
    values = c(
      "darkseagreen3",
      "darkseagreen4",
      "brown4",
      "darkorange",
      "yellow",
      "gold",
      "dodgerblue",
      "blue"
    )
  ) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  #geom_errorbar(aes(ymin=Slopediff-SEslopediff, ymax=Slopediff+SEslopediff), width=.3,
  #position=position_dodge(0.05), size = 1, alpha = 0.5)+
  #geom_errorbar(aes(xmin=Slope1-SEslope, xmax=Slope1+SEslope), width=.3,
  #position=position_dodge(0.05), size = 1, alpha = 0.5)+
  xlim(-3, 3) +
  ylim(-3, 5) +
  theme_bw() +
  labs(shape = "Habitat", colour = "Taxa") +
  theme(
    axis.text.x = element_text(
      face = "bold",
      size = 8,
      color = "black"
    ),
    axis.text.y = element_text(
      face = "bold",
      size = 8,
      color = "black"
    ),
    axis.title.x = element_text(
      face = "bold",
      size = 10,
      color = "black",
      vjust = 0.5
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 10,
      color = "black"
    )
  )


#end snow

df_summary_all_end$Significance_level <- ifelse(df_summary_all_end$Pvalue<0.06, "True", "False")

df_significant_end_snow <- subset(df_summary_all_end, Significance_level == "True")

df_significant_end_snow$SpeciesID <-
  factor(
    df_significant_end_snow$SpeciesID,
    # Relevel group factor
    levels = c(
      "Acari",
      "Collembola",
      "Coccoidea",
      "Ichneumonidae",
      "Chironomidae",
      "Culicidae",
      "Muscidae",
      "Phoridae",
      "Lycosidae"
    )
  )

End_snow <- ggplot(df_significant_end_snow) +
  geom_point(
    mapping = aes(
      x = Slope1,
      y = Slopediff,
      group = interaction(Plot, SpeciesID),
      shape = Habitat,
      color = SpeciesID,  # Controls the outline
      fill = SpeciesID       # Controls the fill
    ),
    color = "white",  # Set border color to white
    size = 4,
    alpha = 0.8,
    stroke = 0.1
  ) +
  ylab("") +
  xlab("") +
  scale_shape_manual(values = c(22, 21, 24, 23, 25)) +
  scale_fill_manual(
    values = c(
      "darkseagreen3",
      "darkseagreen4",
      "chartreuse",
      "brown4",
      "darkorange",
      "#FC4E07",
      "darkgoldenrod1",
      "yellow",
      "blue"
    )
  ) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  #geom_errorbar(aes(ymin=Slopediff-SEslopediff, ymax=Slopediff+SEslopediff), width=.3,
  #position=position_dodge(0.05), size = 1, alpha = 0.5)+
  #geom_errorbar(aes(xmin=Slope1-SEslope, xmax=Slope1+SEslope), width=.3,
  #position=position_dodge(0.05), size = 1, alpha = 0.5)+
  xlim(-3, 3) +
  ylim(-3, 5) +
  theme_bw() +
  labs(shape = "Habitat", colour = "Taxa") +
  theme(
    axis.text.x = element_text(
      face = "bold",
      size = 8,
      color = "black"
    ),
    axis.text.y = element_text(
      face = "bold",
      size = 8,
      color = "black"
    ),
    axis.title.x = element_text(
      face = "bold",
      size = 10,
      color = "black",
      vjust = 0.5
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 10,
      color = "black"
    )
  )


#Onset temp

df_summary_all_temp_onset$Significance_level <- ifelse(df_summary_all_temp_onset$Pvalue<0.06, "True", "False")

df_significant_onset_temp <- subset(df_summary_all_temp_onset, Significance_level == "True")

df_significant_onset_temp$SpeciesID <-
  factor(
    df_significant_onset_temp$SpeciesID,
    # Relevel group factor
    levels = c(
      "Acari",
      "Chalcidoidea",
      "Chironomidae",
      "Lycosidae"
    )
  )

Onset_temp <- ggplot(df_significant_onset_temp) +
  geom_point(
    mapping = aes(
      x = Slope1,
      y = Slopediff,
      group = interaction(Plot, SpeciesID),
      shape = Habitat,
      color = SpeciesID,  # Controls the outline
      fill = SpeciesID       # Controls the fill
    ),
    color = "white",  # Set border color to white
    size = 4,
    alpha = 0.8,
    stroke = 0.1
  ) +
  ylab("") +
  xlab("") +
  scale_shape_manual(values = c(22, 21, 24, 23, 25)) +
  scale_fill_manual(
    values = c(
      "darkseagreen3",
      "brown3",
      "darkorange",
      "blue"
    )
  )  +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  #geom_errorbar(aes(ymin=Slopediff-SEslopediff, ymax=Slopediff+SEslopediff), width=.3,
  #position=position_dodge(0.05), size = 1, alpha = 0.5)+
  #geom_errorbar(aes(xmin=Slope1-SEslope, xmax=Slope1+SEslope), width=.3,
  #position=position_dodge(0.05), size = 1, alpha = 0.5)+
  xlim(-50, 50) +
  ylim(-60, 60) +
  theme_bw() +
  labs(shape = "Habitat", colour = "Taxa") +
  theme(
    axis.text.x = element_text(
      face = "bold",
      size = 8,
      color = "black"
    ),
    axis.text.y = element_text(
      face = "bold",
      size = 8,
      color = "black"
    ),
    axis.title.x = element_text(
      face = "bold",
      size = 10,
      color = "black",
      vjust = 0.5
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 10,
      color = "black"
    )
  )


#peak temp

df_summary_all_temp_peak$Significance_level <- ifelse(df_summary_all_temp_peak$Pvalue<0.06, "True", "False")

df_significant_peak_temp <- subset(df_summary_all_temp_peak, Significance_level == "True")

df_significant_peak_temp$SpeciesID <-
  factor(
    df_significant_peak_temp$SpeciesID,
    # Relevel group factor
    levels = c(
      "Acari",
      "Collembola",
      "Linyphiidae"
    )
  )

Peak_temp <- ggplot(df_significant_peak_temp) +
  geom_point(
    mapping = aes(
      x = Slope1,
      y = Slopediff,
      group = interaction(Plot, SpeciesID),
      shape = Habitat,
      color = SpeciesID,  # Controls the outline
      fill = SpeciesID       # Controls the fill
    ),
    color = "white",  # Set border color to white
    size = 4,
    alpha = 0.8,
    stroke = 0.1
  ) +
  ylab("") +
  xlab("") +
  scale_shape_manual(values = c(22, 21, 24, 23, 25)) +
  scale_fill_manual(
    values = c(
      "darkseagreen3",
      "darkseagreen4",
      "dodgerblue"
    )
  )  +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  #geom_errorbar(aes(ymin=Slopediff-SEslopediff, ymax=Slopediff+SEslopediff), width=.3,
  #position=position_dodge(0.05), size = 1, alpha = 0.5)+
  #geom_errorbar(aes(xmin=Slope1-SEslope, xmax=Slope1+SEslope), width=.3,
  #position=position_dodge(0.05), size = 1, alpha = 0.5)+
  xlim(-50, 50) +
  ylim(-60, 60) +
  theme_bw() +
  labs(shape = "Habitat", colour = "Taxa") +
  theme(
    axis.text.x = element_text(
      face = "bold",
      size = 8,
      color = "black"
    ),
    axis.text.y = element_text(
      face = "bold",
      size = 8,
      color = "black"
    ),
    axis.title.x = element_text(
      face = "bold",
      size = 10,
      color = "black",
      vjust = 0.5
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 10,
      color = "black"
    )
  )


#end temp

df_summary_all_temp_end$Significance_level <- ifelse(df_summary_all_temp_end$Pvalue<0.06, "True", "False")

df_significant_end_temp <- subset(df_summary_all_temp_end, Significance_level == "True")

df_significant_end_temp$SpeciesID <-
  factor(
    df_significant_end_temp$SpeciesID,
    # Relevel group factor
    levels = c(
      "Chalcidoidea",
      "Ichneumonidae",
      "Chironomidae",
      "Linyphiidae"
    )
  )

End_temp <- ggplot(df_significant_end_temp) +
  geom_point(
    mapping = aes(
      Slope1,
      Slopediff,
      group = interaction(Plot, SpeciesID),
      shape = Habitat,
      color = SpeciesID
    ),
    size = 2,
    alpha = 0.8,
    stroke = 1
  ) +
  ylab("") +
  xlab("") +
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  scale_color_manual(
    values = c(
      "brown3",
      "brown4",
      "darkorange",
      "dodgerblue"
    )
  )  +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  #geom_errorbar(aes(ymin=Slopediff-SEslopediff, ymax=Slopediff+SEslopediff), width=.3,
  #position=position_dodge(0.05), size = 1, alpha = 0.5)+
  #geom_errorbar(aes(xmin=Slope1-SEslope, xmax=Slope1+SEslope), width=.3,
  #position=position_dodge(0.05), size = 1, alpha = 0.5)+
  xlim(-50, 50) +
  ylim(-60, 60) +
  theme_bw() +
  labs(shape = "Habitat", colour = "Taxa") +
  theme(
    axis.text.x = element_text(
      face = "bold",
      size = 8,
      color = "black"
    ),
    axis.text.y = element_text(
      face = "bold",
      size = 8,
      color = "black"
    ),
    axis.title.x = element_text(
      face = "bold",
      size = 10,
      color = "black",
      vjust = 0.5
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 10,
      color = "black"
    )
  )



require(ggpubr)

ggarrange(
  Onset_temp,
  Onset_snow,
  Peak_temp,
  Peak_snow,
  End_temp,
  End_snow,
  labels = c(
    "a. Onset - Temperature",
    "b. Onset - Snowmelt",
    "c. Peak - Temperature",
    "d. Peak - Snowmelt",
    "e. End - Temperature",
    "f. End - Snowmelt"
  ),
  hjust = -0.3,
  vjust = -0.5,
  ncol = 2,
  nrow = 3,
  legend = "none",
  font.label = list(color = "black", size = 12)
) +
  theme(plot.margin = margin(2, 2, 2, 2, "cm"))


