#Figure 2 with restrictions on standard errors - only significant breakpoints

library(tidyverse)
library(readxl)

#Read datasets

df_summary_all_onset <-
  read_xlsx("Data/Summary_tables/Final/df_summary_onset_snow_final_new.xlsx")
df_summary_all_temp_onset <-
  read_xlsx("Data/Summary_tables/Final/df_summary_onset_temp_final_new.xlsx")
df_summary_all_peak <-
  read_xlsx("Data/Summary_tables/Final/df_summary_peak_snow_final_new.xlsx")
df_summary_all_temp_peak <-
  read_xlsx("Data/Summary_tables/Final/df_summary_peak_temp_final_new.xlsx")
df_summary_all_end <-
  read_xlsx("Data/Summary_tables/Final/df_summary_end_snow_final_new.xlsx")
df_summary_all_temp_end <-
  read_xlsx("Data/Summary_tables/Final/df_summary_end_temp_final_new.xlsx")

#Onset snow

df_summary_all_onset$Significance_level <- ifelse(df_summary_all_onset$Pvalue<0.06, "True", "False")

df_significant_onset_snow <- subset(df_summary_all_onset, Significance_level == "True")

df_significant_onset_snow$SpeciesID <-
  factor(
    df_significant_onset_snow$SpeciesID,
    # Relevel group factor
    levels = c(
      "Acari",
      "Collembola",
      "Ichneumonidae",
      "Chironomidae",
      "Muscidae",
      "Phoridae",
      "Linyphiidae",
      "Lycosidae"
    )
  )

Onset_snow <- ggplot(df_significant_onset_snow) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point(
    mapping = aes(
      Slope1,
      Slopediff,
      group = interaction(Plot, SpeciesID),
      shape = Habitat,
      color = SpeciesID
    ),
    size = 4,
    alpha = 0.8,
    stroke = 0.6
  ) +
  ylab("") +
  xlab("") +
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  scale_color_manual(
    values = c(
      "darkseagreen3",
      "darkseagreen4",
      "brown4",
      "darkorange",
      "darkgoldenrod1",
      "yellow",
      "dodgerblue",
      "blue"
    )
  )  +
  #geom_errorbar(aes(ymin=Slopediff-SEslopediff, ymax=Slopediff+SEslopediff), width=.3,
  #position=position_dodge(0.05), size = 1, alpha = 0.5)+
  #geom_errorbar(aes(xmin=Slope1-SEslope, xmax=Slope1+SEslope), width=.3,
  #position=position_dodge(0.05), size = 1, alpha = 0.5)+
  xlim(-6, 6) +
  ylim(-6, 6) +
  theme_bw() +
  labs(shape = "Habitat", colour = "Taxa") +
  theme(
    axis.text.x = element_text(
      face = "bold",
      size = 10,
      color = "black"
    ),
    axis.text.y = element_text(
      face = "bold",
      size = 10,
      color = "black"
    ),
    axis.title.x = element_text(
      face = "bold",
      size = 12,
      color = "black",
      vjust = 0.5
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 12,
      color = "black"
    )
    #plot.margin = margin(3, 6, 1, 2, "cm")
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
      "Nymphalidae",
      "Phoridae",
      "Lycosidae",
      "Linyphiidae"
    )
  )

Peak_snow <- ggplot(df_significant_peak_snow) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point(
    mapping = aes(
      Slope1,
      Slopediff,
      group = interaction(Plot, SpeciesID),
      shape = Habitat,
      color = SpeciesID
    ),
    size = 4,
    alpha = 0.8,
    stroke = 0.6
  ) +
  ylab("") +
  xlab("") +
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  scale_color_manual(
    values = c(
      "darkseagreen3",
      "darkseagreen4",
      "brown4",
      "darkorange3",
      "yellow",
      "blue",
      "dodgerblue"
    )
  )  +
  #geom_errorbar(aes(ymin=Slopediff-SEslopediff, ymax=Slopediff+SEslopediff), width=.3,
  #position=position_dodge(0.05), size = 1, alpha = 0.5)+
  #geom_errorbar(aes(xmin=Slope1-SEslope, xmax=Slope1+SEslope), width=.3,
  #position=position_dodge(0.05), size = 1, alpha = 0.5)+
  xlim(-6, 6) +
  ylim(-6, 6) +
  theme_bw() +
  labs(shape = "Habitat", colour = "Taxa") +
  theme(
    axis.text.x = element_text(
      face = "bold",
      size = 10,
      color = "black"
    ),
    axis.text.y = element_text(
      face = "bold",
      size = 10,
      color = "black"
    ),
    axis.title.x = element_text(
      face = "bold",
      size = 12,
      color = "black",
      vjust = 0.5
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 12,
      color = "black"
    )
    #plot.margin = margin(0, 6, 4, 2, "cm")
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
      "Ichneumonidae",
      "Chironomidae",
      "Muscidae",
      "Sciaridae",
      "Lycosidae",
      "Linyphiidae"
    )
  )

End_snow <- ggplot(df_significant_end_snow) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point(
    mapping = aes(
      Slope1,
      Slopediff,
      group = interaction(Plot, SpeciesID),
      shape = Habitat,
      color = SpeciesID
    ),
    size = 4,
    alpha = 0.8,
    stroke = 0.6
  ) +
  ylab("") +
  xlab("") +
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  scale_color_manual(
    values = c(
      "darkseagreen3",
      "darkseagreen4",
      "brown4",
      "darkorange",
      "darkgoldenrod1",
      "gold",
      "blue",
      "dodgerblue"
    )
  )  +
  #geom_errorbar(aes(ymin=Slopediff-SEslopediff, ymax=Slopediff+SEslopediff), width=.3,
  #position=position_dodge(0.05), size = 1, alpha = 0.5)+
  #geom_errorbar(aes(xmin=Slope1-SEslope, xmax=Slope1+SEslope), width=.3,
  #position=position_dodge(0.05), size = 1, alpha = 0.5)+
  xlim(-6, 6) +
  ylim(-6, 6) +
  theme_bw() +
  labs(shape = "Habitat", colour = "Taxa") +
  theme(
    axis.text.x = element_text(
      face = "bold",
      size = 10,
      color = "black"
    ),
    axis.text.y = element_text(
      face = "bold",
      size = 10,
      color = "black"
    ),
    axis.title.x = element_text(
      face = "bold",
      size = 12,
      color = "black",
      vjust = 0.5
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 12,
      color = "black"
    )
    #plot.margin = margin(-3, 6, 7, 2, "cm")
  )


#Onset temp

df_summary_all_temp_onset$Significance_level <- ifelse(df_summary_all_temp_onset$Pvalue<0.06, "True", "False")

df_significant_onset_temp <- subset(df_summary_all_temp_onset, Significance_level == "True")

df_significant_onset_temp$SpeciesID <-
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  factor(
    df_significant_onset_temp$SpeciesID,
    # Relevel group factor
    levels = c(
      "Acari",
      "Coccoidea",
      "Chironomidae",
      "Nymphalidae",
      "Lycosidae"
    )
  )

Onset_temp <- ggplot(df_significant_onset_temp) +
  geom_point(
    mapping = aes(
      Slope1,
      Slopediff,
      group = interaction(Plot, SpeciesID),
      shape = Habitat,
      color = SpeciesID
    ),
    size = 4,
    alpha = 0.8,
    stroke = 1
  ) +
  ylab("") +
  xlab("") +
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  scale_color_manual(
    values = c(
      "darkseagreen3",
      "chartreuse",
      "darkorange",
      "darkorange3",
      "blue"
    )
  )  +
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
      size = 10,
      color = "black"
    ),
    axis.text.y = element_text(
      face = "bold",
      size = 10,
      color = "black"
    ),
    axis.title.x = element_text(
      face = "bold",
      size = 12,
      color = "black",
      vjust = 0.5
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 12,
      color = "black"
    )
    #plot.margin = margin(3, 2, 1, 6, "cm")
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
      "Chironomidae",
      "Linyphiidae"
    )
  )

Peak_temp <- ggplot(df_significant_peak_temp) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point(
    mapping = aes(
      Slope1,
      Slopediff,
      group = interaction(Plot, SpeciesID),
      shape = Habitat,
      color = SpeciesID
    ),
    size = 4,
    alpha = 0.8,
    stroke = 0.6
  ) +
  ylab("") +
  xlab("") +
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  scale_color_manual(
    values = c(
      "darkseagreen3",
      "darkseagreen4",
      "darkorange",
      "dodgerblue"
    )
  )  +
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
      size = 10,
      color = "black"
    ),
    axis.text.y = element_text(
      face = "bold",
      size = 10,
      color = "black"
    ),
    axis.title.x = element_text(
      face = "bold",
      size = 12,
      color = "black",
      vjust = 0.5
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 12,
      color = "black"
    )
    #plot.margin = margin(0, 2, 4, 6, "cm")
  )


#end temp

df_summary_all_temp_end$Significance_level <- ifelse(df_summary_all_temp_end$Pvalue<0.06, "True", "False")

df_significant_end_temp <- subset(df_summary_all_temp_end, Significance_level == "True")

df_significant_end_temp$SpeciesID <-
  factor(
    df_significant_end_temp$SpeciesID,
    # Relevel group factor
    levels = c(
      "Aphidoidea",
      "Chironomidae",
      "Thomisidae"
    )
  )

End_temp <- ggplot(df_significant_end_temp) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point(
    mapping = aes(
      Slope1,
      Slopediff,
      group = interaction(Plot, SpeciesID),
      shape = Habitat,
      color = SpeciesID
    ),
    size = 4,
    alpha = 0.8,
    stroke = 0.6
  ) +
  ylab("") +
  xlab("") +
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  scale_color_manual(
    values = c(
      "chartreuse3",
      "darkorange",
      "dodgerblue4"
    )
  )  +
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
      size = 10,
      color = "black"
    ),
    axis.text.y = element_text(
      face = "bold",
      size = 10,
      color = "black"
    ),
    axis.title.x = element_text(
      face = "bold",
      size = 12,
      color = "black",
      vjust = 0.5
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 12,
      color = "black"
    )
    #plot.margin = margin(-3, 2, 7, 6, "cm")
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


