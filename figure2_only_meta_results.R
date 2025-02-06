#Figure 2 with restrictions on standard errors

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

#Create new dataframe with mean values from meta-analysis
Slopedifference_onset_snow <- 0.79
SlopediffSE_onset_snow <- 0.22
Slope1_onset_snow <- 0.01
SlopeSE_onset_snow <- 0.12

df_mean_onset_snow <- as.data.frame(Slopedifference_onset_snow)
df_mean_onset_snow$SlopediffSE <- SlopediffSE_onset_snow
df_mean_onset_snow$Slope1 <- Slope1_onset_snow
df_mean_onset_snow$SlopeSE <- SlopeSE_onset_snow


#Peak snow

#Create new dataframe with mean values from meta-analysis
Slopedifference_peak_snow <- 0.54
SlopediffSE_peak_snow <- 0.17
Slope1_peak_snow <- -0.05
SlopeSE_peak_snow <- 0.15

df_mean_peak_snow <- as.data.frame(Slopedifference_peak_snow)
df_mean_peak_snow$SlopediffSE <- SlopediffSE_peak_snow
df_mean_peak_snow$Slope1 <- Slope1_peak_snow
df_mean_peak_snow$SlopeSE <- SlopeSE_peak_snow


#End snow

#Create new dataframe with mean values
Slopedifference_end_snow <- 0.17
SlopediffSE_end_snow <- 0.21
Slope1_end_snow <- 0.00
SlopeSE_end_snow <- 0.20

df_mean_end_snow <- as.data.frame(Slopedifference_end_snow)
df_mean_end_snow$SlopediffSE <- SlopediffSE_end_snow
df_mean_end_snow$Slope1 <- Slope1_end_snow
df_mean_end_snow$SlopeSE <- SlopeSE_end_snow



snow <- ggplot(df_mean_onset_snow) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  # First error bars for y-values
  xlab("Slope of first linear segment \n(days/shifted snowmelt day)") +
  ylab("Slope difference between linear segments") +
  geom_errorbar(data = df_mean_onset_snow, 
                mapping = aes(
                  x = Slope1,  # Specify x-axis aesthetic
                  ymin = Slopedifference_onset_snow - SlopediffSE, 
                  ymax = Slopedifference_onset_snow + SlopediffSE
                ), 
                width = 0.0,
                color = "#414487FF",
                position = position_dodge(0.05), 
                size = 1, 
                alpha = 0.5) +
  # Second error bars for x-values
  geom_errorbarh(data = df_mean_onset_snow, 
                 mapping = aes(
                   y = Slopedifference_onset_snow,  # Specify y-axis aesthetic
                   xmin = Slope1 - SlopeSE, 
                   xmax = Slope1 + SlopeSE
                 ), 
                 height = 0.0,
                 color = "#414487FF",
                 position = position_dodge(0.05), 
                 size = 1, 
                 alpha = 0.5) +
  xlim(-1.5, 1.5) +
  ylim(-1.5, 1.5) +
  geom_point(
    data = df_mean_onset_snow,
    mapping = aes(Slope1, Slopedifference_onset_snow),
    size = 3,
    fill = "#414487FF",
    color = "#414487FF",
    shape = 21,
    stroke = 1
  ) +
  # Error bars for peak snow
  geom_errorbar(data = df_mean_peak_snow, 
                mapping = aes(
                  x = Slope1, 
                  ymin = Slopedifference_peak_snow - SlopediffSE, 
                  ymax = Slopedifference_peak_snow + SlopediffSE
                ), 
                width = 0.0,
                color = "#22A884FF",
                position = position_dodge(0.05), 
                size = 1, 
                alpha = 0.5) +
  geom_errorbarh(data = df_mean_peak_snow, 
                 mapping = aes(
                   y = Slopedifference_peak_snow, 
                   xmin = Slope1 - SlopeSE, 
                   xmax = Slope1 + SlopeSE
                 ), 
                 height = 0.0,
                 color = "#22A884FF",
                 position = position_dodge(0.05), 
                 size = 1, 
                 alpha = 0.5) +
  geom_point(
    data = df_mean_peak_snow,
    mapping = aes(Slope1, Slopedifference_peak_snow),
    size = 3,
    fill = "#22A884FF",
    color = "#22A884FF",
    shape = 21,
    stroke = 1
  ) +
  # Error bars for end snow
  geom_errorbar(data = df_mean_end_snow, 
                mapping = aes(
                  x = Slope1, 
                  ymin = Slopedifference_end_snow - SlopediffSE, 
                  ymax = Slopedifference_end_snow + SlopediffSE
                ), 
                width = 0.0,
                color = "#7AD151FF",
                position = position_dodge(0.05), 
                size = 1, 
                alpha = 0.5) +
  geom_errorbarh(data = df_mean_end_snow, 
                 mapping = aes(
                   y = Slopedifference_end_snow, 
                   xmin = Slope1 - SlopeSE, 
                   xmax = Slope1 + SlopeSE
                 ), 
                 height = 0.0,
                 color = "#7AD151FF",
                 position = position_dodge(0.05), 
                 size = 1, 
                 alpha = 0.5) +
  geom_point(
    data = df_mean_end_snow,
    mapping = aes(Slope1, Slopedifference_end_snow),
    size = 3,
    shape = 21,
    fill = "#7AD151FF",
    color = "#7AD151FF",
    stroke = 1
  ) +
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
  )



#Onset temp

#Create new dataframe with mean values from meta analysis
Slopedifference_onset_temp <- -2.52
SlopediffSE_onset_temp <- 2.24
Slope1_onset_temp <- 1.67
SlopeSE_onset_temp <- 2.41

df_mean_onset_temp <- as.data.frame(Slopedifference_onset_temp)
df_mean_onset_temp$SlopediffSE <- SlopediffSE_onset_temp
df_mean_onset_temp$Slope1 <- Slope1_onset_temp
df_mean_onset_temp$SlopeSE <- SlopeSE_onset_temp

#Peak temperature
#Create new dataframe with mean values from meta-analysis

Slopedifference_peak_temp <- -1.94
SlopediffSE_peak_temp <- 1.57
Slope1_peak_temp <- -1.23
SlopeSE_peak_temp <- 0.68

df_mean_peak_temp <- as.data.frame(Slopedifference_peak_temp)
df_mean_peak_temp$SlopediffSE <- SlopediffSE_peak_temp
df_mean_peak_temp$Slope1 <- Slope1_peak_temp
df_mean_peak_temp$SlopeSE <- SlopeSE_peak_temp

#End temp

#Create new dataframe with mean values from meta-analysis
Slopedifference_end_temp <- -1.05
SlopediffSE_end_temp <- 1.10
Slope1_end_temp <- -0.89
SlopeSE_end_temp <- 0.50

df_mean_end_temp <- as.data.frame(Slopedifference_end_temp)
df_mean_end_temp$SlopediffSE <- SlopediffSE_end_temp
df_mean_end_temp$Slope1 <- Slope1_end_temp
df_mean_end_temp$SlopeSE <- SlopeSE_end_temp


temp <- ggplot(df_mean_onset_temp) +
  geom_errorbar(data = df_mean_onset_temp, 
                mapping = aes(
                  x = Slope1,  # Specify x-axis aesthetic
                  ymin = Slopedifference_onset_temp - SlopediffSE, 
                  ymax = Slopedifference_onset_temp + SlopediffSE
                ), 
                width = 0.0,
                color = "#414487FF",
                position = position_dodge(0.05), 
                size = 1, 
                alpha = 0.5) +
  # Second error bars for x-values
  geom_errorbar(data = df_mean_onset_temp, 
                mapping = aes(
                  y = Slopedifference_onset_temp,  # Specify y-axis aesthetic
                  xmin = Slope1 - SlopeSE, 
                  xmax = Slope1 + SlopeSE
                ), 
                width = 0.0,
                color = "#414487FF",
                position = position_dodge(0.05), 
                size = 1, 
                alpha = 0.5) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  coord_cartesian(xlim=c(-5, 5), ylim=c(-5, 5))+
  xlab("Slope of first linear segment \n(days/ºC)") +
  ylab("Slope difference between linear segments") +
  geom_point(
    data = df_mean_onset_temp,
    mapping = aes(Slope1, Slopedifference_onset_temp),
    size = 3,
    color = "#414487FF",
    fill = "#414487FF",
    shape = 21,
    stroke = 1
  ) +
  geom_errorbar(data = df_mean_peak_temp, 
                mapping = aes(
                  x = Slope1,  # Specify x-axis aesthetic
                  ymin = Slopedifference_peak_temp - SlopediffSE, 
                  ymax = Slopedifference_peak_temp + SlopediffSE
                ), 
                width = 0.0,
                color = "#22A884FF",
                position = position_dodge(0.05), 
                size = 1, 
                alpha = 0.5) +
  # Second error bars for x-values
  geom_errorbar(data = df_mean_peak_temp, 
                mapping = aes(
                  y = Slopedifference_peak_temp,  # Specify y-axis aesthetic
                  xmin = Slope1 - SlopeSE, 
                  xmax = Slope1 + SlopeSE
                ), 
                width = 0.0,
                color = "#22A884FF",
                position = position_dodge(0.05), 
                size = 1, 
                alpha = 0.5) +
  geom_point(
    data = df_mean_peak_temp,
    mapping = aes(Slope1, Slopedifference_peak_temp),
    size = 3,
    color = "#22A884FF",
    fill = "#22A884FF",
    shape = 21,
    stroke = 1
  ) +
  geom_errorbar(data = df_mean_end_temp, 
                mapping = aes(
                  x = Slope1,  # Specify x-axis aesthetic
                  ymin = Slopedifference_end_temp - SlopediffSE, 
                  ymax = Slopedifference_end_temp + SlopediffSE
                ), 
                width = 0.0,
                color = "#7AD151FF",
                position = position_dodge(0.05), 
                size = 1, 
                alpha = 0.5) +
  # Second error bars for x-values
  geom_errorbar(data = df_mean_end_temp, 
                mapping = aes(
                  y = Slopedifference_end_temp,  # Specify y-axis aesthetic
                  xmin = Slope1 - SlopeSE, 
                  xmax = Slope1 + SlopeSE
                ), 
                width = 0.0,
                color = "#7AD151FF",
                position = position_dodge(0.05), 
                size = 1, 
                alpha = 0.5) +
  geom_point(
    data = df_mean_end_temp,
    mapping = aes(Slope1, Slopedifference_end_temp),
    size = 3,
    color = "#7AD151FF",
    fill = "#7AD151FF",
    shape = 21,
    stroke = 1
  ) +
  theme_bw() +
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
    #plot.margin = margin(-7.5, 2, 5.5, 0, "cm")
  )



################################################################################
########################## single predictors####################################
################################################################################

#Onset snow

#Create new dataframe with mean values from meta-analysis
Slopedifference_onset_snow <- 0.83
SlopediffSE_onset_snow <- 0.24
Slope1_onset_snow <- 0.12
SlopeSE_onset_snow <- 0.09

df_mean_onset_snow <- as.data.frame(Slopedifference_onset_snow)
df_mean_onset_snow$SlopediffSE <- SlopediffSE_onset_snow
df_mean_onset_snow$Slope1 <- Slope1_onset_snow
df_mean_onset_snow$SlopeSE <- SlopeSE_onset_snow


#Peak snow

#Create new dataframe with mean values from meta-analysis
Slopedifference_peak_snow <- 0.69
SlopediffSE_peak_snow <- 0.28
Slope1_peak_snow <- -0.02
SlopeSE_peak_snow <- 0.15

df_mean_peak_snow <- as.data.frame(Slopedifference_peak_snow)
df_mean_peak_snow$SlopediffSE <- SlopediffSE_peak_snow
df_mean_peak_snow$Slope1 <- Slope1_peak_snow
df_mean_peak_snow$SlopeSE <- SlopeSE_peak_snow


#End snow

#Create new dataframe with mean values
Slopedifference_end_snow <- 0.33
SlopediffSE_end_snow <- 0.21
Slope1_end_snow <- 0.02
SlopeSE_end_snow <- 0.15

df_mean_end_snow <- as.data.frame(Slopedifference_end_snow)
df_mean_end_snow$SlopediffSE <- SlopediffSE_end_snow
df_mean_end_snow$Slope1 <- Slope1_end_snow
df_mean_end_snow$SlopeSE <- SlopeSE_end_snow


snow_single <- ggplot() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  xlim(-1.5, 1.5) +
  ylim(-1.5, 1.5) +
  xlab("Slope of first linear segment \n(days/shifted snowmelt day)") +
  ylab("Slope difference between linear segments") +
  # First error bars for y-values (onset snow)
  geom_errorbar(data = df_mean_onset_snow, 
                mapping = aes(
                  x = Slope1,  # Specify x-axis aesthetic for error bars
                  ymin = Slopedifference_onset_snow - SlopediffSE_onset_snow, 
                  ymax = Slopedifference_onset_snow + SlopediffSE_onset_snow
                ), 
                width = 0.0,
                color = "#414487FF",
                size = 1, 
                alpha = 0.5) +
  
  # Second error bars for x-values (onset snow)
  geom_errorbarh(data = df_mean_onset_snow, 
                 mapping = aes(
                   y = Slopedifference_onset_snow,  # Specify y-axis aesthetic
                   xmin = Slope1 - SlopeSE_onset_snow, 
                   xmax = Slope1 + SlopeSE_onset_snow
                 ), 
                 height = 0.0,
                 color = "#414487FF",
                 size = 1, 
                 alpha = 0.5) +
  
  # Add point for onset snow
  geom_point(
    data = df_mean_onset_snow,
    mapping = aes(Slope1, Slopedifference_onset_snow),
    size = 3,
    fill = "#414487FF",
    color = "#414487FF",
    shape = 21,
    stroke = 1
  ) +
  
  # Error bars for peak snow
  geom_errorbar(data = df_mean_peak_snow, 
                mapping = aes(
                  x = Slope1,  # Specify x-axis aesthetic for peak snow
                  ymin = Slopedifference_peak_snow - SlopediffSE_peak_snow, 
                  ymax = Slopedifference_peak_snow + SlopediffSE_peak_snow
                ), 
                width = 0.0,
                color = "#22A884FF",
                size = 1, 
                alpha = 0.5) +
  
  # Second error bars for x-values (peak snow)
  geom_errorbarh(data = df_mean_peak_snow, 
                 mapping = aes(
                   y = Slopedifference_peak_snow,  # Specify y-axis aesthetic
                   xmin = Slope1 - SlopeSE_peak_snow, 
                   xmax = Slope1 + SlopeSE_peak_snow
                 ), 
                 height = 0.0,
                 color = "#22A884FF",
                 size = 1, 
                 alpha = 0.5) +
  
  # Add point for peak snow
  geom_point(
    data = df_mean_peak_snow,
    mapping = aes(Slope1, Slopedifference_peak_snow),
    size = 3,
    fill = "#22A884FF",
    color = "#22A884FF",
    shape = 21,
    stroke = 1
  ) +
  
  # Error bars for end snow
  geom_errorbar(data = df_mean_end_snow, 
                mapping = aes(
                  x = Slope1,  # Specify x-axis aesthetic for end snow
                  ymin = Slopedifference_end_snow - SlopediffSE_end_snow, 
                  ymax = Slopedifference_end_snow + SlopediffSE_end_snow
                ), 
                width = 0.0,
                color = "#7AD151FF",
                size = 1, 
                alpha = 0.5) +
  
  # Second error bars for x-values (end snow)
  geom_errorbarh(data = df_mean_end_snow, 
                 mapping = aes(
                   y = Slopedifference_end_snow,  # Specify y-axis aesthetic
                   xmin = Slope1 - SlopeSE_end_snow, 
                   xmax = Slope1 + SlopeSE_end_snow
                 ), 
                 height = 0.0,
                 color = "#7AD151FF",
                 size = 1, 
                 alpha = 0.5) +
  
  # Add point for end snow
  geom_point(
    data = df_mean_end_snow,
    mapping = aes(Slope1, Slopedifference_end_snow),
    size = 3,
    shape = 21,
    fill = "#7AD151FF",
    color = "#7AD151FF",
    stroke = 1
  ) +
  
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
    #plot.margin = margin(-7.5, 4, 5.5, -2, "cm")
  )



#Onset temp

#Create new dataframe with mean values from meta analysis
Slopedifference_onset_temp <- 1.47
SlopediffSE_onset_temp <- 2.38
Slope1_onset_temp <- -1.96
SlopeSE_onset_temp <- 1.90

df_mean_onset_temp <- as.data.frame(Slopedifference_onset_temp)
df_mean_onset_temp$SlopediffSE <- SlopediffSE_onset_temp
df_mean_onset_temp$Slope1 <- Slope1_onset_temp
df_mean_onset_temp$SlopeSE <- SlopeSE_onset_temp

#Peak temperature
#Create new dataframe with mean values from meta-analysis

Slopedifference_peak_temp <- 4.20
SlopediffSE_peak_temp <- 2.09
Slope1_peak_temp <- -4.95
SlopeSE_peak_temp <- 0.92

df_mean_peak_temp <- as.data.frame(Slopedifference_peak_temp)
df_mean_peak_temp$SlopediffSE <- SlopediffSE_peak_temp
df_mean_peak_temp$Slope1 <- Slope1_peak_temp
df_mean_peak_temp$SlopeSE <- SlopeSE_peak_temp

#End temp

#Create new dataframe with mean values from meta-analysis
Slopedifference_end_temp <- 1.93
SlopediffSE_end_temp <- 1.17
Slope1_end_temp <- -2.56
SlopeSE_end_temp <- 0.50

df_mean_end_temp <- as.data.frame(Slopedifference_end_temp)
df_mean_end_temp$SlopediffSE <- SlopediffSE_end_temp
df_mean_end_temp$Slope1 <- Slope1_end_temp
df_mean_end_temp$SlopeSE <- SlopeSE_end_temp



temp_single <- ggplot(df_mean_onset_temp) +
  geom_errorbar(data = df_mean_onset_temp, 
                mapping = aes(
                  x = Slope1,  # Specify x-axis aesthetic
                  ymin = Slopedifference_onset_temp - SlopediffSE, 
                  ymax = Slopedifference_onset_temp + SlopediffSE
                ), 
                width = 0.0,
                color = "#414487FF",
                position = position_dodge(0.05), 
                size = 1, 
                alpha = 0.5) +
  # Second error bars for x-values
  geom_errorbar(data = df_mean_onset_temp, 
                mapping = aes(
                  y = Slopedifference_onset_temp,  # Specify y-axis aesthetic
                  xmin = Slope1 - SlopeSE, 
                  xmax = Slope1 + SlopeSE
                ), 
                width = 0.0,
                color = "#414487FF",
                position = position_dodge(0.05), 
                size = 1, 
                alpha = 0.5) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  #xlim(-8, 8) +
  #ylim(-8, 8) +
  coord_cartesian(xlim=c(-8, 8), ylim=c(-8,8))+
  xlab("Slope of first linear segment \n(days/ºC)") +
  ylab("Slope difference between linear segments") +
  geom_point(
    data = df_mean_onset_temp,
    mapping = aes(Slope1, Slopedifference_onset_temp),
    size = 3,
    color = "#414487FF",
    fill = "#414487FF",
    shape = 21,
    stroke = 1
  ) +
  geom_errorbar(data = df_mean_peak_temp, 
                mapping = aes(
                  x = Slope1,  # Specify x-axis aesthetic
                  ymin = Slopedifference_peak_temp - SlopediffSE, 
                  ymax = Slopedifference_peak_temp + SlopediffSE
                ), 
                width = 0.0,
                color = "#22A884FF",
                position = position_dodge(0.05), 
                size = 1, 
                alpha = 0.5) +
  # Second error bars for x-values
  geom_errorbar(data = df_mean_peak_temp, 
                mapping = aes(
                  y = Slopedifference_peak_temp,  # Specify y-axis aesthetic
                  xmin = Slope1 - SlopeSE, 
                  xmax = Slope1 + SlopeSE
                ), 
                width = 0.0,
                color = "#22A884FF",
                position = position_dodge(0.05), 
                size = 1, 
                alpha = 0.5) +
  geom_point(
    data = df_mean_peak_temp,
    mapping = aes(Slope1, Slopedifference_peak_temp),
    size = 3,
    color = "#22A884FF",
    fill = "#22A884FF",
    shape = 21,
    stroke = 1
  ) +
  geom_errorbar(data = df_mean_end_temp, 
                mapping = aes(
                  x = Slope1,  # Specify x-axis aesthetic
                  ymin = Slopedifference_end_temp - SlopediffSE, 
                  ymax = Slopedifference_end_temp + SlopediffSE
                ), 
                width = 0.0,
                color = "#7AD151FF",
                position = position_dodge(0.05), 
                size = 1, 
                alpha = 0.5) +
  # Second error bars for x-values
  geom_errorbar(data = df_mean_end_temp, 
                mapping = aes(
                  y = Slopedifference_end_temp,  # Specify y-axis aesthetic
                  xmin = Slope1 - SlopeSE, 
                  xmax = Slope1 + SlopeSE
                ), 
                width = 0.0,
                color = "#7AD151FF",
                position = position_dodge(0.05), 
                size = 1, 
                alpha = 0.5) +
  geom_point(
    data = df_mean_end_temp,
    mapping = aes(Slope1, Slopedifference_end_temp),
    size = 3,
    color = "#7AD151FF",
    fill = "#7AD151FF",
    shape = 21,
    stroke = 1
  ) +
  theme_bw() +
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
    #plot.margin = margin(-7.5, 2, 5.5, 0, "cm")
  )


require(ggpubr)

ggarrange(snow_single, temp_single, snow, temp) +
  theme(plot.margin = margin(6, 2, 6, 2, "cm"))



