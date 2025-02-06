#Comparing slopes between models with one and two predictors

library(tidyverse)
library(readxl)
library(metafor)

df_onset_snow <-
  read_xlsx("Data/Summary_tables/Final/df_summary_onset_snow_final_new.xlsx")
df_onset_snow$Model <- "M1"

#df_onset_snow <- df_onset_snow[,-19] 
#write_xlsx(df_onset_snow, "Data/Summary_tables/Final\\df_summary_onset_snow_final.xlsx", col_names = TRUE)

df_onset_multiple <-
  read_xlsx("Data/Summary_tables/Final/df_summary_onset_snow_temp_final_new.xlsx")
df_onset_multiple$Model <- "M2"


df_summary_onset_snow <- rbind(df_onset_snow, df_onset_multiple)


df_summary_onset_snow$plotspec <-
  as.factor(paste(df_summary_onset_snow$SpeciesID, df_summary_onset_snow$Plot))

####Slope difference comparison #####

df_summary_onset_snow$SEslopediffsq <- df_summary_onset_snow$SEslopediff ^ 2


all <-
  rma.mv(
    yi = Slopediff,
    V = SEslopediffsq,
    mods = ~ Model,
    random = list( ~ 1 |
                     SpeciesID, ~ 1 | Plot, ~ 1 | plotspec),
    data = df_summary_onset_snow
  )

summary(all)

####First linear segment comparison####

df_summary_onset_snow$SEslopesq <- df_summary_onset_snow$SEslope ^ 2


all_slope <-
  rma.mv(
    yi = Slope1,
    V = SEslopesq,
    mods = ~ Model,
    random = list( ~ 1 |
                     SpeciesID, ~ 1 | Plot, ~ 1 | plotspec),
    data = df_summary_onset_snow
  )

summary(all_slope)


######### PEAK SNOWMELT #########

df_peak_snow <-
  read_xlsx("Data/Summary_tables/Final/df_summary_peak_snow_final_new.xlsx")
df_peak_snow$Model <- "M1"

df_peak_multiple <-
  read_xlsx("Data/Summary_tables/Final/df_summary_peak_snow_temp_final_new.xlsx")
df_peak_multiple$Model <- "M2"

df_peak_snow <- df_peak_snow %>%
  select(-n.1)


#df_peak_snow <- df_peak_snow %>%
  #filter(!(SpeciesID == "Sciaridae" & Plot == "Plot 6"))


df_summary_peak_snow <- rbind(df_peak_snow, df_peak_multiple)


df_summary_peak_snow$plotspec <-
  as.factor(paste(df_summary_peak_snow$SpeciesID, df_summary_peak_snow$Plot))

####Slope difference comparison #####

df_summary_peak_snow$SEslopediffsq <- df_summary_peak_snow$SEslopediff ^ 2


all <-
  rma.mv(
    yi = Slopediff,
    V = SEslopediffsq,
    mods = ~ Model,
    random = list( ~ 1 |
                     SpeciesID, ~ 1 | Plot, ~ 1 | plotspec),
    data = df_summary_peak_snow
  )

summary(all)

####First linear segment comparison####

df_summary_peak_snow$SEslopesq <- df_summary_peak_snow$SEslope ^ 2


all_slope <-
  rma.mv(
    yi = Slope1,
    V = SEslopesq,
    mods = ~ Model,
    random = list( ~ 1 |
                     SpeciesID, ~ 1 | Plot, ~ 1 | plotspec),
    data = df_summary_peak_snow
  )

summary(all_slope)


######### END SNOWMELT #########

df_end_snow <-
  read_xlsx("Data/Summary_tables/Final/df_summary_end_snow_final_new.xlsx")
df_end_snow$Model <- "M1"

df_end_multiple <-
  read_xlsx("Data/Summary_tables/Final/df_summary_end_snow_temp_final_new.xlsx")
df_end_multiple$Model <- "M2"

df_end_snow <- df_end_snow %>%
  select(-n.1)

df_summary_end_snow <- rbind(df_end_snow, df_end_multiple)


df_summary_end_snow$plotspec <-
  as.factor(paste(df_summary_end_snow$SpeciesID, df_summary_end_snow$Plot))

####Slope difference comparison #####

df_summary_end_snow$SEslopediffsq <- df_summary_end_snow$SEslopediff ^ 2


all <-
  rma.mv(
    yi = Slopediff,
    V = SEslopediffsq,
    mods = ~ Model,
    random = list( ~ 1 |
                     SpeciesID, ~ 1 | Plot, ~ 1 | plotspec),
    data = df_summary_end_snow
  )

summary(all)

####First linear segment comparison####

df_summary_end_snow$SEslopesq <- df_summary_end_snow$SEslope ^ 2


all_slope <-
  rma.mv(
    yi = Slope1,
    V = SEslopesq,
    mods = ~ Model,
    random = list( ~ 1 |
                     SpeciesID, ~ 1 | Plot, ~ 1 | plotspec),
    data = df_summary_end_snow
  )

summary(all_slope)

##############################TEMPERATURE###################################


df_onset_temp <-
  read_xlsx("Data/Summary_tables/Final/df_summary_onset_temp_final_new.xlsx")
df_onset_temp$Model <- "M1"

#df_onset_temp <- df_onset_temp[,-19] 
#write_xlsx(df_onset_temp, "Data/Summary_tables/Final\\df_summary_onset_temp_final.xlsx", col_names = TRUE)

df_onset_multiple_temp <-
  read_xlsx("Data/Summary_tables/Final/df_summary_onset_temp_snow_final_new.xlsx")
df_onset_multiple_temp$Model <- "M2"


df_summary_onset_temp <- rbind(df_onset_temp, df_onset_multiple_temp)


df_summary_onset_temp$plotspec <-
  as.factor(paste(df_summary_onset_temp$SpeciesID, df_summary_onset_temp$Plot))

####Slope difference comparison #####

df_summary_onset_temp$SEslopediffsq <- df_summary_onset_temp$SEslopediff ^ 2


all <-
  rma.mv(
    yi = Slopediff,
    V = SEslopediffsq,
    mods = ~ Model,
    random = list( ~ 1 |
                     SpeciesID, ~ 1 | Plot, ~ 1 | plotspec),
    data = df_summary_onset_temp
  )

summary(all)

####First linear segment comparison####

df_summary_onset_temp$SEslopesq <- df_summary_onset_temp$SEslope ^ 2


all_slope <-
  rma.mv(
    yi = Slope1,
    V = SEslopesq,
    mods = ~ Model,
    random = list( ~ 1 |
                     SpeciesID, ~ 1 | Plot, ~ 1 | plotspec),
    data = df_summary_onset_temp
  )

summary(all_slope)

######### PEAK TEMPERATURE #########

df_peak_temp <-
  read_xlsx("Data/Summary_tables/Final/df_summary_peak_temp_final_new.xlsx")
df_peak_temp$Model <- "M1"

df_peak_multiple_temp <-
  read_xlsx("Data/Summary_tables/Final/df_summary_peak_temp_snow_final_new.xlsx")
df_peak_multiple_temp$Model <- "M2"


#df_peak_temp <- df_peak_temp[,-19] 
#write_xlsx(df_peak_temp, "Data/Summary_tables/Final\\df_summary_peak_temp_final.xlsx", col_names = TRUE)

df_summary_peak_temp <- rbind(df_peak_temp, df_peak_multiple_temp)


df_summary_peak_temp$plotspec <-
  as.factor(paste(df_summary_peak_temp$SpeciesID, df_summary_peak_temp$Plot))

####Slope difference comparison #####

df_summary_peak_temp$SEslopediffsq <- df_summary_peak_temp$SEslopediff ^ 2


all <-
  rma.mv(
    yi = Slopediff,
    V = SEslopediffsq,
    mods = ~ Model,
    random = list( ~ 1 |
                     SpeciesID, ~ 1 | Plot, ~ 1 | plotspec),
    data = df_summary_peak_temp
  )

summary(all)

####First linear segment comparison####

df_summary_peak_temp$SEslopesq <- df_summary_peak_temp$SEslope ^ 2


all_slope <-
  rma.mv(
    yi = Slope1,
    V = SEslopesq,
    mods = ~ Model,
    random = list( ~ 1 |
                     SpeciesID, ~ 1 | Plot, ~ 1 | plotspec),
    data = df_summary_peak_temp
  )

summary(all_slope)


######### END TEMPERATURE #########

df_end_temp <-
  read_xlsx("Data/Summary_tables/Final/df_summary_end_temp_final_new.xlsx")
df_end_temp$Model <- "M1"

df_end_multiple_temp <-
  read_xlsx("Data/Summary_tables/Final/df_summary_end_temp_snow_final_new.xlsx")
df_end_multiple_temp$Model <- "M2"

df_summary_end_temp <- rbind(df_end_temp, df_end_multiple_temp)


df_summary_end_temp$plotspec <-
  as.factor(paste(df_summary_end_temp$SpeciesID, df_summary_end_temp$Plot))

####Slope difference comparison #####

df_summary_end_temp$SEslopediffsq <- df_summary_end_temp$SEslopediff ^ 2


all <-
  rma.mv(
    yi = Slopediff,
    V = SEslopediffsq,
    mods = ~ Model,
    random = list( ~ 1 |
                     SpeciesID, ~ 1 | Plot, ~ 1 | plotspec),
    data = df_summary_end_temp
  )

summary(all)

####First linear segment comparison####

df_summary_end_temp$SEslopesq <- df_summary_end_temp$SEslope ^ 2


all_slope <-
  rma.mv(
    yi = Slope1,
    V = SEslopesq,
    mods = ~ Model,
    random = list( ~ 1 |
                     SpeciesID, ~ 1 | Plot, ~ 1 | plotspec),
    data = df_summary_end_temp
  )

summary(all_slope)


