
library(tidyverse)
library(readxl)
#install.packages("hms")
#install.packages("gvlma")
library(gvlma) #For assessing linear model assumptions.
#install.packages("data.table")
library(data.table)


dfsnowdepth <-
  read.table("Data/climate_data/snow/raw_data/snow_depth_180min_sample_data.txt", header = TRUE, sep = "\t")

#Correcting column names
dfsnowdepth = dfsnowdepth %>% rename("SnowDepth" = "SD..m.")


#Add columns HOUR, DOY, MONTH, YEAR
dfsnowdepth %>%
  separate(Time, c("Hour", "Minute", 'Seconds'), ":") -> dfsnowdepth


dfsnowdepth$DOY <- yday(ymd(dfsnowdepth$Date))
dfsnowdepth$Month <- month(ymd(dfsnowdepth$Date))
dfsnowdepth$Year <- year(ymd(dfsnowdepth$Date))

#Turn all -9999 where measurements have not been made to NA's
dfsnowdepth$SnowDepth[dfsnowdepth$SnowDepth == -9999] <- NA
dfsnowdepth$SnowDepth <- as.numeric(dfsnowdepth$SnowDepth)


dfsnowdepth %>%
  group_by(Year, DOY) %>%
  summarize(DOYsnowdepth = mean(SnowDepth, na.rm = T)) %>%
  ggplot(aes(x = DOY, y = DOYsnowdepth)) +
  geom_line() +
  ylab("Mean daily snow depth (m)") + ### of Soil measured at 0,5 and 10 cm
  facet_wrap( ~ Year)

dfsnowdepth %>%
  group_by(Year, Month, DOY) %>%
  summarize(DOYsnowdepth = mean(SnowDepth, na.rm = T)) -> dfsnowdepth_mean

#dfsnowdepth_mean%>%
#group_by(Year,DOY)%>%
#summarize(Include=ifelse(DOYsnowdepth<0.1,1,0))->dfsnowmelt1

#Remove months <4 as in some years, there is no snow at the beginning of the year
dfsnowdepth_mean %>%
  subset(!Month < 4) -> dfsnowdepth_mean


dfsnowdepth_mean$DOYsnowdepth_include <-
  ifelse(dfsnowdepth_mean$DOYsnowdepth < 0.1, 1, 0)

###Find DOY with <10 cm snow cover
first_equal_to <- function(x, value) {
  (x == value) & (cumsum(x == value) == 1)
}

dfsnowdepth_mean %>%
  subset(!is.na(DOYsnowdepth_include)) %>%
  group_by(Year) %>%
  mutate(first = first_equal_to(DOYsnowdepth_include, 1)) -> dfsnowmelt2

dfsnowmelt2$first[dfsnowmelt2$first == FALSE] <- 0

dfsnowmelt2 <- subset(dfsnowmelt2, first != "0")


dfsnowmelt2 %>%
  dplyr::select(-c(Month, DOYsnowdepth_include, first)) -> dfsnowmelt3

#write.csv(dfsnowmelt3, file = "Data\\Snowmelt_Climatestation.csv", row.names =
           # FALSE)

#library(writexl)
#write_xlsx(
#  dfsnowmelt3,
#  "Data/climate_data/snow\\Snowmelt_Climatestation.xlsx",
#  col_names = TRUE
#)

###############################################################################
########### The below code is needed for years with very early ################
########### snowmelt or years where no snowdepth was measured. ################
########### This is the case for years: 1996, 2009, 2013, 2019 ################
###############################################################################


####Compare DOY mean temperature with date of snowmelt to fill empty observations
#in timing of snowmelt variable

df_temp <-
  read.csv(
    "Data/df_mean_temp_red.csv",
    sep = ",",
    stringsAsFactors = FALSE,
    header = TRUE
  )

#To find snow melting dates for years with little snow we use temperature data
#The requirements for finding the zero curtain each year is as follows (following Rixen et al., 2022)
#1. Day of mean daily temp between -1 and +1 degrees C
#2. Mean daily temp (Max - min) rise above +2 degrees C after diurnal fluctuations

#Calculate Max - Min daily temp
df_temp %>%
  group_by(Year, Month, DOY) %>%
  summarise(K = Max - Min) -> df2

df_temp$K <-
  df2$K[match(
    paste0(df_temp$Year, df_temp$Month, df_temp$DOY),
    paste0(df2$Year, df2$Month, df2$DOY)
  )]

#Find days included in zero curtain for each year
df_temp$Include <-
  ifelse(df_temp$DOYTemp > -1.2 & df_temp$DOYTemp < 1.20 &
           df_temp$K < 2, 1, 0)

#Subset zero curtain periods
df3 <- subset(df_temp, Include == "1")

#The last day of the zero curtain period is the day of snowmelt
df3 %>%
  group_by(Year) %>%
  summarise(SnowmeltDOY = max(DOY)) -> df_snow

#Final dataframe is df_snow. Save file

#write_xlsx(
  #df_snow,
  #"Data\\Snowmelt_TemperatureData.xlsx",
  #col_names = TRUE
#)

df_temp <-
  read.csv(
    "Data/snowmelt_data/Snowmelt_TemperatureData.csv",
    sep = ",",
    stringsAsFactors = FALSE,
    header = TRUE
  )
dfsnowmelt_climatestation <-
  readxl::read_xlsx("Data/snowmelt_data/Snowmelt_snowmeltDOY_temperature_censor.xlsx")

dfsnowmelt_climatestation <-
  subset(dfsnowmelt_climatestation, Year != "1996")
dfsnowmelt_climatestation <-
  subset(dfsnowmelt_climatestation, Year != "2009")
dfsnowmelt_climatestation <-
  subset(dfsnowmelt_climatestation, Year != "2013")
dfsnowmelt_climatestation <-
  subset(dfsnowmelt_climatestation, Year != "2019")

ggplot(dfsnowmelt_climatestation, aes(DOY, SnowmeltDOY)) +
  geom_point(size = 3) +
  geom_abline(intercept = 0, slope = 1) +
  scale_x_continuous(breaks = c(150, 160, 170, 180, 190, 200)) +
  scale_y_continuous(breaks = c(150, 160, 170, 180, 190, 200)) +
  xlab("Snowmelt timing from snow depth censor") +
  ylab("Snowmelt timing from soil temperature") +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.title.y = element_text(
      face = "bold",
      size = 12,
      color = "black",
      vjust = 3
    ),
    axis.title.x = element_text(
      face = "bold",
      size = 12,
      color = "black",
      vjust = -1
    ),
    axis.text.y = element_text(
      face = "bold",
      size = 10,
      color = "black"
    ),
    axis.text.x = element_text(
      face = "bold",
      size = 10,
      color = "black"
    ),
    panel.border = element_rect(
      color = "black",
      fill = NA,
      size = 2
    ),
    plot.margin = margin(10, 4, 10, 4, "cm")
  )

res1 <-
  cor.test(dfsnowmelt_climatestation$DOY,
           dfsnowmelt_climatestation$DOY,
           method = "pearson")
res1

#Comparing timing of snowmelt at climate station and in local plots

df1_Art1 <- subset(df1, Plot == "Art1")
df1_Art2 <- subset(df1, Plot == "Art2")
df1_Art3 <- subset(df1, Plot == "Art3")
df1_Art4 <- subset(df1, Plot == "Art4")
df1_Art5 <- subset(df1, Plot == "Art5")
df1_Art6 <- subset(df1, Plot == "Art6")
df1_Art7 <- subset(df1, Plot == "Art7")

df_Art1 = merge(df_temp, df1_Art1, by = "Year")
class(df_Art1$MeanSnowmelt)
df_Art1$MeanSnowmelt <- as.numeric(df_Art1$MeanSnowmelt)

df_Art2 = merge(df_temp, df1_Art2, by = "Year")
class(df_Art2$MeanSnowmelt)
df_Art2$MeanSnowmelt <- as.numeric(df_Art2$MeanSnowmelt)

df_Art3 = merge(df_temp, df1_Art3, by = "Year")
class(df_Art3$MeanSnowmelt)
df_Art3$MeanSnowmelt <- as.numeric(df_Art3$MeanSnowmelt)

df_Art4 = merge(df_temp, df1_Art4, by = "Year")
class(df_Art4$MeanSnowmelt)
df_Art4$MeanSnowmelt <- as.numeric(df_Art4$MeanSnowmelt)

df_Art5 = merge(df_temp, df1_Art5, by = "Year")
class(df_Art5$MeanSnowmelt)
df_Art5$MeanSnowmelt <- as.numeric(df_Art5$MeanSnowmelt)

df_Art6 = merge(df_temp, df1_Art6, by = "Year")
class(df_Art6$MeanSnowmelt)
df_Art6$MeanSnowmelt <- as.numeric(df_Art6$MeanSnowmelt)

df_Art7 = merge(df_temp, df1_Art7, by = "Year")
class(df_Art7$MeanSnowmelt)
df_Art7$MeanSnowmelt <- as.numeric(df_Art7$MeanSnowmelt)

res1 <- cor.test(df_Art1$SnowmeltDOY, df_Art1$MeanSnowmelt,
                 method = "pearson")
res1

res2 <- cor.test(df_Art2$SnowmeltDOY, df_Art2$MeanSnowmelt,
                 method = "pearson")
res2

res3 <- cor.test(df_Art3$SnowmeltDOY, df_Art3$MeanSnowmelt,
                 method = "pearson")
res3

res4 <- cor.test(df_Art4$SnowmeltDOY, df_Art4$MeanSnowmelt,
                 method = "pearson")
res4

res5 <- cor.test(df_Art5$SnowmeltDOY, df_Art5$MeanSnowmelt,
                 method = "pearson")
res5

res6 <- cor.test(df_Art6$SnowmeltDOY, df_Art6$MeanSnowmelt,
                 method = "pearson")
res6

res7 <- cor.test(df_Art7$SnowmeltDOY, df_Art7$MeanSnowmelt,
                 method = "pearson")
res7


ggplot(df_Art7, aes(SnowmeltDOY, MeanSnowmelt)) +
  geom_point() +
  geom_smooth(method = "lm")

