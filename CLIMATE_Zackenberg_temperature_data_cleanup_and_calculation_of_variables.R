
#####CLEAN TEMPERATURE DATA AND CALCULATE AVERAGE SUMMER AND SPRING TEMPERATURES####

library(tidyverse)
library(readxl) 
#install.packages("hms")
library(hms)#Makes it easier to store and format time-of-day values based on the difftime class.
#install.packages("strucchange")
library(zoo) 
library(strucchange) #Testing, monitoring and dating structural changes in linear regression models.
#install.packages("gvlma")
library(gvlma) #For assessing linear model assumptions. 
#install.packages("reshape2")
#install.packages("data.table")
library(data.table) #Extension of data.frame. Fast aggregation of large data.
library(cowplot)

#install.Rtools()

#Read file:Air Temperature and Soil Temperature. Provide full path to file
dfair <-
  read.table("Data/climate_data/temperature/raw_data/air_temperature_200cm_60min_sample_data.txt", header = TRUE, sep = "\t")


dfair = dfair %>% rename("HourTemp" = "AT...C.") #Fejl vises ved Date.

colnames(dfair) #Når script åbnes påny laver den uforstaaelige bogstaver om til ?

#Add sensor column. Inden datasættene forbindes skal der tilføjes en kolonne som angiver hvilke type data der er tale om.
dfair$Sensor <- "Air"

dfair$HourTemp <- as.numeric(dfair$HourTemp)

#Add columns: Hour, DOY, MONTH, Year
dfair %>%
  separate(Time, c("Hour", "Minute", 'Seconds'), ":") -> df

df$DOY <- yday(ymd(df$Date))
df$Month <- month(ymd(df$Date))
df$Year <- year(ymd(df$Date))

#Change columns to correct format for calculations
df$HourTemp[df$HourTemp == -9999] <- NA
df$HourTemp <- as.numeric(df$HourTemp)

#Seasonal variation in soil temperature across all three soil temp series
df %>%
  group_by(Sensor, Year, DOY) %>%
  summarise(DOYTemp = mean(HourTemp, na.rm = T)) %>%
  ggplot(aes(x = DOY, y = DOYTemp, colour = Sensor)) +
  geom_line() +
  ylab("Mean daily temperature (degrees C)") + ### of Soil measured at 0,5 and 10 cm
  facet_wrap( ~ Year)


#Air temperature calculations
df %>%
  group_by(Year, Month, DOY) %>%
  summarise(
    DOYTemp = mean(HourTemp, na.rm = T),
    Min = min(HourTemp, na.rm = T),
    Max = max(HourTemp, na.rm = T)
  ) -> dfair1

ggplot(dfair1, aes(x = DOY, y = Max)) +
  geom_line() +
  ylab("Max temperature (degrees C)") +
  facet_wrap( ~ Year)

#Calculating the rolling mean (air temp)
#dfair1$doymean<-rollmean(dfair1$DOYTemp,k=50,fill=NA,align="right")
#Problem with NA values in 1996 which can be fixed with rollapply
dfair1$doymean_apply<-rollapply(dfair1$DOYTemp,width=30,FUN=mean,na.rm = TRUE,fill=NA,align="right")

ggplot(dfair1, aes(x=DOY, y=doymean_apply)) + 
  geom_line() +
  ylab("Mean rolling 30 day temperature (degrees C)") +
  facet_wrap(~Year)

#write.csv(dfair1, file="Data/Climate_data_Zackenberg\\dfair1_mean_temp.csv", row.names = FALSE)

