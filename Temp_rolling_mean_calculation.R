#Calculating rolling mean temperature 50 days back

library(tidyverse)
library(readxl) 
library(lubridate) #Functions to work with date-time data.
library(dplyr)
library(tidyr)
library(ggplot2)
#install.packages("hms")
library(hms)#Makes it easier to store and format time-of-day values based on the difftime class.
#install.packages("strucchange")
library(strucchange) #Testing, monitoring and dating structural changes in linear regression models.
#install.packages("gvlma")
library(gvlma) #For assessing linear model assumptions. 
#install.packages("reshape2")
library(reshape2) #Outdated, better to use Tidyr. Makes it easier to transform data between wide and long formats.
#install.packages("data.table")
library(data.table) #Extension of data.frame. Fast aggregation of large data.
library(zoo) #Calculating a rolling mean
library(cowplot)
library(sjPlot)

Air_temp <-
  read.table("Data/climate_data/temperature/raw_data/air_temperature_200cm_60min_sample_data.txt", header = TRUE, sep = "\t")

Air_temp = Air_temp %>% rename("HourTemp" = "AT...C.") #Fejl vises ved Date.

Air_temp$HourTemp <- as.numeric(Air_temp$HourTemp)

Air_temp %>%
  separate(Time, c("Hour", "Minute", 'Seconds'), ":") -> df

df$DOY <- yday(ymd(df$Date))
df$Month <- month(ymd(df$Date))
df$Year <- year(ymd(df$Date))

#Change columns to correct format for calculations
df$HourTemp[df$HourTemp == -9999] <-NA
df$HourTemp <-as.numeric(df$HourTemp)

df %>%
  group_by(Year,Month,DOY) %>% 
  summarise(DOYTemp=mean(HourTemp,na.rm=T),
            Min=min(HourTemp,na.rm=T),
            Max=max(HourTemp,na.rm=T)) -> dfair1

ggplot(dfair1, aes(x=DOY, y=Max)) + 
  geom_line() +
  ylab("Max temperature (degrees C)") +
  facet_wrap(~Year)

#Calculating the rolling mean (air temp)
#dfair1$doymean<-rollmean(dfair1$DOYTemp,k=50,fill=NA,align="right")
#Problem with NA values in 1996 which can be fixed with rollapply
dfair1$doymean_apply<-rollapply(dfair1$DOYTemp,width=30,FUN=mean,na.rm = TRUE,fill=NA,align="right")

ggplot(dfair1, aes(x=DOY, y=doymean_apply)) + 
  geom_line() +
  ylab("Mean rolling 30 day temperature (degrees C)") +
  facet_wrap(~Year)

#write.csv(dfair1, file="Data\\dfair1_mean_temp.csv", row.names = FALSE)

####ROLLING MEAN IS NOW MATCHED WITH AVERAGE PHENO EVENTS ACROSS YEARS FOR ALL TAXA AND PLOTS####


df_phen_event <-
 read.csv(
  "Data/phenology_data\\df_phenology_metrics.csv",
   sep = ",",
    stringsAsFactors = FALSE,
    header = TRUE
  )

#For air temperature
#df_mean_temp_air <-
 # read.csv(
  #  "Data\\dfair1_mean_temp.csv",
   # sep = ",",
  #  stringsAsFactors = FALSE,
   # header = TRUE
  #)

df_phen_event%>%
  group_by(SpeciesID, Plot)%>% 
  summarise(Onset_meanDOY=mean(Onset,na.rm=T),
            Peak_meanDOY=mean(Peak,na.rm=T),
            End_meanDOY=mean(End,na.rm=T),
            Onset_SD=sd(Onset,na.rm=T),
            Peak_SD=sd(Peak,na.rm=T),
            End_SD=sd(End,na.rm=T))-> df_phen_event_mean


df_phen_event_mean%>%
  group_by(SpeciesID, Plot)%>% 
  summarise(Onset_DOY=Onset_meanDOY-Onset_SD,
            Peak_DOY=Peak_meanDOY-Peak_SD,
            End_DOY=End_meanDOY-End_SD)-> df_phen_event_min


df_phen_event_min[,-1:-2] <- round(df_phen_event_min[,-1:-2], 0)

#Match with original dataframe
df_phen_event$Onset_DOY <- df_phen_event_min$Onset_DOY[match(paste0(df_phen_event$SpeciesID,df_phen_event$Plot),
                                                             paste0(df_phen_event_min$SpeciesID,df_phen_event_min$Plot))]

df_phen_event$Peak_DOY <- df_phen_event_min$Peak_DOY[match(paste0(df_phen_event$SpeciesID,df_phen_event$Plot),
                                                           paste0(df_phen_event_min$SpeciesID,df_phen_event_min$Plot))]

df_phen_event$End_DOY <- df_phen_event_min$End_DOY[match(paste0(df_phen_event$SpeciesID,df_phen_event$Plot),
                                                         paste0(df_phen_event_min$SpeciesID,df_phen_event_min$Plot))]

#Match rollmean temperature 30 days with doy for phen event

df_phen_event$Onset_Temp <- dfair1$doymean_apply[match(paste0(df_phen_event$Year,df_phen_event$Onset_DOY),
                                                           paste0(dfair1$Year,dfair1$DOY))]

df_phen_event$Peak_Temp <- dfair1$doymean_apply[match(paste0(df_phen_event$Year,df_phen_event$Peak_DOY),
                                                          paste0(dfair1$Year,dfair1$DOY))]

df_phen_event$End_Temp <- dfair1$doymean_apply[match(paste0(df_phen_event$Year,df_phen_event$End_DOY),
                                                         paste0(dfair1$Year,dfair1$DOY))]

write.csv(df_phen_event, file="Data/phenology_data\\Air_temp_30_days_rolling.csv", row.names = FALSE)


####Calculating the rolling mean (air temp) with 50 days####

dfair1$doymean<-rollmean(dfair1$DOYTemp,k=50,fill=NA,align="right")
#Problem with NA values in 1996 which can be fixed with rollapply
dfair1$doymean_apply_50<-rollapply(dfair1$DOYTemp,width=50,FUN=mean,na.rm = TRUE,fill=NA,align="right")

ggplot(dfair1, aes(x=DOY, y=doymean_apply_50)) + 
  geom_line() +
  ylab("Mean rolling 50 day temperature (degrees C)") +
  facet_wrap(~Year)


#Match rollmean temperature 50 days with doy for phen event

df_phen_event$Onset_Temp_50 <- dfair1$doymean_apply_50[match(paste0(df_phen_event$Year,df_phen_event$Onset_DOY),
                                                           paste0(dfair1$Year,dfair1$DOY))]

df_phen_event$Peak_Temp_50 <- dfair1$doymean_apply_50[match(paste0(df_phen_event$Year,df_phen_event$Peak_DOY),
                                                          paste0(dfair1$Year,dfair1$DOY))]

df_phen_event$End_Temp_50 <- dfair1$doymean_apply_50[match(paste0(df_phen_event$Year,df_phen_event$End_DOY),
                                                         paste0(dfair1$Year,dfair1$DOY))]

write.csv(df_phen_event, file="Data/phenology_data\\Air_temp_50_days_rolling.csv", row.names = FALSE)


####Calculating the rolling mean (air temp) with 40 day window####

#dfair1$doymean<-rollmean(dfair1$DOYTemp,k=40,fill=NA,align="right")
#Problem with NA values in 1996 which can be fixed with rollapply
dfair1$doymean_apply_40<-rollapply(dfair1$DOYTemp,width=40,FUN=mean,na.rm = TRUE,fill=NA,align="right")

ggplot(dfair1, aes(x=DOY, y=doymean_apply)) + 
  geom_line() +
  ylab("Mean rolling 30 day temperature (degrees C)") +
  facet_wrap(~Year)

#write.csv(dfair1, file="Data\\dfair1_mean_temp.csv", row.names = FALSE)

#Match rollmean temperature 40 days with doy for phen event

df_phen_event$Onset_Temp_40 <- dfair1$doymean_apply_40[match(paste0(df_phen_event$Year,df_phen_event$Onset_DOY),
                                                             paste0(dfair1$Year,dfair1$DOY))]

df_phen_event$Peak_Temp_40 <- dfair1$doymean_apply_40[match(paste0(df_phen_event$Year,df_phen_event$Peak_DOY),
                                                            paste0(dfair1$Year,dfair1$DOY))]

df_phen_event$End_Temp_40 <- dfair1$doymean_apply_40[match(paste0(df_phen_event$Year,df_phen_event$End_DOY),
                                                           paste0(dfair1$Year,dfair1$DOY))]

write.csv(df_phen_event, file="Data\\Air_temp_40_days_rolling.csv", row.names = FALSE)


####Calculating the rolling mean (air temp) with 20 day window####

#dfair1$doymean<-rollmean(dfair1$DOYTemp,k=20,fill=NA,align="right")
#Problem with NA values in 1996 which can be fixed with rollapply
dfair1$doymean_apply_20<-rollapply(dfair1$DOYTemp,width=20,FUN=mean,na.rm = TRUE,fill=NA,align="right")

ggplot(dfair1, aes(x=DOY, y=doymean_apply)) + 
  geom_line() +
  ylab("Mean rolling 30 day temperature (degrees C)") +
  facet_wrap(~Year)

#write.csv(dfair1, file="Data\\dfair1_mean_temp.csv", row.names = FALSE)

#Match rollmean temperature 20 days with doy for phen event

df_phen_event$Onset_Temp_20 <- dfair1$doymean_apply_20[match(paste0(df_phen_event$Year,df_phen_event$Onset_DOY),
                                                           paste0(dfair1$Year,dfair1$DOY))]

df_phen_event$Peak_Temp_20 <- dfair1$doymean_apply_20[match(paste0(df_phen_event$Year,df_phen_event$Peak_DOY),
                                                          paste0(dfair1$Year,dfair1$DOY))]

df_phen_event$End_Temp_20 <- dfair1$doymean_apply_20[match(paste0(df_phen_event$Year,df_phen_event$End_DOY),
                                                         paste0(dfair1$Year,dfair1$DOY))]

write.csv(df_phen_event, file="Data\\Air_temp_20_days_rolling.csv", row.names = FALSE)

write.csv(df_phen_event, file="Data\\Air_temp_all_rolling_temp.csv", row.names = FALSE)


####30 day rolling mean####

#Calculating the rolling mean (air temp)
dfair1$doymean<-rollmean(dfair1$DOYTemp,k=30,fill=NA,align="right")
#Problem with NA values in 1996 which can be fixed with rollapply
dfair1$doymean_apply<-rollapply(dfair1$DOYTemp,width=30,FUN=mean,na.rm = TRUE,fill=NA,align="right")

ggplot(dfair1, aes(x=DOY, y=doymean_apply)) + 
  geom_line() +
  ylab("Mean rolling 30 day temperature (degrees C)") +
  facet_wrap(~Year)

#write.csv(dfair1, file="Data\\dfair1_mean_temp.csv", row.names = FALSE)

####ROLLING MEAN IS NOW MATCHED WITH AVERAGE PHENO EVENTS ACROSS YEARS FOR ALL TAXA AND PLOTS####


df_phen_event <-
  read.csv(
    "Data\\duration.csv",
    sep = ",",
    stringsAsFactors = FALSE,
    header = TRUE
  )

#For air temperature
#df_mean_temp_air <-
# read.csv(
#  "Data\\dfair1_mean_temp.csv",
# sep = ",",
#  stringsAsFactors = FALSE,
# header = TRUE
#)

df_phen_event%>%
  group_by(SpeciesID, Plot)%>% 
  summarise(Onset_meanDOY=mean(Onset,na.rm=T),
            Peak_meanDOY=mean(Peak,na.rm=T),
            End_meanDOY=mean(End,na.rm=T),
            Onset_SD=sd(Onset,na.rm=T),
            Peak_SD=sd(Peak,na.rm=T),
            End_SD=sd(End,na.rm=T))-> df_phen_event_mean


df_phen_event_mean%>%
  group_by(SpeciesID, Plot)%>% 
  summarise(Onset_DOY=Onset_meanDOY-Onset_SD,
            Peak_DOY=Peak_meanDOY-Peak_SD,
            End_DOY=End_meanDOY-End_SD)-> df_phen_event_min


df_phen_event_min[,-1:-2] <- round(df_phen_event_min[,-1:-2], 0)

#Match with original dataframe
df_phen_event$Onset_DOY <- df_phen_event_min$Onset_DOY[match(paste0(df_phen_event$SpeciesID,df_phen_event$Plot),
                                                             paste0(df_phen_event_min$SpeciesID,df_phen_event_min$Plot))]

df_phen_event$Peak_DOY <- df_phen_event_min$Peak_DOY[match(paste0(df_phen_event$SpeciesID,df_phen_event$Plot),
                                                           paste0(df_phen_event_min$SpeciesID,df_phen_event_min$Plot))]

df_phen_event$End_DOY <- df_phen_event_min$End_DOY[match(paste0(df_phen_event$SpeciesID,df_phen_event$Plot),
                                                         paste0(df_phen_event_min$SpeciesID,df_phen_event_min$Plot))]

#Match rollmean temperature 30 days with doy for phen event

df_phen_event$Onset_Temp <- dfair1$doymean_apply[match(paste0(df_phen_event$Year,df_phen_event$Onset_DOY),
                                                       paste0(dfair1$Year,dfair1$DOY))]

df_phen_event$Peak_Temp <- dfair1$doymean_apply[match(paste0(df_phen_event$Year,df_phen_event$Peak_DOY),
                                                      paste0(dfair1$Year,dfair1$DOY))]

df_phen_event$End_Temp <- dfair1$doymean_apply[match(paste0(df_phen_event$Year,df_phen_event$End_DOY),
                                                     paste0(dfair1$Year,dfair1$DOY))]

write.csv(df_phen_event, file="Data\\Air_temp_30_days_rolling.csv", row.names = FALSE)
