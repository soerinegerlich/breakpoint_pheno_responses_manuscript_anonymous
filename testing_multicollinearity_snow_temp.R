#Meta analysis using the metafor package in R

library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gvlma) #For assessing linear model assumptions. 
library(car)
library(writexl)
library(readxl)
library(lme4)
library(lmerTest)
#install.packages("rsq")
library(metafor)
library(rsq)
#install.packages("forestplot")
library(forestplot)
library(ggpubr)
library(scales)

df_phenology <-
  read.csv(
    "Data/phenology_data/df_phenology_metrics.csv",
    sep = ",",
    stringsAsFactors = FALSE,
    header = TRUE
  )

df_air <-
  read.csv(
    "Data/phenology_data/Air_temp_30_days_rolling.csv",
    sep = ",",
    stringsAsFactors = FALSE,
    header = TRUE
  )

# Assuming the dataframes are df1 (main dataframe) and df2 (temperature values)
merged_data <- merge(df_phenology, df_air, by = c("Year", "SpeciesID", "Plot", "Onset", "Peak", "End"), all.x = TRUE)

df_phen_event <- merged_data %>%
  select("Year", "SpeciesID", "Plot", "Onset", "Peak", "End", "Onset_Temp", "Peak_Temp", "End_Temp")

#df_phen_event <- na.omit(df_phen_event)


dfsnowmelt_climatestation <-
  read_xlsx("Data/climate_data/snow/Snowmelt_Climatestation_updated.xlsx")


#Match climate variables with phen. event data to compile them in the same dataframe
df_phen_event$Snowmelt <-
  dfsnowmelt_climatestation$DOY[match(paste0(df_phen_event$Year),
                                      paste0(dfsnowmelt_climatestation$Year))]

df_phen_event %>%
  subset(!is.na(End_Temp) & !is.na(Onset)) -> df_phen_event


df_summary_all_air<-data.frame(SpeciesID=character(),Plot=character(),Pheno_event=character(),Slope1=numeric(),Slope2=numeric(),
                               SE1=numeric(),SE2=numeric(),Tvalue1=numeric(),Tvalue2=numeric(),Pvalue1=numeric(),Pvalue2=numeric(),
                               Count=numeric(),n=numeric(),AIC=numeric(),Rsquared=numeric(), 
                               Residual=numeric(),CI_lwr1=numeric(), CI_upr1=numeric(),CI_lwr2=numeric(),CI_upr2=numeric(),
                               vif_snow=numeric(), vif_temp=numeric())

for (i in unique(df_phen_event$SpeciesID)){
  print(i)
  df8b<-subset(df_phen_event,SpeciesID==i)
  for (j in unique(df8b$Plot)){
    print(j)
    df8a<-subset(df8b,Plot==j)
    df8sub<-subset(df8a,!is.na(Onset)&!is.na(Onset_Temp))
    
    if(length(df8sub$Year)<5){ #sum(is.na) finder alle NA værdier. !is.na fjerner alle NA værdier i en vektor. Men denne kan vel ikke bruges her?
      #print(sum(is.na(df8$Onset)))
      #print(sum(!is.na(df8$Onset)))
      #length(df8$Onset[is.na(df8$Onset)]) #Denne kode viser alle værdier af ikke-NA værdier!!
      df_temp<-data.frame(SpeciesID=df8sub$SpeciesID[1], #sum(!is.na()) er antallet af ikke-Na værdier
                          Plot=df8sub$Plot[1],#[1] betyder at indeksere en vektor. I dete tilfælde får du det første element som output.
                          Pheno_event="Onset",
                          Slope1=NA,
                          Slope2=NA,
                          SE1=NA,
                          SE2=NA,
                          Tvalue1=NA,
                          Tvalue2=NA,
                          Pvalue1=NA,
                          Pvalue2=NA,
                          Count=NA,
                          n=NA,
                          AIC=NA,
                          Rsquared=NA,
                          Residual=NA,
                          CI_lwr1=NA,
                          CI_upr1=NA,
                          CI_lwr2=NA,
                          CI_upr2=NA,
                          vif_snow=NA,
                          vif_temp=NA)
    }
    else{ 
      mod1 <- lm(Onset ~ Snowmelt+Onset_Temp, data=df8sub)       
      AIC <- AIC(mod1)
      R.mod1 <- rsq(mod1, adj = TRUE)
      Residual1 <- sqrt(deviance(mod1)/df.residual(mod1))
      CI <- confint(mod1, level=0.95)
      df_temp<-data.frame(SpeciesID=df8sub$SpeciesID[1], 
                          Plot=df8sub$Plot[1],
                          Pheno_event="Onset",
                          Slope1=summary(mod1)$coefficients[2],
                          Slope2=summary(mod1)$coefficients[3],
                          SE1=summary(mod1)$coefficients[5],
                          SE2=summary(mod1)$coefficients[6],
                          Tvalue1=summary(mod1)$coefficients[8],
                          Tvalue2=summary(mod1)$coefficients[9],
                          Pvalue1=summary(mod1)$coefficients[11],
                          Pvalue2=summary(mod1)$coefficients[12],
                          Count=sum(df8sub$TotalAbundance),
                          n=sum(!is.na(df8sub$Onset)),
                          AIC=AIC,
                          Rsquared=R.mod1,
                          Residual=Residual1,
                          CI_lwr1=CI[2],
                          CI_upr1=CI[5],
                          CI_lwr2=CI[3],
                          CI_upr2=CI[6],
                          vif_snow=car::vif(mod1)[1],
                          vif_temp=car::vif(mod1)[2])
      df_summary_all_air<-bind_rows(df_summary_all_air,df_temp)
    }
    #plot(mod1)
    #}
    #} 
    
    
    if(length(df8sub$Year)<5){
      df_temp<-data.frame(SpeciesID=df8sub$SpeciesID[1],
                          Plot=df8sub$Plot[1],
                          Pheno_event="Peak",
                          Slope1=NA,
                          Slope2=NA,
                          SE1=NA,
                          SE2=NA,
                          Tvalue1=NA,
                          Tvalue2=NA,
                          Pvalue1=NA,
                          Pvalue2=NA,
                          Count=NA,
                          n=NA,
                          AIC=NA,
                          Rsquared=NA,
                          Residual=NA,
                          CI_lwr1=NA,
                          CI_upr1=NA,
                          CI_lwr2=NA,
                          CI_upr2=NA,
                          vif_snow=NA,
                          vif_temp=NA)
    }
    
    else{ 
      mod2 <- lm(Peak ~ Snowmelt+Peak_Temp, data=df8sub)
      AIC2 <- AIC(mod1)
      R.mod2 <- rsq(mod2, adj = TRUE)
      Residual2 <- sqrt(deviance(mod2)/df.residual(mod2))
      CI2 <- confint(mod2, level=0.95)
      df_temp<-data.frame(SpeciesID=df8sub$SpeciesID[1],
                          Plot=df8sub$Plot[1],
                          Pheno_event="Peak",
                          Slope1=summary(mod2)$coefficients[2],
                          Slope2=summary(mod2)$coefficients[3],
                          SE1=summary(mod2)$coefficients[5],
                          SE2=summary(mod2)$coefficients[6],
                          Tvalue1=summary(mod2)$coefficients[8],
                          Tvalue2=summary(mod2)$coefficients[9],
                          Pvalue1=summary(mod2)$coefficients[11],
                          Pvalue2=summary(mod2)$coefficients[12],
                          Count=sum(df8sub$TotalAbundance),
                          n=sum(!is.na(df8sub$Peak)),
                          AIC=AIC2,
                          Rsquared=R.mod2,
                          Residual=Residual2,
                          CI_lwr1=CI2[2],
                          CI_upr1=CI2[5],
                          CI_lwr2=CI2[3],
                          CI_upr2=CI2[6],
                          vif_snow=car::vif(mod2)[1],
                          vif_temp=car::vif(mod2)[2])
      df_summary_all_air<-bind_rows(df_summary_all_air,df_temp)
    }
    
    #plot(mod2)
    
    if(length(df8sub$Year)<5){
      df_temp<-data.frame(SpeciesID=df8sub$SpeciesID[1],
                          Plot=df8sub$Plot[1],
                          Pheno_event="End",
                          Slope1=NA,
                          Slope2=NA,
                          SE1=NA,
                          SE2=NA,
                          Tvalue1=NA,
                          Tvalue2=NA,
                          Pvalue1=NA,
                          Pvalue2=NA,
                          Count=NA,
                          n=NA,
                          AIC=NA,
                          Rsquared=NA,
                          Residual=NA,
                          CI_lwr1=NA,
                          CI_upr1=NA,
                          CI_lwr2=NA,
                          CI_upr2=NA,
                          vif_snow=NA,
                          vif_temp=NA)
    }
    
    else{ 
      
      mod3 <- lm(End ~ Snowmelt+End_Temp, data=df8sub)
      AIC3 <- AIC(mod3)
      R.mod3 <- rsq(mod3, adj = TRUE)
      Residual3 <- sqrt(deviance(mod3)/df.residual(mod3))
      CI3 <- confint(mod3, level=0.95)
      df_temp<-data.frame(SpeciesID=df8sub$SpeciesID[1],
                          Plot=df8sub$Plot[1],
                          Pheno_event="End",
                          Slope1=summary(mod3)$coefficients[2],
                          Slope2=summary(mod3)$coefficients[3],
                          SE1=summary(mod3)$coefficients[5],
                          SE2=summary(mod3)$coefficients[6],
                          Tvalue1=summary(mod3)$coefficients[8],
                          Tvalue2=summary(mod3)$coefficients[9],
                          Pvalue1=summary(mod3)$coefficients[11],
                          Pvalue2=summary(mod3)$coefficients[12],
                          Count=sum(df8sub$TotalAbundance),
                          n=sum(!is.na(df8sub$End)),
                          AIC=AIC3,
                          Rsquared=R.mod3,
                          Residual=Residual3,
                          CI_lwr1=CI3[2],
                          CI_upr1=CI3[5],
                          CI_lwr2=CI3[3],
                          CI_upr2=CI3[6],
                          vif_snow=car::vif(mod3)[1],
                          vif_temp=car::vif(mod3)[2])
      df_summary_all_air<-bind_rows(df_summary_all_air,df_temp)

    }
  }
}

df_summary_all_air <- subset(df_summary_all_air,SpeciesID!="Scathophagidae")

df_summary_all_air <- subset(df_summary_all_air,SpeciesID!="Lygaeidae")

write_xlsx(df_summary_all_air, "Data\\df_summary_all_linear_30_day.xlsx", col_names = TRUE)

###############################################################################

#### 50 days rolling mean ####


df_phenology <-
  read.csv(
    "Data/phenology_data/df_phenology_metrics.csv",
    sep = ",",
    stringsAsFactors = FALSE,
    header = TRUE
  )

df_air <-
  read.csv(
    "Data/phenology_data/Air_temp_50_days_rolling.csv",
    sep = ",",
    stringsAsFactors = FALSE,
    header = TRUE
  )

# Assuming the dataframes are df1 (main dataframe) and df2 (temperature values)
merged_data <- merge(df_phenology, df_air, by = c("Year", "SpeciesID", "Plot", "Onset", "Peak", "End"), all.x = TRUE)

df_phen_event <- merged_data %>%
  select("Year", "SpeciesID", "Plot", "Onset", "Peak", "End", "Onset_Temp_50", "Peak_Temp_50", "End_Temp_50")

#df_phen_event <- na.omit(df_phen_event)


dfsnowmelt_climatestation <-
  read_xlsx("Data/climate_data/snow/Snowmelt_Climatestation_updated.xlsx")


#Match climate variables with phen. event data to compile them in the same dataframe
df_phen_event$Snowmelt <-
  dfsnowmelt_climatestation$DOY[match(paste0(df_phen_event$Year),
                                      paste0(dfsnowmelt_climatestation$Year))]

df_phen_event %>%
  subset(!is.na(End_Temp_50) & !is.na(Onset)) -> df_phen_event


df_summary_all_air<-data.frame(SpeciesID=character(),Plot=character(),Pheno_event=character(),Slope1=numeric(),Slope2=numeric(),
                               SE1=numeric(),SE2=numeric(),Tvalue1=numeric(),Tvalue2=numeric(),Pvalue1=numeric(),Pvalue2=numeric(),
                               Count=numeric(),n=numeric(),AIC=numeric(),Rsquared=numeric(), 
                               Residual=numeric(),CI_lwr1=numeric(), CI_upr1=numeric(),CI_lwr2=numeric(),CI_upr2=numeric(),
                               vif_snow=numeric(), vif_temp=numeric())

for (i in unique(df_phen_event$SpeciesID)){
  print(i)
  df8b<-subset(df_phen_event,SpeciesID==i)
  for (j in unique(df8b$Plot)){
    print(j)
    df8a<-subset(df8b,Plot==j)
    df8sub<-subset(df8a,!is.na(Onset)&!is.na(Onset_Temp_50))
    
    if(length(df8sub$Year)<5){ #sum(is.na) finder alle NA værdier. !is.na fjerner alle NA værdier i en vektor. Men denne kan vel ikke bruges her?
      #print(sum(is.na(df8$Onset)))
      #print(sum(!is.na(df8$Onset)))
      #length(df8$Onset[is.na(df8$Onset)]) #Denne kode viser alle værdier af ikke-NA værdier!!
      df_temp<-data.frame(SpeciesID=df8sub$SpeciesID[1], #sum(!is.na()) er antallet af ikke-Na værdier
                          Plot=df8sub$Plot[1],#[1] betyder at indeksere en vektor. I dete tilfælde får du det første element som output.
                          Pheno_event="Onset",
                          Slope1=NA,
                          Slope2=NA,
                          SE1=NA,
                          SE2=NA,
                          Tvalue1=NA,
                          Tvalue2=NA,
                          Pvalue1=NA,
                          Pvalue2=NA,
                          Count=NA,
                          n=NA,
                          AIC=NA,
                          Rsquared=NA,
                          Residual=NA,
                          CI_lwr1=NA,
                          CI_upr1=NA,
                          CI_lwr2=NA,
                          CI_upr2=NA,
                          vif_snow=NA,
                          vif_temp=NA)
    }
    else{ 
      mod1 <- lm(Onset ~ Snowmelt+Onset_Temp_50, data=df8sub)       
      AIC <- AIC(mod1)
      R.mod1 <- rsq(mod1, adj = TRUE)
      Residual1 <- sqrt(deviance(mod1)/df.residual(mod1))
      CI <- confint(mod1, level=0.95)
      df_temp<-data.frame(SpeciesID=df8sub$SpeciesID[1], 
                          Plot=df8sub$Plot[1],
                          Pheno_event="Onset",
                          Slope1=summary(mod1)$coefficients[2],
                          Slope2=summary(mod1)$coefficients[3],
                          SE1=summary(mod1)$coefficients[5],
                          SE2=summary(mod1)$coefficients[6],
                          Tvalue1=summary(mod1)$coefficients[8],
                          Tvalue2=summary(mod1)$coefficients[9],
                          Pvalue1=summary(mod1)$coefficients[11],
                          Pvalue2=summary(mod1)$coefficients[12],
                          Count=sum(df8sub$TotalAbundance),
                          n=sum(!is.na(df8sub$Onset)),
                          AIC=AIC,
                          Rsquared=R.mod1,
                          Residual=Residual1,
                          CI_lwr1=CI[2],
                          CI_upr1=CI[5],
                          CI_lwr2=CI[3],
                          CI_upr2=CI[6],
                          vif_snow=car::vif(mod1)[1],
                          vif_temp=car::vif(mod1)[2])
      df_summary_all_air<-bind_rows(df_summary_all_air,df_temp)
    }
    #plot(mod1)
    #}
    #} 
    
    
    if(length(df8sub$Year)<5){
      df_temp<-data.frame(SpeciesID=df8sub$SpeciesID[1],
                          Plot=df8sub$Plot[1],
                          Pheno_event="Peak",
                          Slope1=NA,
                          Slope2=NA,
                          SE1=NA,
                          SE2=NA,
                          Tvalue1=NA,
                          Tvalue2=NA,
                          Pvalue1=NA,
                          Pvalue2=NA,
                          Count=NA,
                          n=NA,
                          AIC=NA,
                          Rsquared=NA,
                          Residual=NA,
                          CI_lwr1=NA,
                          CI_upr1=NA,
                          CI_lwr2=NA,
                          CI_upr2=NA,
                          vif_snow=NA,
                          vif_temp=NA)
    }
    
    else{ 
      mod2 <- lm(Peak ~ Snowmelt+Peak_Temp_50, data=df8sub)
      AIC2 <- AIC(mod1)
      R.mod2 <- rsq(mod2, adj = TRUE)
      Residual2 <- sqrt(deviance(mod2)/df.residual(mod2))
      CI2 <- confint(mod2, level=0.95)
      df_temp<-data.frame(SpeciesID=df8sub$SpeciesID[1],
                          Plot=df8sub$Plot[1],
                          Pheno_event="Peak",
                          Slope1=summary(mod2)$coefficients[2],
                          Slope2=summary(mod2)$coefficients[3],
                          SE1=summary(mod2)$coefficients[5],
                          SE2=summary(mod2)$coefficients[6],
                          Tvalue1=summary(mod2)$coefficients[8],
                          Tvalue2=summary(mod2)$coefficients[9],
                          Pvalue1=summary(mod2)$coefficients[11],
                          Pvalue2=summary(mod2)$coefficients[12],
                          Count=sum(df8sub$TotalAbundance),
                          n=sum(!is.na(df8sub$Peak)),
                          AIC=AIC2,
                          Rsquared=R.mod2,
                          Residual=Residual2,
                          CI_lwr1=CI2[2],
                          CI_upr1=CI2[5],
                          CI_lwr2=CI2[3],
                          CI_upr2=CI2[6],
                          vif_snow=car::vif(mod2)[1],
                          vif_temp=car::vif(mod2)[2])
      df_summary_all_air<-bind_rows(df_summary_all_air,df_temp)
    }
    
    #plot(mod2)
    
    if(length(df8sub$Year)<5){
      df_temp<-data.frame(SpeciesID=df8sub$SpeciesID[1],
                          Plot=df8sub$Plot[1],
                          Pheno_event="End",
                          Slope1=NA,
                          Slope2=NA,
                          SE1=NA,
                          SE2=NA,
                          Tvalue1=NA,
                          Tvalue2=NA,
                          Pvalue1=NA,
                          Pvalue2=NA,
                          Count=NA,
                          n=NA,
                          AIC=NA,
                          Rsquared=NA,
                          Residual=NA,
                          CI_lwr1=NA,
                          CI_upr1=NA,
                          CI_lwr2=NA,
                          CI_upr2=NA,
                          vif_snow=NA,
                          vif_temp=NA)
    }
    
    else{ 
      
      mod3 <- lm(End ~ Snowmelt+End_Temp_50, data=df8sub)
      AIC3 <- AIC(mod3)
      R.mod3 <- rsq(mod3, adj = TRUE)
      Residual3 <- sqrt(deviance(mod3)/df.residual(mod3))
      CI3 <- confint(mod3, level=0.95)
      df_temp<-data.frame(SpeciesID=df8sub$SpeciesID[1],
                          Plot=df8sub$Plot[1],
                          Pheno_event="End",
                          Slope1=summary(mod3)$coefficients[2],
                          Slope2=summary(mod3)$coefficients[3],
                          SE1=summary(mod3)$coefficients[5],
                          SE2=summary(mod3)$coefficients[6],
                          Tvalue1=summary(mod3)$coefficients[8],
                          Tvalue2=summary(mod3)$coefficients[9],
                          Pvalue1=summary(mod3)$coefficients[11],
                          Pvalue2=summary(mod3)$coefficients[12],
                          Count=sum(df8sub$TotalAbundance),
                          n=sum(!is.na(df8sub$End)),
                          AIC=AIC3,
                          Rsquared=R.mod3,
                          Residual=Residual3,
                          CI_lwr1=CI3[2],
                          CI_upr1=CI3[5],
                          CI_lwr2=CI3[3],
                          CI_upr2=CI3[6],
                          vif_snow=car::vif(mod3)[1],
                          vif_temp=car::vif(mod3)[2])
      df_summary_all_air<-bind_rows(df_summary_all_air,df_temp)
      
    }
  }
}

df_summary_all_air <- subset(df_summary_all_air,SpeciesID!="Scathophagidae")

df_summary_all_air <- subset(df_summary_all_air,SpeciesID!="Lygaeidae")

write_xlsx(df_summary_all_air, "Data\\df_summary_all_linear_50_day.xlsx", col_names = TRUE)




