library(tidyverse)
library(tidyr)
library(RDocumentation)#Denne gør din help() funktion bedre
library(readxl) 
library(lubridate) #Beregner datoer og tidspunkter. Det er en toolbox, eller en række tools der hjælper med at fremstille tid/datoer bedre. 
library(mgcv) #funktioner der kan analysere med GAM og generalised additive mixed modeling.
library(MESS) #teste antagelser i GAM, statistiske detaljer.
library(corrplot)
library(writexl)


df_additional <- read_excel("Data/phenology_data/df_2021_2022_2023.xlsx")

df3 <- read_excel("Data/phenology_data/df3_clean_data.xlsx")

#Match corrected df3 with uncorrected df_2010 before manually correcting for mites and spider abundances
df3 <- rbind(df3,df_additional)

#Need to reorder rows
df3 %>%
  arrange(Year) -> df3_new


#Data has been manually checked.
#Read new excel file with corrected mite data

#df3 <- read_excel("Data/phenology_data/Pre_data/df3_new.xlsx")

#### ACARI CORRECTION####

#Check which traps Bumblebees has been caught
df1c <- subset(df1, Genus=="Bombus")
df1c%>%
  group_by(Year,Plot,Month,DOY)%>%
  summarise(DaysA=mean(DaysA),DaysB=mean(DaysB),DaysC=mean(DaysC),DaysD=mean(DaysD),
            DaysE=mean(DaysE),DaysF=mean(DaysF),DaysG=mean(DaysG),DaysH=mean(DaysH),
            A=sum(A),B=sum(B),C=sum(C),D=sum(D),E=sum(E),F=sum(F),G=sum(G),H=sum(H))->df1d
df1d%>%
  group_by(Year,Plot,Month,DOY)%>%
  summarise(Sum_Abundance = A+B+C+D+E+F+G+H,
            Trapdays = DaysA+DaysB+DaysC+DaysD+DaysE+DaysF+DaysG+DaysH) -> df1d

df1c$Sum_Abundance <- (df1d$Sum_Abundance[match(paste0(df1c$Year,df1c$Plot, df1c$Month, df1c$DOY),paste0(df1d$Year,df1d$Plot, df1d$Month, df1d$DOY))])

df1c <- subset(df1c,Sum_Abundance!=0)

df1_acari <- subset(df1, SpeciesID == "Acari")
df1_acari <- subset(df1_acari, select = -c(E, F, G, H))
df1_acari%>%
  group_by(Year,Plot,Month,DOY)%>%
  summarise(DaysA=mean(DaysA),DaysB=mean(DaysB),DaysC=mean(DaysC),DaysD=mean(DaysD),
            DaysE=mean(DaysE),DaysF=mean(DaysF),DaysG=mean(DaysG),DaysH=mean(DaysH),
            A=sum(A),B=sum(B),C=sum(C),D=sum(D))->df1_acari

df1_acari%>%
  group_by(Year,Plot,Month,DOY)%>%
  summarise(Sum_Abundance = A+B+C+D,
            Trapdays = DaysA+DaysB+DaysC+DaysD+DaysE+DaysF+DaysG+DaysH) -> df1_acariSum

df1_acari%>%
  gather(key=Trap,value=Abundance,13:16) -> df2_Acari

df2_Acari%>%
  group_by(Year,Plot,Month,DOY)%>%
  summarise(Average = mean(Abundance), na.rm =TRUE) -> df1_acariAverage


df1c$A_acari <- (df1_acari$A[match(paste0(df1c$Year,df1c$Plot, df1c$Month, df1c$DOY),paste0(df1_acari$Year,df1_acari$Plot, df1_acari$Month, df1_acari$DOY))])
df1c$B_acari <- (df1_acari$B[match(paste0(df1c$Year,df1c$Plot, df1c$Month, df1c$DOY),paste0(df1_acari$Year,df1_acari$Plot, df1_acari$Month, df1_acari$DOY))])
df1c$C_acari <- (df1_acari$C[match(paste0(df1c$Year,df1c$Plot, df1c$Month, df1c$DOY),paste0(df1_acari$Year,df1_acari$Plot, df1_acari$Month, df1_acari$DOY))])
df1c$D_acari <- (df1_acari$D[match(paste0(df1c$Year,df1c$Plot, df1c$Month, df1c$DOY),paste0(df1_acari$Year,df1_acari$Plot, df1_acari$Month, df1_acari$DOY))])

df1c$Sum_Abundance_Acari <- (df1_acariSum$Sum_Abundance[match(paste0(df1c$Year,df1c$Plot, df1c$Month, df1c$DOY),paste0(df1_acariSum$Year,df1_acariSum$Plot, df1_acariSum$Month, df1_acariSum$DOY))])
df1c$Average_Abundance_Acari <- (df1_acariAverage$Average[match(paste0(df1c$Year,df1c$Plot, df1c$Month, df1c$DOY),paste0(df1_acariAverage$Year,df1_acariAverage$Plot, df1_acariAverage$Month, df1_acariAverage$DOY))])


df1c <- subset(df1c, select = -c(SpeciesID, General_remarks, Sorting_remarks, Field_remarks))
df1c <- subset(df1c, select = -c(E, F, G, H))

# Manually check data

#Correct for Lycosidae juvenile peaks
df1_lyco <- subset(df1, SpeciesID == "Lycosidae")

df1_lyco %>%
  group_by(Year, Plot, Month, DOY) %>%
  summarise(
    DaysA = mean(DaysA),
    DaysB = mean(DaysB),
    DaysC = mean(DaysC),
    DaysD = mean(DaysD),
    DaysE = mean(DaysE),
    DaysF = mean(DaysF),
    DaysG = mean(DaysG),
    DaysH = mean(DaysH),
    A = sum(A),
    B = sum(B),
    C = sum(C),
    D = sum(D),
    E = sum(E),
    F = sum(F),
    G = sum(G),
    H = sum(H)
  ) -> df1_lyco

#Easier to remove year 1996 as all samples are combined in trap A and so it is difficult to find spikes in abundance numbers
df1_lyco <- subset(df1_lyco, Year != 1996)

#Then subset years 1999 - 2006 as count data from all 8 traps are used
df_lycosub <- df1_lyco[which(df1_lyco$Year > 1998
                             & df1_lyco$Year < 2007),]

df1_lyco <- subset(df1_lyco, Year != 1999)
df1_lyco <- subset(df1_lyco, Year != 2000)
df1_lyco <- subset(df1_lyco, Year != 2001)
df1_lyco <- subset(df1_lyco, Year != 2002)
df1_lyco <- subset(df1_lyco, Year != 2003)
df1_lyco <- subset(df1_lyco, Year != 2004)
df1_lyco <- subset(df1_lyco, Year != 2005)
df1_lyco <- subset(df1_lyco, Year != 2006)


df1_lyco %>%
  group_by(Year, Plot, Month, DOY) %>%
  summarise(
    Sum_Abundance = A + B + C + D + E + F + G + H,
    Trapdays = DaysA + DaysB + DaysC + DaysD + DaysE + DaysF + DaysG +
      DaysH
  ) -> df1_lycoSum

df1_lyco <- subset(df1_lyco, select = -c(E, F, G, H))

df_lycosub %>%
  group_by(Year, Plot, Month, DOY) %>%
  summarise(
    Sum_Abundance = A + B + C + D + E + F + G + H,
    Trapdays = DaysA + DaysB + DaysC + DaysD + DaysE + DaysF + DaysG +
      DaysH
  ) -> df_lycoSumSub

df1_lyco %>%
  gather(key = Trap, value = Abundance, 13:16) -> df2_lyco

df_lycosub %>%
  gather(key = Trap, value = Abundance, 13:20) -> df2_lycosub


df2_lyco %>%
  group_by(Year, Plot, Month, DOY) %>%
  summarise(Average = mean(Abundance), na.rm = TRUE) -> df1_lycoAverage

df2_lycosub %>%
  group_by(Year, Plot, Month, DOY) %>%
  summarise(Average = mean(Abundance), na.rm = TRUE) -> df1_lycoSubAverage


df1_lyco$Sum_Abundance <-
  (df1_lycoSum$Sum_Abundance[match(
    paste0(df1_lyco$Year, df1_lyco$Plot, df1_lyco$Month, df1_lyco$DOY),
    paste0(
      df1_lycoSum$Year,
      df1_lycoSum$Plot,
      df1_lycoSum$Month,
      df1_lycoSum$DOY
    )
  )])
df1_lyco$Average_Abundance <-
  (df1_lycoAverage$Average[match(
    paste0(df1_lyco$Year, df1_lyco$Plot, df1_lyco$Month, df1_lyco$DOY),
    paste0(
      df1_lycoAverage$Year,
      df1_lycoAverage$Plot,
      df1_lycoAverage$Month,
      df1_lycoAverage$DOY
    )
  )])

df_lycosub$Sum_Abundance <-
  (df_lycoSumSub$Sum_Abundance[match(
    paste0(
      df_lycosub$Year,
      df_lycosub$Plot,
      df_lycosub$Month,
      df_lycosub$DOY
    ),
    paste0(
      df_lycoSumSub$Year,
      df_lycoSumSub$Plot,
      df_lycoSumSub$Month,
      df_lycoSumSub$DOY
    )
  )])
df_lycosub$Average_Abundance <-
  (df1_lycoSubAverage$Average[match(
    paste0(
      df_lycosub$Year,
      df_lycosub$Plot,
      df_lycosub$Month,
      df_lycosub$DOY
    ),
    paste0(
      df1_lycoSubAverage$Year,
      df1_lycoSubAverage$Plot,
      df1_lycoSubAverage$Month,
      df1_lycoSubAverage$DOY
    )
  )])

#Remove traps where 0 individuals have been caught
df1_lyco <- subset(df1_lyco, Sum_Abundance != 0)
df_lycosub <- subset(df_lycosub, Sum_Abundance != 0)
