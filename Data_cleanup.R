#From raw data to clean data

#install.packages(c("tidyverse", "tidyr", "RDocumentation", "readxl", "lubridate", 
#"mgcv", "MESS", "corrplot", "writexl"))
library(tidyverse)
library(tidyr)
library(RDocumentation)
library(readxl) 
library(lubridate)  
library(mgcv) 
library(MESS) 
library(corrplot)
library(writexl)
library(dplyr)

##### Zackenberg raw data - downloaded 14 September 2022 #####

#Read txt file
df1 <- read.table("Data/arthropod_emergence/arthropod_emergence_raw_data.txt", header = TRUE, sep = "\t")

#Rename columns.
#No space between Days and letter
df1 = df1 %>% rename(
  "DaysA" = "Days.A",
  "DaysB" = "Days.B",
  "DaysC" = "Days.C",
  "DaysD" = "Days.D",
  "DaysE" = "Days.E",
  "DaysF" = "Days.F",
  "DaysG" = "Days.G",
  "DaysH" = "Days.H"
)
#df1 = df1 %>% rename("Plot"="Plot.ID", "Date"="ï..Date") #Fejl vises ved Date.
df1 = df1 %>% rename("Plot" = "Plot.ID")

#Replace all Days rows which includes -9999 with NA. Just means that there is no snow
df1$DaysA[df1$DaysA == -9999] <- NA #Replace '-9999' with 'NA'
df1$DaysB[df1$DaysB == -9999] <- NA #Replace '-9999' with 'NA'
df1$DaysC[df1$DaysC == -9999] <- NA #Replace '-9999' with 'NA'
df1$DaysD[df1$DaysD == -9999] <- NA #Replace '-9999' with 'NA'
df1$DaysE[df1$DaysE == -9999] <- NA #Replace '-9999' with 'NA'
df1$DaysF[df1$DaysF == -9999] <- NA #Replace '-9999' with 'NA'
df1$DaysG[df1$DaysG == -9999] <- NA #Replace '-9999' with 'NA'
df1$DaysH[df1$DaysH == -9999] <- NA #Replace '-9999' with 'NA'

#Same with abundance columns but we replace -9999 and -999 with 0 instead in
# order to use it for calculations later
df1$A[df1$A == -9999] <- 0 #Replace '-9999' with zero
df1$B[df1$B == -9999] <- 0 #Replace '-9999' with zero
df1$C[df1$C == -9999] <- 0 #Replace '-9999' with zero
df1$D[df1$D == -9999] <- 0 #Replace '-9999' with zero
df1$E[df1$E == -9999] <- 0 #Replace '-9999' with zero
df1$F[df1$F == -9999] <- 0 #Replace '-9999' with zero
df1$G[df1$G == -9999] <- 0 #Replace '-9999' with zero
df1$H[df1$H == -9999] <- 0 #Replace '-9999' with zero

df1$A[df1$A == -999] <- 0 #Replace '-9999' with zero
df1$B[df1$B == -999] <- 0 #Replace '-9999' with zero
df1$C[df1$C == -999] <- 0 #Replace '-9999' with zero
df1$D[df1$D == -999] <- 0 #Replace '-9999' with zero
df1$E[df1$E == -999] <- 0 #Replace '-9999' with zero
df1$F[df1$F == -999] <- 0 #Replace '-9999' with zero
df1$G[df1$G == -999] <- 0 #Replace '-9999' with zero
df1$H[df1$H == -999] <- 0 #Replace '-9999' with zero

#Some places is written NA but this is added by observers and needs to be 
# converted to 0 for later calculations
df1[df1=="NA"] <- NA
df1$A[is.na(df1$A)] <- 0 #Replace 'na' with zero
df1$B[is.na(df1$B)] <- 0 #Replace 'na' with zero
df1$C[is.na(df1$C)] <- 0 #Replace 'na' with zero
df1$D[is.na(df1$D)] <- 0 #Replace 'na' with zero
df1$E[is.na(df1$E)] <- 0 #Replace 'na' with zero
df1$F[is.na(df1$F)] <- 0 #Replace 'na' with zero
df1$G[is.na(df1$G)] <- 0 #Replace 'na' with zero
df1$H[is.na(df1$H)] <- 0 #Replace 'na' with zero

df1$DaysA[is.na(df1$DaysA)] <- 0 #Replace 'na' with zero
df1$DaysB[is.na(df1$DaysB)] <- 0 #Replace 'na' with zero
df1$DaysC[is.na(df1$DaysC)] <- 0 #Replace 'na' with zero
df1$DaysD[is.na(df1$DaysD)] <- 0 #Replace 'na' with zero
df1$DaysE[is.na(df1$DaysE)] <- 0 #Replace 'na' with zero
df1$DaysF[is.na(df1$DaysF)] <- 0 #Replace 'na' with zero
df1$DaysG[is.na(df1$DaysG)] <- 0 #Replace 'na' with zero
df1$DaysH[is.na(df1$DaysH)] <- 0 #Replace 'na' with zero

#New columns with DOY, week number and year
df1$DOY<-yday(ymd(df1$Date)) #Add Day of year variable
df1$Month<-month(df1$Date) # Add month variable as factor.
df1$Year<-year(df1$Date) # Add year variable as a factor.

#Change Days E,F,G,H to zero after 2006 trapdays are recorded, 
#but samples not processed
#From 2006 traps are only sorted from A - D 
df1 <- within(df1, DaysE[Year>2006] <- 0)
df1 <- within(df1, DaysF[Year>2006] <- 0)
df1 <- within(df1, DaysG[Year>2006] <- 0)
df1 <- within(df1, DaysH[Year>2006] <- 0)


#Change to numeric
df1$A <- as.numeric(df1$A)
df1$B <- as.numeric(df1$B)
df1$C <- as.numeric(df1$C)
df1$D <- as.numeric(df1$D)
df1$E <- as.numeric(df1$E)
df1$F <- as.numeric(df1$F)
df1$G <- as.numeric(df1$G)
df1$H <- as.numeric(df1$H)

#Combine taxonomic information into one column
df1$SpeciesID <- as.character(df1$Family)
df1$SpeciesID[is.na(df1$SpeciesID)] <-
  as.character(df1$Order[is.na(df1$SpeciesID)])
df1$SpeciesID[is.na(df1$SpeciesID)] <-
  as.character(df1$Phylum[is.na(df1$SpeciesID)])

#Adjust taxonomic names.
sort(unique(df1$SpeciesID))
df1$SpeciesID[df1$SpeciesID == "PieridaeÂ "] <- "Pieridae"
df1$SpeciesID[df1$SpeciesID == "TriopsidaeÂ "] <- "Triopsidae"

#gsub("TriopsidaeÂ","Triopsidae",df1$SpeciesID)
#gsub("PieridaeÂ ","Pieridae",df1$SpeciesID)

#str_replace_all(df1$SpeciesID,"Â","")

#Remove data which cannot be identified to family level
df1<- subset(df1,SpeciesID!="unidentified")
df1<- subset(df1,SpeciesID!="Brachycera larvae")
df1<- subset(df1,SpeciesID!="Cyclorrhapha larvae")
df1<- subset(df1,SpeciesID!="Lepidoptera larvae")
df1<- subset(df1,SpeciesID!="Diptera larvae")
df1<- subset(df1,SpeciesID!="Nematocera larvae")
df1<- subset(df1,SpeciesID!="Symphyta larvae")
df1<- subset(df1,SpeciesID!="Tipulidae larvae")
df1<- subset(df1,SpeciesID!="Hymenoptera larvae")


#print(unique(df1$SpeciesID))

###############################################################################

#Summarize to one value per SpeciesID,Year,Plot,DOY
df1 %>%
  group_by(SpeciesID, Year, Plot, Month, DOY) %>%
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
  ) -> df2


#Sum of abundance in all traps. Abundance not calculated till later on though
#df2$Abundance<-df2$A+df2$B+df2$C+df2$D+df2$E+df2$F+df2$G+df2$H
df2$Trapdays <-
  df2$DaysA + df2$DaysB + df2$DaysC + df2$DaysD + df2$DaysE + df2$DaysF +
  df2$DaysG + df2$DaysH
#df2$Abundance<-df2$A+df2$B+df2$C+df2$D+df2$E+df2$F+df2$G+df2$H

#Abundance test plots - Family level data. 

#Just to make sure all NAs are 0.
df2$A[is.na(df2$A)] <- 0
df2$B[is.na(df2$B)] <- 0
df2$C[is.na(df2$C)] <- 0
df2$D[is.na(df2$D)] <- 0
df2$E[is.na(df2$E)] <- 0
df2$F[is.na(df2$F)] <- 0
df2$G[is.na(df2$G)] <- 0
df2$H[is.na(df2$H)] <- 0

#ABUNDANCE CALCULATION

#Some of the families are combined as they were mixed together in the early years
df2 %>%
  group_by(SpeciesID, Year, Plot, Month, DOY, Trapdays) %>%
  summarise(Abundance = A + B + C + D + E + F + G + H) %>%
  within(Abundance[is.na(Abundance)] <- 0) %>%
  spread(key = SpeciesID, value = Abundance) %>%
  #pivot_wider(names_from = c(SpeciesID), values_from = c(Abundance))%>%
  within(Muscidae[is.na(Muscidae)] <- 0) %>%
  within(Anthomyiidae[is.na(Anthomyiidae)] <- 0) %>%
  within(Anthomyzidae[is.na(Anthomyzidae)] <- 0) %>%
  within(Chironomidae[is.na(Chironomidae)] <- 0) %>%
  within(Ceratopogonidae[is.na(Ceratopogonidae)] <- 0) %>%
  within(Mycetophilidae[is.na(Mycetophilidae)] <- 0) %>%
  within(Sciaridae[is.na(Sciaridae)] <- 0) %>%
  mutate(
    ANMU = Muscidae + Anthomyiidae + Anthomyzidae,
    CHCE = Chironomidae + Ceratopogonidae,
    MYSC = Mycetophilidae + Sciaridae
  ) %>%
  select(
    -c(
      Muscidae,
      Anthomyiidae,
      Anthomyzidae,
      Chironomidae,
      Ceratopogonidae,
      Mycetophilidae,
      Sciaridae
    )
  ) %>%
  gather(key = SpeciesID, value = Abundance, 6:73) -> df3

######INCORPORATING DATA FROM 2010########

#Data from 2021-2023 is now available and we need to extract this data in order 
# to correct the data according to mites and spiders
df_additional <- subset(df3,Year=="2021" | Year=="2022" | Year=="2023")
#write_xlsx(df_additional, "Data/phenology_data\\df_2021_2022_2023.xlsx", col_names = TRUE)

#### READ CORRECTED DATA ####

df_additional <- read_excel("Data/phenology_data/df_2021_2022_2023.xlsx")

df3 <- read_excel("Data/phenology_data/df3_clean_data.xlsx")

#Match corrected df3 with uncorrected df_2010 before manually correcting for mites and spider abundances
df3 <- rbind(df3,df_additional)

#Need to reorder rows
df3 %>%
  arrange(Year) -> df3_new

#Now create a new df3
write_xlsx(df3_new, "Data/phenology_data\\df3_updated.xlsx", col_names = TRUE)

#The manual corrections will be performed using the "Acari_lyco_correction" script
######SEE SCRIPT:2.Data_cleanup_Acari_lyco_correction#######

###############################################################################
# read updated dataframe
###############################################################################








