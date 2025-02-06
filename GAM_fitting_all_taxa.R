####Script to calculate phenological events####

#Read clean data
df3 <- read_excel("Data/phenology_data/df3_updated.xlsx")

df3$Abundance <- as.numeric(df3$Abundance)

#df3$Abundance_corrected <- ifelse(df3$Year<2006,df3$Abundance/2,df3$Abundance)

#After calculating abundance some NA values may appear if the observations in
# the calculations only contained 0
#Therefore we make sure to convert all NA values to 0 again
#df3$Abundance_corrected[is.na(df3$Abundance_corrected)] <- 0

df3$Abundance[is.na(df3$Abundance)] <- 0

####CRITERIA IMPLEMENTED####

#The following criteria have to be met in order to model the seasonal activity
#1. Abundance > 50
#2. The taxa have to be sampled at least twice

#Column with Event created. If abundance is more than 0 it will be indicated with 1, otherwise 0.
#df3$Event<-ifelse(df3$Abundance_corrected>0,1,0)
df3$Event <- ifelse(df3$Abundance > 0, 1, 0)

df3$Month <- as.numeric(df3$Month)
class(df3$Abundance)

df3 %>%
  subset(Month > 5 & Month < 9) %>%
  group_by(SpeciesID, Plot, Year) %>%
  summarise(TotalAbundance = sum(Abundance),
            TotalEvents = sum(Event)) -> df2a

####Extra - Check total abundance for each family in every plot###

df3 %>%
  subset(Month > 5 & Month < 9) %>%
  group_by(SpeciesID, Plot) %>%
  summarise(TotalAbundance = sum(Abundance)) %>%
  spread(key = Plot, value = TotalAbundance) -> df2b



df2b %>%
  group_by(SpeciesID) %>%
  summarise(Sum = Art1 + Art2 + Art3 + Art4 + Art5 + Art6 + Art7) -> df2d

df2b$Sum <-
  (df2d$Sum[match(paste0(df2b$SpeciesID), paste0(df2d$SpeciesID))])
sum(df2b$Sum)

###############################################################################
#Abundance criteria 50 individuals
#df2a$TotalAbunAndEventCriteria<-ifelse(df2a$TotalAbundance>25&df2a$TotalEvents>2,1,0)
df2a$TotalAbunAndEventCriteria <-
  ifelse(df2a$TotalAbundance > 50 & df2a$TotalEvents > 2, 1, 0)

#df2a%>%
#group_by(SpeciesID,Year)%>%
#summarise(TotalYear=sum(TotalAbunAndEventCriteria))->df3a


#Calculating number of years where a family and plot has fulfilled criteria in 
#abundance and events
df2a%>%
  group_by(SpeciesID,Plot)%>%
  summarise(TotalYear=sum(TotalAbunAndEventCriteria))->df2c

#Overføre TotalYear kolonne til df2a
#df2a$TotalYear <- (df3a$TotalYear[match(paste0(df2a$SpeciesID,df2a$Year),paste0(df3a$SpeciesID,df3a$Year))])
#df3$TotalAbunAndEventCriteria <- (df2a$TotalAbunAndEventCriteria[match(paste0(df3$SpeciesID,df3$Year),paste0(df2a$SpeciesID,df2a$Year))])
df3$TotalYear <-
  (df2c$TotalYear[match(paste0(df3$SpeciesID, df3$Plot),
                        paste0(df2c$SpeciesID, df2c$Plot))])



#df3$YearThres<-ifelse(df3$TotalYear>0,1,0)
#df3$Include2<-0


#Calculation of Include column where all 3 criteria are fulfilled or not####
df2a$TotalEvents <- as.numeric(df2a$TotalEvents)
df2a$TotalAbundance <- as.numeric(df2a$TotalAbundance)

df2a$Include <-
  ifelse(df2a$TotalAbundance > 50 &
           df2a$TotalEvents > 2, 1, 0)#Need at least 50 individuals in a season and 3 capture events

#Filter original data for sampling criterias
df3$Include <-
  (df2a$Include[match(
    paste0(df3$SpeciesID, df3$Year, df3$Plot),
    paste0(df2a$SpeciesID, df2a$Year, df2a$Plot)
  )])#paste betyder at det er kombinationen af variable der skal matche.

#Include abundancePTD
df3$AbundancePTD <- (df3$Abundance / df3$Trapdays) * 10

#Visualize abundance calculations
df3 %>%
  subset(Month > 5 & Month < 9 & Include == 1 & Plot != "Art6") %>%
  group_by(SpeciesID, Plot, Year) %>%
  summarise(
    Abundance_corrected = sum(AbundancePTD),
    Trapdays = sum(Trapdays),
    AbundancePTD = Abundance / Trapdays
  ) %>%
  ggplot(aes(Year, Abundance_corrected)) + ylab("Abundance") +
  geom_smooth(method = "lm") + geom_point() + facet_grid(Plot ~ SpeciesID, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0))

df3 %>%
  subset(Month > 5 & Month < 9 & Include == 1 & Plot != "Art6") %>%
  group_by(SpeciesID, Plot, Year) %>%
  summarise(
    Abundance = sum(Abundance),
    Trapdays = sum(Trapdays),
    AbundancePTD = Abundance / Trapdays
  ) %>%
  ggplot(aes(Year, Abundance)) + ylab("Abundance") +
  geom_smooth(method = "lm") + geom_point() + facet_grid(Plot ~ SpeciesID, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0))


#AbundancePTD
df3 %>%
  subset(Month > 5 & Month < 9 & Include == 1 & Plot != "Art6") %>%
  group_by(SpeciesID, Plot, Year) %>%
  summarise(
    Abundance = sum(Abundance),
    Trapdays = sum(Trapdays),
    AbundancePTD = Abundance / Trapdays
  ) %>%
  ggplot(aes(Year, AbundancePTD)) + ylab("Abundance per trap day") +
  geom_smooth(method = "lm") + geom_point() + facet_grid(Plot ~ SpeciesID, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0))

#AbundancePTD plots as colours
df3 %>%
  subset(Month > 5 & Month < 9 & Include == 1 & Plot != "Art6") %>%
  group_by(SpeciesID, Plot, Year) %>%
  summarise(
    Abundance = sum(Abundance),
    Trapdays = sum(Trapdays),
    AbundancePTD = Abundance / Trapdays
  ) %>%
  ggplot(aes(Year, AbundancePTD, colour = Plot)) + ylab("Abundance per trap day") +
  #geom_smooth(method="lm")+
  geom_line() + facet_wrap( ~ SpeciesID, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0))

#Abundance plots as colours
df3 %>%
  subset(Month > 5 & Month < 9 & Include == 1 & Plot != "Art6") %>%
  group_by(SpeciesID, Plot, Year) %>%
  summarise(
    Abundance = sum(Abundance),
    Trapdays = sum(Trapdays),
    AbundancePTD = Abundance / Trapdays
  ) %>%
  ggplot(aes(Year, Abundance, colour = Plot)) + ylab("Abundance") +
  #geom_smooth(method="lm")+
  geom_line() + facet_wrap( ~ SpeciesID, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0))

#df3$Include<-ifelse(df3$TotalAbundanceYear>24&df3$TotalEventsYear>2,1,0)

#SpeciesID before plot
df4 <-df3 %>% 
  select(SpeciesID, everything())



####GAM fitting####
df6 <- data.frame(df4)
df5 <- select(df6, SpeciesID,Plot,Year,DOY,Abundance,AbundancePTD,Include,Event,TotalYear)
sum(is.na(df5$Include))#Tjek om der er nogle NA værdier
#df5$AbundancePTD <- (df5$Abundance/df5$Trapdays)
#df5$Include <- (df2a$Include[match(paste0(df4$SpeciesID,df4$Plot),paste0(df2a$SpeciesID,df2a$Plot))])
#summarise(Abundance = sum(Abundance),Trapdays=sum(Trapdays),AbundancePTD=Abundance/Trapdays)->df4

class(df5$Include)

df5$AbundancePTD[is.na(df5$AbundancePTD)] <- 0

#write.csv(df5, file = "Data/dataset_for_gam\\EMdata_final_AbundancePTD.csv", row.names=FALSE)
#row.names=FALSE fjerne første kolonne, da vi ellers får et problem med en kolonne X.


############################################################
#New data frame saved including the following columns:
#df5 = SpeciesID, Plot, Year, DOY, Abundance, Event, Include
#It is also possible to continue using df5, but the dataset
#is saved and reread so that you do not have to go through 
# all the above steps when revisiting the script
############################################################

#df7<- read.csv("ZAC_all.csv")
df7 <-
  read.csv2(
    "Data/dataset_for_gam/EMdata_final_AbundancePTD.csv",
    sep = ",",
    stringsAsFactors = FALSE,
    header = TRUE
  )

#df8<-subset(df7,select=-c(X))


df7$Year <- as.factor(df7$Year)#Easier to work with year as factor
#df7$SpeciesID<- as.factor(df7$SpeciesID)
#df7$Plot<- as.factor(df7$Plot)
df7$Abundance <- as.numeric(df7$Abundance)
class(df7$AbundancePTD)
#typeof(df7$Abundance)
df7$AbundancePTD <- as.numeric(df7$AbundancePTD)


df7$TotalYear <- as.numeric(df7$TotalYear)

#class(df7$Trapdays)
#df7$Trapdays<-as.integer(df7$Trapdays)
#class(df7$Trapdays)

#class(df7$AbundancePTD)
#df7$AbundancePTD<-as.numeric(df7$AbundancePTD)
#class(df7$AbundancePTD)

#Now the data is tested

df7 %>%
  group_by(SpeciesID, Plot, Year) %>%
  summarise(Include = max(Include)) -> df7temp
df7temp %>%
  group_by(SpeciesID, Plot) %>%
  summarise(Total_GAM = sum(Include)) -> df7GAM
df7include <- subset(df7GAM, Total_GAM >= 5)
#>= 10 eller 5 eller? Der er ingen Total GAM på 10..

#########unique(df7include$SpeciesID)

#THIS IS MAINLY FOR VISUAL PURPOSES#
#Creates PDF files with phen curves
df7$Abundance <- as.numeric(df7$Abundance)
class(df7$AbundancePTD)
class(df7$Abundance)
df7$DOY <- as.numeric(df7$DOY)
df7$AbundancePTD <- as.numeric(df7$AbundancePTD)


for (k in unique(df7$SpeciesID)) {
  dfsub <- subset(df7, SpeciesID == k)
  pdf(paste("Figures_PTD", k, ".pdf"),
      width = 20,
      height = 12)
  par(
    mfrow = c(7, 25),
    oma = c(5, 5, 4, 0) ,
    mar = c(2, 1, 2, 2) + 0.1
  ) #it goes c(bottom, left, top, right)
  for (i in unique(dfsub$Plot)) {
    for (j in unique(dfsub$Year)) {
      dfsuba <- subset(dfsub, Plot == i)
      dfsuba <- subset(dfsuba, Year == j)
      threshold <-
        length(dfsuba[dfsuba$AbundancePTD >= 1, 1]) # beregne antal events
      sumabund <- sum(dfsuba$Abundance)
      if (threshold <= 2 || sumabund < 50) {
        # skip gam if too few data
        plot(
          dfsuba$DOY,
          dfsuba$AbundancePTD,
          type = "p",
          main = j,
          ylim = c(0, 1.05 * max(
            1, max(dfsuba$AbundancePTD, na.rm = TRUE)
          )),
          xlim = c(154, 238)
        )#Det er de åbne symboler der ikke når threshold.
      }
      else{
        g <-
          gam(
            round(AbundancePTD, 2) ~ s(DOY, k = 4),
            family = poisson(link = "log"),
            data = dfsuba
          )
        pred <-
          data.frame(DOY = seq(154, 238, by = 0.1)) # frame of model predictions: day 154 to 238.
        pred$AbundancePTD <-
          predict(g, newdata = pred, type = "response") # Get predicted values under new values of avgsr and trapdays.
        plot(
          pred$DOY,
          pred$AbundancePTD,
          type = "l",
          col = "black",
          lwd = 1,
          main = j,
          ylim = c(0, 1.05 * max(c(
            max(pred$AbundancePTD, na.rm = TRUE),
            max(dfsuba$AbundancePTD, na.rm = TRUE)
          ))),
          xlim = c(154, 238)
        )
        points(dfsuba$DOY,
               dfsuba$AbundancePTD,
               pch = 16,
               cex = 1.5)#closed symbols indicating years where data criteria
        # are met
      }
    }
  }
  dev.off()
}


#length(df_test[df_test$AbundancePTD >= 1, 1])

####################
# Four functions (phenodate,phenogam,modpred,EM) to calculate
# "Onset", "Peak" og "End". Calculated as 10%, 50% og 90% of arthropod taxa 
# in each plot caught in a season
####################

class(df7$AbundancePTD)

df7$Include <- as.numeric(df7$Include)
df7$Event <- as.numeric(df7$Event)

#### phenodate --- FUNCTION ####
phenodate <- function(mod, em.level)
{
  if (min(mod$prop) > em.level) {
    result <- NA
  }
  else{
    lower <- (max(mod$prop[mod$prop <= em.level]))
    upper <- (min(mod$prop[mod$prop > em.level]))
    lower_doy <- mod$DOY[which(mod$prop == lower)]
    upper_doy <- mod$DOY[which(mod$prop == upper)]
    result <-
      ((em.level - lower) * ((upper_doy - lower_doy) / (upper - lower)) + lower_doy)
  }
  return(result)
}
#This function interpolates to the date equal to a given fraction (em.level) of 
#the total seasonal capture rate
#em.level: onset=0.10, peak=0.50, end=0.90


#### phenogam --- FUNCTION ####
phenogam <- function(SpeciesID, Plot, Year, data = df7) {
  y <- data[data$SpeciesID == SpeciesID &
              data$Plot == Plot &
              data$Year == Year,]
  if (nrow(y) == 0) {
    print(paste("No data for SpeciesID:", SpeciesID, "Plot:", Plot, "Year:", Year))
    return(list(NA))
  }
  event <- length(y[y$Abundance >= 1, 1])
  cum.abundance <- sum(y$Abundance)
  if (event <= 2 || cum.abundance <= 49) {
    print(paste("Insufficient data for SpeciesID:", SpeciesID, "Plot:", Plot, "Year:", Year))
    return(list(NA))
  }
  tryCatch({
    g <- gam(round(AbundancePTD, 0) ~ s(DOY, k = 4), family = poisson(link = "log"), data = y)
    return(g)
  }, error = function(e) {
    print(paste("GAM error for SpeciesID:", SpeciesID, "Plot:", Plot, "Year:", Year))
    return(list(NA))
  })
}

#Estimates a non-linear GAM function to describe seasondynamics for a species in 
#a plot in a year. k describes number of dimensions used in the function


#### modpred --- FUNCTION ####
modpred <- function(SpeciesID, Plot, Year, data = df7)
{
  y <- data[data$SpeciesID == SpeciesID &
              data$Plot == Plot &
              data$Year == Year,] # Extracting data from species, plot, year combination
  phenogam.result <-
    phenogam(SpeciesID, Plot, Year) # Get the gam-object
  pred <-
    data.frame(DOY = seq(154, 238, by = 0.1)) # frame of model predictions: day 154 to 238.
  if (length(phenogam.result) <= 1)
  {
    result <-
      rep(NA, 3)
  } # Assign NA if no gam model, three times because in table there is 
  #three(Onset, peak, end) for each combination of SpeciesID, Plot, Year.
  else
    # Give gam model prediction if there is a gam model
  {
    pred$AbundancePTD <-
      predict(phenogam.result, newdata = pred, type = "response") # Get predicted 
    #values under new values of avgsr and trapdays.
    pred$csum <- ave(pred$AbundancePTD, FUN = cumsum)
    pred$prop <- pred$csum / max(pred$csum)
    result <-
      c(
        Onset = phenodate(mod = pred, 0.1),
        Peak = phenodate(mod = pred, 0.5),
        End = phenodate(mod = pred, 0.9)
      )
  }
  return(result)
} #return predict table with predicted values.
#Calculates seasondynamics per 0.1 day from the GAM function estimated in 
#phenogam and calculates onset, peak and end


print(df7$SpeciesID)
print(levels(df7$SpeciesID))
#### EM --- FUNKTION ####
EM <- sapply(levels(as.factor(df7$SpeciesID)), function(SpeciesID) {
  print(SpeciesID)
  #print(levels(as.factor(df7$Plot)))
  sapply(levels(as.factor(df7$Plot)), function(Plot) {
    #print("Plot")
    sapply(levels(as.factor(df7$Year)), function(Year) {
      #print("Year")
      ope.liste <-
        c(modpred(
          SpeciesID = SpeciesID,
          Plot = Plot,
          Year = Year
        ))
      return(ope.liste)
    })
  })
})
# ope.liste = onset, peak, end, listen.
#Returns a list with results, but takes a while to run!


#Create list to dataframe
dfEM <- as.data.frame(EM)
dfEM$Pheno.Event <- rep(c("Onset", "Peak", "End"), 196) #25*7=175
dfEM$Year <-
  rep(c(seq(1996, 2023, by = 1)), each = 3, times =
        7)
dfEM$Plot <-
  rep(c("Art1", "Art2", "Art3", "Art4", "Art5", "Art6", "Art7"), each = 84)#25*3=75

#All data for families are piled
dfOPE <-
  gather(dfEM,
         key = SpeciesID,
         value = DOY,
         1:74,
         -Year,
         -Plot,
         -Pheno.Event)

df7 %>%
  group_by(SpeciesID, Plot, Year) %>%
  summarise(
    TotalAbundance = sum(Abundance),
    MeanAbundance = mean(Abundance, na.rm = T)
  ) -> df7a

dfOPE$TotalAbundance <-
  df7a$TotalAbundance[match(
    paste0(dfOPE$SpeciesID, dfOPE$Year, dfOPE$Plot),
    paste0(df7a$SpeciesID, df7a$Year, df7a$Plot)
  )]

dfOPE$TotalAbundance[is.na(dfOPE$TotalAbundance)] <- 0

length(dfEM)
#OPE = Onset, Peak, End
#write.csv(df5, file = "Data/Dataset_for_GAM\\EMdata_final.csv", row.names=FALSE)
#write.csv(dfOPE, file = "Data/Dataset_for_GAM_NEW\\dfOPE_dataframe_ptd.csv", row.names=FALSE)
#write.table(dfOPE, file = "OPE_liste.txt", sep = "\t")
#dfOPE<-read.csv2("Data/Dataset_for_GAM_NEW/dfOPE_dataframe_ptd.csv",sep=",",stringsAsFactors = FALSE, header = TRUE)

dfOPEsub <- subset(dfOPE, dfOPE$DOY != "NA", select = -Year)

class(dfOPEsub$DOY)
dfOPEsub$DOY <- as.numeric(dfOPEsub$DOY)
class(dfOPEsub$DOY)
#dfOPEsub[,"DOY", drop = FALSE] #To change dimensions
dfOPEsub %>%
  group_by(SpeciesID, Plot, Pheno.Event) %>%
  summarise(
    MinDOY = min(DOY),
    MeanDOY = mean(DOY),
    MaxDOY = max(DOY)
  ) -> dfOPEsub

write.table(dfOPEsub, file = "Data/Dataset_for_gam\\OPE_mmm_ptd.txt", sep = "\t")

sort(unique(dfOPE$SpeciesID))

#Columns with Onset, Peak, and End.
#Calculation of duration of arthropod activity
df8 <- dfOPE %>%
  spread(Pheno.Event, DOY)

df8$End <- as.numeric(df8$End)
df8$Onset <- as.numeric(df8$Onset)
df8$Peak <- as.numeric(df8$Peak)
df8$TotalAbundance <- as.numeric(df8$TotalAbundance)

df8$Duration <- (df8$End - df8$Onset)
df8 <- df8 %>%
  select(Year, Plot, SpeciesID, TotalAbundance, Onset, Peak, End, Duration)
df8sub <- subset(df8, df8$Duration != "NA")
df8$Year <- as.factor(df8$Year)
df8sub$Year <- as.factor(df8sub$Year)

#Lastly, all scripts are saved


write.csv(df8, file = "Data/Dataset_for_gam\\duration_ptd.csv", 
          row.names = FALSE)
write.csv(df8sub, 
          file = "Data/Dataset_for_gam\\duration_subset_ptd.csv", 
          row.names = FALSE)
write.table(df8sub,
            file = "Data/Dataset_for_gam\\duration_subset_ptd.txt",
            sep = "\t",
            row.names = FALSE)



############################################################
#Datasets saved which include:
#dfOPE = Pheno.Event, Year, Plot, SpeciesID, DOY.
#df8 =  Year, Plot, SpeciesID, Onset, Peak, End, Duration.
#df8sub = Year, Plot, SpeciesID, Onset, Peak, End, Duration. (No NA's)
############################################################



