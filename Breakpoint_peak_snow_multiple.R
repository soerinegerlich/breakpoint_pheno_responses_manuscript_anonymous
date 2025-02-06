library(tidyverse)
library(readxl)

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


######## Read the breakpoint model function ##########

bkpr <- function(formula,
                 bpv = "x",
                 data,
                 nsim = 1000,
                 minlength) {
  if (any(colnames(data) %in% c("fac", "nx", "y_simj"))) {
    stop("fac, nx and y_simj are reserved variables")
  }
  
  x <- data[, bpv]
  
  PBP <- sapply(
    FUN = function(p) {
      p >= x
    },
    unique(x)
  )
  # cut x above (TRUE) and below (FALSE) for every value
  
  #Need to restrict it so cuts can't be shorter than minlength
  unwanted <-
    c(which(colSums(PBP) <= minlength), which(colSums(PBP) > (length(x) - minlength)))
  PBP <- PBP[, -unwanted]
  
  m0 <- lm(formula, data = data)
  mnull <- update(m0, . ~ . + x)
  # fit the null model with common slope
  
  LL <- sapply(
    1:ncol(PBP),
    FUN = function(i) {
      data$fac <- PBP[, i]
      data$nx <- x - unique(x)[-unwanted][i]
      #centre the data at the breakpoint to generate nx. And allow for only one intercept
      logLik(update(m0, . ~ . + fac:nx))
    }
  )
  # fit models with all possible breakpoints and store likelihood
  
  MLBP <- unique(x)[-unwanted][which.max(LL)]
  # get breakpoint with highest likelihood
  
  data$fac <- PBP[, which.max(LL)]
  data$nx <- x - unique(x)[-unwanted][which.max(LL)]
  
  m1 <- update(m0, . ~ . + fac:nx)
  # fit best model
  
  y_sim <- simulate(mnull, nsim)
  # simulate data under the null model
  
  LLdiff <- 1:nsim
  slopediff <- 1:nsim
  slope1 <- 1:nsim
  
  for (j in 1:nsim) {
    data$y_simj <- y_sim[, j]
    LLsim <- sapply(
      1:ncol(PBP),
      FUN = function(i) {
        data$fac <- PBP[, i]
        data$nx <- x - unique(x)[-unwanted][i]
        logLik(update(m0, y_simj ~ . + fac:nx))
      }
    )
    ####################
    
    slopediffsim <- sapply(
      1:ncol(PBP),
      FUN = function(i) {
        data$fac <- PBP[, i]
        data$nx <- x - x[-unwanted][i]
        coef(update(m0, y_simj ~ . + fac:nx))["facFALSE:nx"]
        
      }
    )
    
    slope1sim <- sapply(
      1:ncol(PBP),
      FUN = function(i) {
        data$fac <- PBP[, i]
        data$nx <- x - x[-unwanted][i]
        coef(update(m0, y_simj ~ . + fac:nx))["x"]
        
      }
    )
    
    
    LLdiff[j] <- max(LLsim) - logLik(update(mnull, y_simj ~ .))
    slopediff[j] <- slopediffsim [which(LLsim == max(LLsim))]
    slope1[j] <- slope1sim [which(LLsim == max(LLsim))]
    # calculate the difference between the likelihoods on simulated data
  }
  return(
    list(
      model = m1,
      meanslope1 = mean(slope1),
      se.slope1 = sd(slope1),
      meanslopediff = mean(slopediff),
      se.slopediff = sd(slopediff),
      breakpoint = MLBP,
      pval = 1 - sum((logLik(m1) - logLik(mnull)) > LLdiff) / nsim,
      model_se = summary(m1)$coefficients[, "Std. Error"],
      ci.slope = confint(m1, level = 0.95),
      AIC_0 = AIC(m0),
      AIC_1 = AIC(m1),
      OriginalSlope = summary(m0)$coefficients[2, "Estimate"],
      OriginalSE = summary(m0)$coefficients[2, "Std. Error"]
    )
  )
}




######## Collect output from model in summary table ########

#colnames(df_phen_event)[9]<-"temperature"
colnames(df_phen_event)[5] <- "phenology"
colnames(df_phen_event)[10] <- "x"
colnames(df_phen_event)[8] <- "z"


df_summary_all <-
  data.frame(
    SpeciesID = character(),
    Plot = character(),
    Intercept = numeric(),
    Slope1 = numeric(),
    Slopediff = numeric(),
    Pvalue = numeric(),
    Break = numeric(),
    Meanslope = numeric(),
    Meanslopediff = numeric(),
    SEslope = numeric(),
    SEslopediff = numeric(),
    CI_lwr = numeric(),
    CI_upr = numeric(),
    n = numeric(),
    AIC_0 = numeric(),
    AIC_1 = numeric(),
    OriginalSlope = numeric(),
    OriginalSE = numeric()
  )

for (i in unique(df_phen_event$SpeciesID)) {
  print(i)
  df1 <- subset(df_phen_event, SpeciesID == i)
  #pdf(paste("Data/Figures_two_predictors\\Peak_temp",x,".pdf"),width=20,height=12)
  #par(mfrow=c(3,2),mar = c(4,5,4,10), oma = c(2,25,2,25)) #it goes c(bottom, left, top, right)
  for (j in unique(df1$Plot)) {
    print(j)
    df2 <- subset(df1, Plot == j)
    
    if (length(df2$Year) < 7) {
      
    }
    
    else{
      #temperature <- df2$temperature
      
      #bplocate<-sort(temperature)[5:(length(temperature) - 4)]
      #segment<-temperature>=bplocate
      #centretemp<-temperature-bplocate
      
      #phenology <- df2$phenology
      
      
      store.output <-
        bkpr(
          phenology ~ x + z,
          bpv = "x",
          data = df2,
          nsim = 100,
          minlength = 2
        )
      
      
      df_temp <- data.frame(
        SpeciesID = df2$SpeciesID[1],
        Plot = df2$Plot[1],
        Intercept = store.output$model$coefficients['(Intercept)'],
        Slope1 = store.output$model$coefficients['x'],
        Slopediff = store.output$model$coefficients['facFALSE:nx'],
        Pvalue = store.output$pval,
        Break = store.output$breakpoint,
        Meanslope = store.output$meanslope1,
        Meanslopediff = store.output$meanslopediff,
        SEslope = store.output$se.slope1,
        SEslopediff = store.output$se.slopediff,
        CI_lwr = store.output$ci.slope[2],
        CI_upr = store.output$ci.slope[3],
        AIC_0 = store.output$AIC_0,
        AIC_1 = store.output$AIC_1,
        n = sum(!is.na(df2$i)),
        OriginalSlope = store.output$OriginalSlope,
        OriginalSE = store.output$OriginalSE
      )
      df_summary_all <- bind_rows(df_summary_all, df_temp)
      
      #storep[j]<-store.output$pval
      #storeintercept[j]<-store.output$model$coefficients['(Intercept)']
      #storeslope1[j]<-store.output$model$coefficients['x']
      #storeslopediff[j]<-store.output$model$coefficients['facFALSE:nx']
      #storebreak[j]<-store.output$breakpoint
      #storemeanslope1[j]<-store.output $meanslope1
      #storemeanslopediff[j]<-store.output $meanslopediff
      #storeseslope1[j]<-store.output$se.slope1
      #storeseslopediff[j]<-store.output$se.slopediff
      #print(c(storep[j],storeslope1[j], storeseslope1[j],storeslopediff[j],storebreak[j],storemeanslopediff[j],storeseslopediff[j]))
      
      
      #breakval<-store.output$breakpoint
      #segments<-temperature > breakval
      
      #plot(temperature,phenology,col=as.factor(segments),pch=16,size = 10)
      #coefs<-store.output$model$coefficients
      #seg1<-c(min(temperature),breakval)
      #seg2<-c(store.output$breakpoint,max(temperature))
      #points(seg1,coefs["(Intercept)"]+coefs["x"]*seg1,type="l",size = 8)
      #points(seg2,(coefs["(Intercept)"]+coefs["x"]*seg1)[2]+(coefs["x"]+coefs["facFALSE:nx"])*(seg2-breakval),type="l",col=2,size = 8)
      
    }
  }
  #dev.off()
}


#require(writexl)
write_xlsx(df_summary_all, "Data/Summary_tables\\df_summary_peak_snow_temp_new.xlsx", col_names = TRUE)


######## Read summary excel file to do a bit of data clean up #########


df_summary_all_peak <-
  read_xlsx("Data/Summary_tables/df_summary_peak_snow_temp_new.xlsx")

length(which(df_summary_all_peak$Pvalue < 0.05)) / length(df_summary_all_peak$Pvalue)

df_summary_all_peak <-
  subset(
    df_summary_all_peak,
    df_summary_all_peak$Plot != "Art1" |
      df_summary_all_peak$SpeciesID != "Collembola"
  )
df_summary_all_peak <-
  subset(
    df_summary_all_peak,
    df_summary_all_peak$Plot != "Art1" |
      df_summary_all_peak$SpeciesID != "Acari"
  )

#df_summary_all$Slope2 <- df_summary_all$Slope1 + df_summary_all$Slopediff
#df_summary_all$SEslope2 <- df_summary_all$SEslopediff - df_summary_all$SEslope

df_summary_all_peak %>%
  mutate(
    Order = case_when(
      SpeciesID == "Acari" ~ "Decomposer",
      SpeciesID == "ANMU" ~ "Pollinator",
      SpeciesID == "Aphidoidea" ~ "Herbivore",
      SpeciesID == "Chalcidoidea" ~ "Pollinator",
      SpeciesID == "CHCE" ~ "Pollinator",
      SpeciesID == "Coccoidea" ~ "Herbivore",
      SpeciesID == "Collembola" ~ "Decomposer",
      SpeciesID == "Culicidae" ~ "Pollinator",
      SpeciesID == "Ichneumonidae" ~ "Parasitoid",
      SpeciesID == "Linyphiidae" ~ "Predator",
      SpeciesID == "Lycosidae" ~ "Predator",
      SpeciesID == "MYSC" ~ "Pollinator",
      SpeciesID == "Nymphalidae" ~ "Pollinator",
      SpeciesID == "Phoridae" ~ "Pollinator",
      SpeciesID == "Thomisidae" ~ "Predator"
    )
  ) -> df_summary_all_peak


df_summary_all_peak %>%
  mutate(
    Habitat = case_when(
      Plot == "Art1" ~ "Pond",
      Plot == "Art2" ~ "Wet fen",
      Plot == "Art3" ~ "Mesic heath",
      Plot == "Art4" ~ "Mesic heath",
      Plot == "Art5" ~ "Arid heath",
      Plot == "Art6" ~ "Snowbed",
      Plot == "Art7" ~ "Arid heath"
    )
  ) -> df_summary_all_peak

df_summary_all_peak$Plot[df_summary_all_peak$Plot == "Art1"] <-
  "Plot 1"
df_summary_all_peak$Plot[df_summary_all_peak$Plot == "Art2"] <-
  "Plot 2"
df_summary_all_peak$Plot[df_summary_all_peak$Plot == "Art3"] <-
  "Plot 3"
df_summary_all_peak$Plot[df_summary_all_peak$Plot == "Art4"] <-
  "Plot 4"
df_summary_all_peak$Plot[df_summary_all_peak$Plot == "Art5"] <-
  "Plot 5"
df_summary_all_peak$Plot[df_summary_all_peak$Plot == "Art6"] <-
  "Plot 6"
df_summary_all_peak$Plot[df_summary_all_peak$Plot == "Art7"] <-
  "Plot 7"


df_summary_all_peak$SpeciesID[df_summary_all_peak$SpeciesID == "CHCE"] <-
  "Chironomidae"
df_summary_all_peak$SpeciesID[df_summary_all_peak$SpeciesID == "ANMU"] <-
  "Muscidae"
df_summary_all_peak$SpeciesID[df_summary_all_peak$SpeciesID == "MYSC"] <-
  "Sciaridae"

write_xlsx(df_summary_all_peak, "Data/Summary_tables\\df_summary_peak_snow_temp_final_new.xlsx", col_names = TRUE)
