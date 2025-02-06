
#Nonlinearity in onset of activity to temperature

library(tidyverse)
library(readxl)
library(writexl)

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


df_phen_event%>%
  subset(!is.na(End_Temp)&!is.na(Onset)) -> df_phen_event

######## Read the breakpoint model function ##########

#Run the function
bkpr<-function(formula, bpv="x", data, nsim=1000,minlength){
  
  if(any(colnames(data)%in%c("fac", "nx", "y_simj"))){stop("fac, nx and y_simj are reserved variables")}  
  
  x<-data[,bpv]
  
  PBP<-sapply(FUN=function(p){p>=x},unique(x)) 
  # cut x above (TRUE) and below (FALSE) for every value 
  
  #Need to restrict it so cuts can't be shorter than minlength
  unwanted<-c(which(colSums(PBP)<=minlength),which(colSums(PBP)>(length(x)-minlength)))
  PBP<-PBP[,-unwanted]
  
  m0<-lm(formula, data=data)
  mnull<-update(m0, .~.+x)
  # fit the null model with common slope
  
  LL<-sapply(1:ncol(PBP),FUN=function(i){
    data$fac<-PBP[,i]
    data$nx<-x-unique(x)[-unwanted][i]
    #centre the data at the breakpoint to generate nx. And allow for only one intercept
    logLik(update(m0, .~.+fac:nx))
  })
  # fit models with all possible breakpoints and store likelihood
  
  MLBP<-unique(x)[-unwanted][which.max(LL)]
  # get breakpoint with highest likelihood
  
  data$fac<-PBP[,which.max(LL)]
  data$nx<-x-unique(x)[-unwanted][which.max(LL)]
  
  m1<-update(m0, .~.+fac:nx)
  # fit best model
  
  y_sim<-simulate(mnull,nsim)
  # simulate data under the null model
  
  LLdiff<-1:nsim
  slopediff<-1:nsim
  slope1<-1:nsim
  
  for(j in 1:nsim){
    data$y_simj<-y_sim[,j]
    LLsim<-sapply(1:ncol(PBP),FUN=function(i){
      data$fac<-PBP[,i]
      data$nx<-x-unique(x)[-unwanted][i]
      logLik(update(m0, y_simj~.+fac:nx))
    })
    ####################
    
    slopediffsim<-sapply(1:ncol(PBP),FUN=function(i){
      data$fac<-PBP[,i]
      data$nx<-x-x[-unwanted][i]
      coef(update(m0, y_simj~.+fac:nx))["facFALSE:nx"]
      
    })
    
    slope1sim<-sapply(1:ncol(PBP),FUN=function(i){
      data$fac<-PBP[,i]
      data$nx<-x-x[-unwanted][i]
      coef(update(m0, y_simj~.+fac:nx))["x"]
      
    })
    
    
    LLdiff[j]<-max(LLsim)-logLik(update(mnull, y_simj~.))
    slopediff[j]<-slopediffsim [which(LLsim==max(LLsim))]
    slope1[j]<-slope1sim [which(LLsim==max(LLsim))]
    # calculate the difference between the likelihoods on simulated data
  }
  return(list(model=m1,meanslope1=mean(slope1),se.slope1=sd(slope1),meanslopediff=mean(slopediff),se.slopediff=sd(slopediff), breakpoint=MLBP, pval=1-sum((logLik(m1)-logLik(mnull))>LLdiff)/nsim,model_se=summary(m1)$coefficients[,"Std. Error"],
              ci.slope=confint(m1, level = 0.95), AIC_0=AIC(m0), AIC_1=AIC(m1), OriginalSlope=summary(m0)$coefficients[2,"Estimate"], OriginalSE=summary(m0)$coefficients[2,"Std. Error"]))
}

####Try with all taxa in one plot

#df_test <- subset(df_phen_event_50, df_phen_event_50$SpeciesID == "ANMU")
#df_test <- subset(df_test, df_test$Plot == "Art4")

#lm <- lm(phenology ~ x, df_test)
#summary(lm)
#summary(lm)$coefficients[2,"Std. Error"]


######## Collect output from model in summary table ########

colnames(df_phen_event)[4]<-"phenology"
colnames(df_phen_event)[7]<-"x"

df_summary_all<-data.frame(SpeciesID=character(),Plot=character(),Intercept=numeric(),Slope1=numeric(),Slopediff=numeric(),
                           Pvalue=numeric(),Break=numeric(),Meanslope=numeric(),Meanslopediff=numeric(),SEslope=numeric(),SEslopediff=numeric(),
                           CI_lwr=numeric(), CI_upr=numeric(),n=numeric(), AIC_0=numeric(), AIC_1=numeric(), OriginalSlope = numeric(),
                           OriginalSE = numeric())
#Still need AIC, Rsq, CI



for(i in unique(df_phen_event$SpeciesID)){
  print(i)
  df1 <- subset(df_phen_event, SpeciesID == i)
  #pdf(paste("Data/Figures\\Figures_Onset_temp",i,".pdf"),width=20,height=12)
  #par(mfrow=c(3,2),mar = c(4,5,4,10), oma = c(2,25,2,25)) #it goes c(bottom, left, top, right) 
  for (j in unique(df1$Plot)) {
    print(j)
    df2 <- subset(df1, Plot == j)
    
    if (length(df2$Year) < 7) {
      
    }
    
    else{
      
      
      
      #bplocate<-sort(temperature)[5:(length(temperature) - 4)]
      #segment<-temperature>=bplocate
      #centretemp<-temperature-bplocate
      
      #phenology <- df2$phenology
      
      
      #temperature <- df2$x
      
      store.output<-bkpr(phenology~x, bpv="x", data=df2, nsim=100,minlength=2)
      
      df_temp<-data.frame(SpeciesID=df2$SpeciesID[1], 
                          Plot=df2$Plot[1],
                          Intercept=store.output$model$coefficients['(Intercept)'],
                          Slope1=store.output$model$coefficients['x'],
                          Slopediff=store.output$model$coefficients['facFALSE:nx'],
                          Pvalue=store.output$pval,
                          Break=store.output$breakpoint,
                          Meanslope=store.output$meanslope1,
                          Meanslopediff=store.output$meanslopediff,
                          SEslope=store.output$se.slope1,
                          SEslopediff=store.output$se.slopediff,
                          CI_lwr=store.output$ci.slope[2],
                          CI_upr=store.output$ci.slope[3],
                          AIC_0=store.output$AIC_0,
                          AIC_1=store.output$AIC_1,
                          n=sum(!is.na(df2$i)),
                          OriginalSlope=store.output$OriginalSlope,
                          OriginalSE=store.output$OriginalSE)
      df_summary_all<-bind_rows(df_summary_all,df_temp)
      
      
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
      
      #plot(temperature,phenology,col=as.factor(segments),pch=16, size = 10, main = c(df2$SpeciesID[1],df2$Plot[1]))
      #coefs<-store.output$model$coefficients
      #seg1<-c(min(temperature),breakval)
      #seg2<-c(store.output$breakpoint,max(temperature))
      #points(seg1,coefs["(Intercept)"]+coefs["x"]*seg1,type="l",size = 8)
      #points(seg2,(coefs["(Intercept)"]+coefs["x"]*seg1)[2]+(coefs["x"]+coefs["facFALSE:nx"])*(seg2-breakval),type="l",col=2,size = 8)
      
    }
  }
  #dev.off()
}

#dev.off()

unique(df_summary_all$SpeciesID)


#df_summary_all$Slope2 <- df_summary_all$Slope1 + df_summary_all$Slopediff

#df_summary_all <- df_summary_all[, c(1, 2, 3, 4, 5, 13, 6, 7, 8, 9, 10, 11, 12)]

#require(writexl)
#write_xlsx(df_summary_all, "Data/Summary_tables\\df_summary_temp_onset_new.xlsx", col_names = TRUE)


slope1 <- df_summary_all$Slope1

length(which(df_summary_all$Pvalue<0.05))/length(df_summary_all$Pvalue)
#This quantifies power. We'd hope for 0.8

par(mfrow=c(1,2))
hist(slope1,xlab="slope 1",breaks=20,main="")
abline(v= mean(df_summary_all$Slope1),col=1,lty=2)

hist(df_summary_all$Slopediff,xlab="slope difference",breaks=20,main="")
abline(v= mean(df_summary_all$Slopediff),col=1,lty=2)

plot(df_summary_all$Slopediff,(1/df_summary_all$SEslopediff))
#This shows the bias away from 0. 


require(readxl)
df_summary_temp <- read_xlsx("Data/Summary_tables/df_summary_temp_onset_new.xlsx")

length(which(df_summary_temp$Pvalue<0.05))/length(df_summary_temp$Pvalue)

df_summary_temp <-
  subset(
    df_summary_temp,
    df_summary_temp$Plot != "Art1" |
      df_summary_temp$SpeciesID != "Collembola"
  )
df_summary_temp <-
  subset(
    df_summary_temp,
    df_summary_temp$Plot != "Art1" |
      df_summary_temp$SpeciesID != "Acari"
  )


df_summary_temp %>%
  mutate(Order = case_when(
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
    SpeciesID == "Thomisidae" ~ "Predator")) -> df_summary_temp


df_summary_temp %>%
  mutate(Habitat = case_when(
    Plot == "Art1" ~ "Pond",
    Plot == "Art2" ~ "Wet fen",
    Plot == "Art3" ~ "Mesic heath",
    Plot == "Art4" ~ "Mesic heath",
    Plot == "Art5" ~ "Arid heath",
    Plot == "Art6" ~ "Snowbed",
    Plot == "Art7" ~ "Arid heath")) -> df_summary_temp

df_summary_temp$Plot[df_summary_temp$Plot == "Art1"] <- "Plot 1" 
df_summary_temp$Plot[df_summary_temp$Plot == "Art2"] <- "Plot 2" 
df_summary_temp$Plot[df_summary_temp$Plot == "Art3"] <- "Plot 3" 
df_summary_temp$Plot[df_summary_temp$Plot == "Art4"] <- "Plot 4" 
df_summary_temp$Plot[df_summary_temp$Plot == "Art5"] <- "Plot 5" 
df_summary_temp$Plot[df_summary_temp$Plot == "Art6"] <- "Plot 6"
df_summary_temp$Plot[df_summary_temp$Plot == "Art7"] <- "Plot 7" 


df_summary_temp$SpeciesID[df_summary_temp$SpeciesID == "CHCE"] <- "Chironomidae"
df_summary_temp$SpeciesID[df_summary_temp$SpeciesID == "ANMU"] <- "Muscidae"
df_summary_temp$SpeciesID[df_summary_temp$SpeciesID == "MYSC"] <- "Sciaridae"

write_xlsx(df_summary_temp, "Data/Summary_tables\\df_summary_onset_temp_final_new.xlsx", col_names = TRUE)


####Figure####


#Create new dataframe with mean values
Slopedifference <- -0.97
SlopediffSE <- 2.36
Slope1 <- -0.78
SlopeSE <- 1.56

df_mean <- as.data.frame(Slopedifference) 
df_mean$SlopediffSE <- SlopediffSE
df_mean$Slope1 <- Slope1
df_mean$SlopeSE <- SlopeSE

df_summary_temp$SpeciesID <- factor(df_summary_temp$SpeciesID,                 # Relevel group factor
                                    levels = c("Acari", "Collembola", "Coccoidea", "Aphidoidea", "Chalcidoidea", "Ichneumonidae", "Chironomidae", "Culicidae",
                                               "Muscidae", "Nymphalidae", "Phoridae", "Sciaridae", "Linyphiidae", "Lycosidae", "Thomisidae"))

Onset_temp <- ggplot(df_summary_temp)+
  geom_point(mapping=aes(Slope1, Slopediff, group = interaction(Plot,SpeciesID), shape = Habitat, color = SpeciesID),size = 6, alpha = 0.8)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  ylab("")+
  xlab("")+
  #ggtitle("Temperature as single predictor")+
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  scale_color_manual(values = c("darkseagreen3", "darkseagreen4", "chartreuse", "chartreuse3", "brown3", "brown4", "darkorange", "#FC4E07",
                                "darkgoldenrod1", "darkorange3", "yellow", "gold", "dodgerblue", "blue", "dodgerblue4"))+
  # geom_errorbar(aes(ymin=Slopediff-SEslopediff, ymax=Slopediff+SEslopediff), width=.3,
  # position=position_dodge(0.05), size = 1, alpha = 0.5)+
  #geom_errorbar(aes(xmin=Slope1-SEslope, xmax=Slope1+SEslope), width=.3,
  #position=position_dodge(0.05), size = 1, alpha = 0.5)+
  xlim(-50,50)+
  ylim(-50,50)+
  geom_point(data=df_mean, mapping=aes(Slope1, Slopedifference), size = 5, shape = 4, stroke = 3)+
  theme_bw()+
  theme(axis.text.x = element_text(face = "bold", size = 15, color = "black"), 
        axis.text.y = element_text(face = "bold", size = 15, color = "black"),
        axis.title.x = element_text(face = "bold", size = 20, color = "black", vjust = 0.5),
        axis.title.y = element_text(face = "bold", size = 20, color = "black"))


#Only significant breakpoints

df_summary_temp$Significance_level <- ifelse(df_summary_temp$Pvalue<0.06, "True", "False")

df_significant <- subset(df_summary_temp, Significance_level == "True")

Onset_temp <- ggplot(df_significant)+
  geom_point(mapping=aes(Slope1, Slopediff, group = interaction(Plot,SpeciesID), shape = Habitat, color = SpeciesID),size = 6, alpha = 0.8)+
  #geom_errorbar(data=df_significant,mapping=aes(ymin=Slopediff-SEslopediff, ymax=Slopediff+SEslopediff), width=.3,
                #position=position_dodge(0.05), size = 1, alpha = 0.5)+
  #geom_errorbar(data=df_significant,mapping=aes(xmin=Slope1-SEslope, xmax=Slope1+SEslope), width=.3,
                #position=position_dodge(0.05), size = 1, alpha = 0.5)+
  ylab("")+
  xlab("")+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  #ggtitle("Temperature as single predictor - Onset")+
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  scale_color_manual(values = c("darkseagreen3","brown4", "darkorange", "darkorange3", "blue"))+
  xlim(-50,50)+
  ylim(-50,50)+
  geom_point(data=df_mean, mapping=aes(Slope1, Slopedifference), size = 5, shape = 4, stroke = 3)+
  theme_bw()+
  theme(axis.text.x = element_text(face = "bold", size = 15, color = "black"), 
        axis.text.y = element_text(face = "bold", size = 15, color = "black"),
        axis.title.x = element_text(face = "bold", size = 20, color = "black", vjust = 0.5),
        axis.title.y = element_text(face = "bold", size = 20, color = "black"))

