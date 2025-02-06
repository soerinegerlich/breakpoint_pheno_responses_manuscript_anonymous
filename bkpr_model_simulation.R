#Code for for the breakpoint model to estimate a breakpoint and calculate 
# first slope and difference between slopes before and after a breakpoint. 

#The code is written by Jarrod Hadfield and Ally Phillimore (University of 
# Edinburgh)

#The issue with the model is that if there is a slope difference then the estimate
# will be inflated (i.e. further from 0). Slope differences are not biased
# under the null.


library(tidyr)
library(ggplot2)


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
      OriginalP = summary(m1)$coefficients[3, "Pr(>|t|)"]
    )
  )
}


####Running simulations####

simulate_slope<--5
simulate_slopediff<-5
#Under null slope diff would be 0

storep<-c()
storeporiginal<-c()
storeslope1<-c()
storeslopediff<-c()
storebreak<-c()
storemeanslope1<-c()
storemeanslopediff<-c()
storeseslope1<-c()
storeseslopediff<-c()
storeseslopedifforiginal<-c()


yearno<-28
#Number of years in dataset
	
	
#Run 1000 simulations but will take several days
for(x in 1:1000){
	
#simulate temperature variation
temperature<-rnorm(n=yearno,mean=4,sd=2)

#The simulations has been run with breakpoints at several different locations.
#Choose one!!

#1. The location of the breakpoint is somewhere between observations. 
#bplocate<-sort(temperature)[sample(6:20,1)]

#2. The location of the bp is at a specific location in the middle of temp
# observations
bplocate<-sort(temperature)[3]

#3. The location of the bp is at a specific location in the extreme
#bplocate<-sort(temperature)[3]



segment<-temperature>=bplocate
centretemp<-temperature-bplocate
phenology<-rep(NA,length(temperature))
phenology[segment==FALSE]<-160+(temperature[segment==FALSE]-bplocate)* simulate_slope
phenology[segment==TRUE]<-160+(temperature[segment==TRUE]-bplocate)* (simulate_slope + simulate_slopediff)

phenology<-phenology+rnorm(yearno,mean=0,sd=10)


dataset<-as.data.frame(cbind(phenology,temperature))
colnames(dataset)[2]<-"x"

store.output<-bkpr(phenology~x, bpv="x", data=dataset, nsim=1000,minlength=2)

storep[x]<-store.output$pval
storeporiginal[x]<-store.output$OriginalP
storeslope1[x]<-store.output$model$coefficients['x']
storeslopediff[x]<-store.output$model$coefficients['facFALSE:nx']
storebreak[x]<-store.output$breakpoint
storemeanslope1[x]<-store.output $meanslope1
storemeanslopediff[x]<-store.output $meanslopediff
storeseslope1[x]<-store.output$se.slope1
storeseslopediff[x]<-store.output$se.slopediff
storeseslopedifforiginal[x]<-store.output$model_se[3]


breakval<-store.output$breakpoint
segments<-temperature> breakval

#plot(temperature,phenology,col=as.factor(segments),pch=16)
#coefs<-store.output$model$coefficients
	#seg1<-c(min(temperature),breakval)
	#seg2<-c(store.output$breakpoint,max(temperature))
	#points(seg1,coefs["(Intercept)"]+coefs["x"]*seg1,type="l")
	#points(seg2,(coefs["(Intercept)"]+coefs["x"]*seg1)[2]+(coefs["x"]+coefs["facFALSE:nx"])*(seg2-breakval),type="l",col=2)
}


#Power
mean(storep<=0.05)

#type I errors

length(which(storep<=0.05))/length(storep)
#This quantifies power. We'd hope for 0.8
length(which(storeporiginal<=0.05))/length(storeporiginal)

#For breakpoint at value 13 the results are OriginalPvalue = 0.421, M_P = 0.197

#effect on se
plot(storeseslopedifforiginal, storeseslopediff,xlab="Original model se",ylab="Simulation based se",pch=16,col="gray",xlim=c(0,28),ylim=c(0,28))
abline(0,1,lty=2)

#Power in null model (not calculated)

#length(which(storep<0.05))/length(storep)
#This quantifies power. We'd hope for 0.8
#length(which(storeporiginal<0.05))/length(storeporiginal)

#Results: OriginalPvalue = 0.341, M_P = 0.199
#But shouldn't our model Pvalue be higher than the original? Ideally yes,
#but the power is really low primarily because the no. of observations is low.


####Manuscript figure presented in suppl. info regarding power:

#Based on 28 years

#Breakpoint ~ 13
df <- data.frame(Model_Pvalue=c("Original_P", "M_P"),
                 Freq_signi_slopediff=c(0.49, 0.27))

#Breakpoint ~ 3
df <- data.frame(Model_Pvalue=c("Original_P", "M_P"),
                 Freq_signi_slopediff=c(0.24, 0.08))

#Breakpoint ~ 13 and 3 type 1 error (null model)
df <- data.frame(Model_Pvalue=c("Original model P null BP ~13", "New model P null BP ~13", 
                                "Original model P null BP ~3", "New model P null BP ~3"),
                 Freq_signi_slope=c( 0.49, 0.27, 0.24, 0.08),
                 ID = c(1,1,2,2))

df$Model_Pvalue <- factor(df$Model_Pvalue,                 # Relevel group factor
                             levels = c("Original model P null BP ~13", "New model P null BP ~13", 
                                        "Original model P null BP ~3", "New model P null BP ~3"))

df <- data.frame(Model_Pvalue=c("Original model P null BP ~13", "New model P null BP ~13"),
                 Freq_signi_slope=c( 0.181, 0.053))

df$Model_Pvalue <- factor(df$Model_Pvalue,                 # Relevel group factor
                          levels = c("Original model P null BP ~13", "New model P null BP ~13"))

ggplot(data=df, aes(x=Model_Pvalue, y=Freq_signi_slope)) +
  geom_bar(stat="identity", width=0.4, color = "black", fill = "gray95")+
  ylab("Frequency significant slope")+
  xlab("")+
  #scale_fill_brewer(palette = "Greys")+
  theme_bw()+
  theme(axis.text.y = element_text(size = 15), 
        axis.text.x = element_text(size = 12, angle = 30, vjust = 0.5),
        axis.title.y = element_text(size = 15, vjust = 2.5))

#Breakpoint ~ 13 and 3 power

df <- data.frame(Model_Pvalue=c("New model P BP ~13", 
                                "New model P BP ~3"),
                 Freq_signi_slope_diff=c(0.266, 0.084),
                 ID = c(1,2))

df$Model_Pvalue <- factor(df$Model_Pvalue,                 # Relevel group factor
                          levels = c("New model P BP ~13", 
                                     "New model P BP ~3"))

ggplot(data=df, aes(x=Model_Pvalue, y=Freq_signi_slope_diff, fill = factor(ID))) +
  geom_bar(stat="identity", width=0.4, color = "black")+
  ylab("Frequency significant slope difference")+
  xlab("")+
  scale_fill_brewer(palette = "Greys")+
  theme_bw()+
  theme(axis.text.y = element_text(size = 15), 
        axis.text.x = element_text(size = 12, angle = 30, vjust = 0.5),
        axis.title.y = element_text(size = 15, vjust = 2.5))


#Breakpoint [13] null model
df <- data.frame(Model_Pvalue=c("Original model P", "New model P", "Original model P null", "New model P null"),
                 Freq_signi_slopediff=c(0.494, 0.266, 0.181, 0.053))

df$Model_Pvalue <- factor(df$Model_Pvalue,        # Relevel group factor
                          levels = c("Original model P", "New model P", "Original model P null", "New model P null"))

#Breakpoint [3] null model
df <- data.frame(Model_Pvalue=c("Original model P", "New model P", "Original model P null", "New model P null"),
                 Freq_signi_slopediff=c(0.217, 0.084, 0.162, 0.047))

df$Model_Pvalue <- factor(df$Model_Pvalue,        # Relevel group factor
                          levels = c("Original model P", "New model P", "Original model P null", "New model P null"))

#Breakpoint 6 - 20
df <- data.frame(Model_Pvalue=c("Original model P", "New model P", "Original model P null", "New model P null"),
                 Freq_signi_slopediff=c(0.404, 0.177, 0.181, 0.055))

df$Model_Pvalue <- factor(df$Model_Pvalue,        # Relevel group factor
                       levels = c("Original model P", "New model P", "Original model P null", "New model P null"))

ggplot(data=df, aes(x=Model_Pvalue, y=Freq_signi_slopediff)) +
  geom_bar(stat="identity", width=0.6, color = "black", fill = "gray")+
  ylab("Frequency significant slope diff.")+
  xlab("")+
  theme_bw()+
  theme(axis.text.y = element_text(size = 15), 
        axis.text.x = element_text(size = 12, angle = 30, vjust = 0.5),
        axis.title.y = element_text(size = 15, vjust = 2))

#Breakpoint [6:20] null model
df <- data.frame(Model_Pvalue=c("Original_P", "M_P"),
                 Freq_signi_slopediff=c(0.181, 0.055))


#Slope bias:

par(mfrow=c(1,2))
hist(storeslope1,xlab="slope 1",breaks=20,main="")
abline(v= simulate_slope,col=2,lty=1)
abline(v= mean(storeslope1),col=1,lty=2)

hist(storeslopediff,xlab="slope difference",breaks=20,main="")
abline(v= simulate_slopediff,col=2,lty=1)
abline(v= mean(storeslopediff),col=1,lty=2)

plot(storeslopediff,(1/storeseslopediff))
#This shows the bias away from 0. 

df1 <- as.data.frame(storeslopediff)
df1$Model <- "Non_null"

require(writexl)

#write_xlsx(df1, "Data/Simulation\\df_simulation_bp3_new.xlsx", col_names = TRUE)

#write_xlsx(df1, "Data/Simulation\\df_simulation_bp13_new.xlsx", col_names = TRUE)

#write_xlsx(df1, "Data/Simulation\\df_simulation_50.xlsx", col_names = TRUE)

#write_xlsx(df1, "Data/Simulation\\df_simulation_new.xlsx", col_names = TRUE)

df1 %>%
  ggplot(aes(x = Model, y = storeslopediff)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_boxplot(alpha = .5)+
  theme_bw()+
  theme(axis.text.y = element_text(size = 15))

#Look at simulation_null_model for slopediff = 0 simulation


