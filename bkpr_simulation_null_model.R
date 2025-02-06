
#Simulation under the null model (simulated slope difference = 0)

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


simulate_slope<--5
simulate_slopediff<-0
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
  bplocate<-sort(temperature)[13]
  
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
  
  store.output<-bkpr(phenology~x, bpv="x", data=dataset, nsim=1000,minlength=5)
  
  storep[x]<-store.output$pval
  storeporiginal[x]<-store.output$OriginalP
  storeslope1[x]<-store.output$model$coefficients['x']
  storeslopediff[x]<-store.output$model$coefficients['facFALSE:nx']
  storebreak[x]<-store.output$breakpoint
  storemeanslope1[x]<-store.output $meanslope1
  storemeanslopediff[x]<-store.output $meanslopediff
  storeseslope1[x]<-store.output$se.slope1
  storeseslopediff[x]<-store.output$se.slopediff
  
  
  breakval<-store.output$breakpoint
  segments<-temperature> breakval
  
  #plot(temperature,phenology,col=as.factor(segments),pch=16)
  #coefs<-store.output$model$coefficients
  #seg1<-c(min(temperature),breakval)
  #seg2<-c(store.output$breakpoint,max(temperature))
  #points(seg1,coefs["(Intercept)"]+coefs["x"]*seg1,type="l")
  #points(seg2,(coefs["(Intercept)"]+coefs["x"]*seg1)[2]+(coefs["x"]+coefs["facFALSE:nx"])*(seg2-breakval),type="l",col=2)
}


#Power in null model

length(which(storep<=0.05))/length(storep)
#This quantifies power. We'd hope for 0.8
length(which(storeporiginal<=0.05))/length(storeporiginal)

plot(storeseslopedifforiginal, storeseslopediff,xlab="Original model se",ylab="Simulation based se",pch=16,col="gray",xlim=c(0,25),ylim=c(0,25))
abline(0,1,lty=2)


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

df2 <- as.data.frame(storeslopediff)
df2$Model <- "Null"

#require(writexl)
#write_xlsx(df2, "Data/Simulation\\df_simulation_null.xlsx", col_names = TRUE)

df2 %>%
  ggplot(aes(x = Model, y = storeslopediff)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_boxplot(alpha = .5)

require(readxl)
df1 <- read_xlsx("Data/Simulation/df_simulation_bp13_new.xlsx")
#df2 <- read_xlsx("Data/Simulation/df_simulation_null.xlsx")


#library(dplyr)
df_new <- bind_rows(df1, df2)

median(df1$storeslopediff)
mean(df1$storeslopediff)

median(df2$storeslopediff)
mean(df2$storeslopediff)

df_new <- df_new %>% 
  rename("Slope_diff_estimate" = "storeslopediff")


ggplot(df_new, aes(x = Model, y = Slope_diff_estimate)) + 
  geom_hline(yintercept = 5, color = "red") + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_boxplot(alpha = .5) +
  theme_classic()
  #annotate("text", x = df_new$Model=="Non_null", y = 15, label = "5.99") + 
  #annotate("text", x = df_new$Model=="Null", y = 15, label = "0.17")

df_new %>%
  group_by(Model)%>%
  summarise(Mean = mean(storeslopediff)) -> df_mean



