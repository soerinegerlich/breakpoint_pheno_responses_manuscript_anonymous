#Metaanalysis for peak phenology and temperature

###############################################################################
# Help and inspiration for the meta-analyses performed in the study comes from
# the guide by Harrer et al. (2021) cited in the main manuscript
###############################################################################

library(tidyverse)
library(readxl)
library(metafor)

df_summary_temp <-
  read_xlsx("Data/Summary_tables/df_summary_peak_temp_final_new.xlsx")

#rma.mv() is the only metafor function where it's possible to add random effects. However,
#it's not possible to use SE. Instead we have to include the variance of the
# effect size, which is the same as SE^2 = V
#When usidng rma.mv() we need to include observation ID's for standard random effects
#we can paste SpeciesID and Plot together and then feed it to the model
df_summary_temp$plotspec <-
  as.factor(paste(df_summary_temp$SpeciesID, df_summary_temp$Plot))

df_summary_temp$SEslopediffsq <- df_summary_temp$SEslopediff ^ 2

all <-
  rma.mv(
    yi = Slopediff,
    V = SEslopediffsq,
    random = list( ~ 1 |
                     SpeciesID, ~ 1 | Plot, ~ 1 | plotspec),
    data = df_summary_temp
  )

summary(all)

######## Run rma function on simple function with no random effects ########

simple <-
  rma.mv(yi = Slopediff, V = SEslopediffsq, data = df_summary_temp)
summary(simple)

anova(simple, all)

forest(
  all,
  slab = paste(SpeciesID, Plot, sep = ", "),
  addpred = TRUE,
  header = "Taxa and Plot",
  colout = "blue",
  #xlim = c(-15, 15),
  xlab = "Change in peak emergence (days per °C)"
)

########### Test for simple linear regression ############
df_summary_temp$OriginalSEsq <- df_summary_temp$OriginalSE ^ 2

all_linear <-
  rma.mv(
    yi = OriginalSlope,
    V = OriginalSEsq,
    random = list( ~ 1 |
                     SpeciesID, ~ 1 | Plot, ~ 1 | plotspec),
    data = df_summary_temp
  )

summary(all_linear)

#### How does taxon and habitat contribute with variation in the model? ####
#### Test of variance for each random effect parameter ####

var.test <-
  rma.mv(
    yi = Slopediff,
    V = SEslopediffsq,
    random = list( ~ 1 | SpeciesID, ~ 1 | Plot),
    data = df_summary_temp
  )


variance_decomposition <- function(m) {
  n <- m$k
  vector.inv.var <- 1 / (diag(m$V))
  sum.inv.var <- sum(vector.inv.var)
  sum.sq.inv.var <- (sum.inv.var) ^ 2
  vector.inv.var.sq <- 1 / (diag(m$V) ^ 2)
  sum.inv.var.sq <- sum(vector.inv.var.sq)
  num <- (n - 1) * sum.inv.var
  den <- sum.sq.inv.var - sum.inv.var.sq
  est.samp.var <- num / den
  if (length(m$sigma2) > 2)
    stop("Cannot handle more than three levels.")
  total_var <- (sum(m$sigma2) + est.samp.var) / 100
  Variance <- c(est.samp.var, m$sigma2) / total_var
  names(Variance) <- c("Level1", m$s.names)
  Variance
}

variance_decomposition(var.test)

#### In addition to the test of variance, each random effect variable is included
# separately in a model to compare effect of output from meta-analysis #####

#Taxa

md <-
  rma.mv(
    yi = Slopediff,
    V = SEslopediffsq,
    random = list( ~ 1 | SpeciesID),
    data = df_summary_temp
  )
summary(md)

#Plot

md1 <-
  rma.mv(
    yi = Slopediff,
    V = SEslopediffsq,
    random = list( ~ 1 | Plot),
    data = df_summary_temp
  )
summary(md1)

#Residual variance

md2 <-
  rma.mv(
    yi = Slopediff,
    V = SEslopediffsq,
    random = list( ~ 1 | plotspec),
    data = df_summary_temp
  )
summary(md2)

#Compare models with likelihood ratio and wald-type tests for rma objects

anova(simple, md)
anova(simple, md1)
anova(simple, md2)

anova(all, md)
anova(all, md1)
anova(all, md2)


######## Same meta-analyses but with slope of first linear segment ########

df_summary_temp$SEslopesq <- df_summary_temp$SEslope ^ 2

########## Run rma function on full model ###########

all_slope <-
  rma.mv(
    yi = Slope1,
    V = SEslopesq,
    random = list( ~ 1 |
                     SpeciesID, ~ 1 | Plot, ~ 1 | plotspec),
    data = df_summary_temp
  )

summary(all_slope)

########## Run rma function on simple model ###########

simple_slope <- rma.mv(yi = Slope1, V = SEslopesq, data = df_summary_temp)

summary(simple_slope)

anova(simple_slope, all_slope)

forest(
  all,
  slab = paste(SpeciesID, Plot, sep = ", "),
  addpred = TRUE,
  header = "Taxa and Plot",
  colout = "red",
  xlim = c(-10, 10),
  xlab = "Change in peak emergence (days per °C)"
)

####How does taxon and habitat contribute with variation in the model?

var.test.slope <-
  rma.mv(
    yi = Slope1,
    V = SEslopesq,
    random = list( ~ 1 | SpeciesID, ~ 1 | Plot),
    data = df_summary_temp
  )

variance_decomposition(var.test.slope)

md_slope <-
  rma.mv(
    yi = Slope1,
    V = SEslopesq,
    random = list( ~ 1 | SpeciesID),
    data = df_summary_temp
  )
summary(md_slope)

md1_slope <-
  rma.mv(
    yi = Slope1,
    V = SEslopesq,
    random = list( ~ 1 | Plot),
    data = df_summary_temp
  )
summary(md1_slope)

md2_slope <-
  rma.mv(
    yi = Slope1,
    V = SEslopesq,
    random = list( ~ 1 | plotspec),
    data = df_summary_temp
  )
summary(md2_slope)

anova(simple_slope, md_slope)
anova(simple_slope, md1_slope)
anova(simple_slope, md2_slope)

anova(all_slope, md_slope)
anova(all_slope, md1_slope)
anova(all_slope, md2_slope)


