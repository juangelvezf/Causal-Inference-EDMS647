### -----------------------------------
## 
## Script name: Analysis # 1
## class: EDMS647 - Causal Inference & Evaluation Methods
## Author: Juan Gelvez-Ferreira
## Date created: March 22nd, 23
## Email: j.gelvez@umd.edu
##
## ------------------------------------

# clear all the space objects
rm(list=ls()) 

# Load up the packages 

packages = c("tidyverse","readxl","ggplot2","skimr","writexl","sem","lme4")

package.check <- lapply(packages, FUN = function(x) {
if (!require(x, character.only = TRUE)) {
install.packages(x, dependencies = TRUE)
library(x, character.only = TRUE)
  
}})



# open data
# database_name <- rio::import("/.../name_database.csv")
data <- rio::import(here::here("data","math_camp_RCT.csv"))

# descriptive statistics 
head(data)
skim(data)



# Checking balance among groups

select.diff <- function(x, grp, wt = rep(1, length(x))) 
{
  # computes balance statistics: standardized mean difference and variance ratio
  # and performs a regression test
  # also works for dichotomeous categorical variables (but not for more than two categories)
  # x    ... baseline covariate
  # grp  ... dichotomous grouping variable (treatment/assignment)
  # wt   ... weights (typically inverse-propensity or PS-strata weights)
  
  if (is.factor(grp)) grp <- as.numeric(grp) - 1     # transform dichotomeous factor into dummy
  if (is.factor(x)) x <- as.numeric(x) - 1           # transform dichotomeous factor into dummy
  stat.wt <- function(xw) {                          # function computing weighted mean and variance
    out <- cov.wt(xw[, 1, drop = F], xw[, 2])
    c(mean.wt = out$center, var.wt = out$cov)
  }
  stat <- unlist(by(cbind(x, wt), grp, stat.wt))     # weighted mean and variance (vector)
  m <- stat[seq(1, 4, by = 2)]                       # extract means
  v <- stat[seq(2, 4, by = 2)]                       # extract variances
  B <- (m[2] - m[1]) / sqrt(sum(v) / 2)              # standardized difference in means
  R <- v[2] / v[1]                                   # variance ratio
  rslt <- summary(lm(x ~ grp, weights = wt))         # run regression test
  res <- c(rslt$coef[2, c(1:2, 4)], B, R)
  names(res) <- c("Mean.Diff", "Std.Error", "p-value", "Std.Mean.Diff", "Var.Ratio")
  res
}

dnsty <- function(x, grp, wt = rep(1, length(x)), main = NULL, 
                  lwd = 1, col.vec = c('blue', 'red'), ...) 
{
  # plots kernel-density estimates for groups
  # x   ... metric variable
  # grp ... grouping variable (factor)
  # wt  ... weights for observations (e.g., inverse-propensity weights or survey weights)
  
  lev <- levels(grp)
  cmplt <- na.omit(data.frame(x, grp, wt))
  x <- cmplt$x
  grp <- cmplt$grp
  wt <- cmplt$wt
  dens <- list()
  x.m <- rep(NA, length(table(grp)))
  bw <- density(x, weights = wt / sum(wt))$bw
  for (i in 1:length(table(grp))) {
    grp.ind <- grp == lev[i]
    dens[[i]] <- if (is.null(wt)) density(x[grp == lev[i]], ...) else
      density(x[grp.ind], na.rm = T, weights = wt[grp.ind] / sum(wt[grp.ind]), bw = bw, ...)
    x.m[i] <- weighted.mean(x[grp.ind], w = wt[grp.ind]) 
  }
  plot(dens[[1]], type = 'n', main = main, xlab = '', ylab = '', yaxt = 'n',
       ylim = c(0, max(c(dens[[1]]$y, dens[[2]]$y))), 
       xlim = range(c(dens[[1]]$x, dens[[2]]$x)), bty = 'l')
  for (i in 1:length(table(grp))) {
    grp.ind <- grp == lev[i]
    ind <- dens[[i]]$x - x.m[i]
    lines(dens[[i]], col = col.vec[i], lwd = lwd, lty = 3 - i, xpd = T)
    lines(rep(x.m[i], 2), c(0, dens[[i]]$y[abs(ind) == min(abs(ind))][1]), col = col.vec[i], lwd = lwd, lty = 3)
  }
}

# 




#(non)compliance table

(tab <- with(data, table(R, T)))
addmargins(prop.table(tab, 1), 2)


#Balance check - random assignment (R)
round(select.diff(data$P, data$R), 3)
round(select.diff(data$SES, data$R), 3)
dnsty(data$P, as.factor(data$R), main = 'Pretest distribution by assignment (R)')
dnsty(data$SES, as.factor(data$R), main = 'SES distribution by assignment (R)')

#Balance check - treatment received (T)
round(select.diff(data$P, data$T), 3)       
round(select.diff(data$SES, data$T), 3)
dnsty(data$P, as.factor(data$T), main = 'Pretest distribution by treatment receipt (T)')
dnsty(data$SES, as.factor(data$T), main = 'SES distribution by treatment receipt (T)')



##### 
#  TREATMENT EFFECT ESTIMATION 

#0) Intent to Treat (ITT)

summary(lm(Y ~ R, data))


#1) Treatment-As-Received Analysis

summary(lm(Y ~ T, data))


#2) Per-Protocol Analysis

summary(lm(Y ~ T, data, sub = (R == 1 & T == 1) | (R == 0 & T == 0)))


#3) IV Estimator with R as instrument

with(data, table(R, T))                  # (non)compliance table
summary(lm(T ~ R, data))                 # Is first stage predictive?
summary(tsls(Y ~ T, ~ R, data))          # 2SLS without covariates
summary(tsls(Y ~ T + SES + P, ~ R + SES + P, data))  # 2SLS with covariates


#4) Gain/change Score Analysis

summary(lm(I(Y - P) ~ T, data))          # without covariates
summary(lm(I(Y - P) ~ T + SES, data))    # with covariate (DO NOT include P!)


#5) Difference-in-Differences (DiD) Estimator

dat.long <- reshape(data, 
                    direction = 'long',
                    varying = list(c('P', 'Y')),
                    v.names = 'math',
                    idvar = c('id'),
                    timevar = 'time',
                    times = c(0, 1))
dat.long <- dat.long[order(dat.long[, 'id']), ]    # sorting would not be necessary
head(dat.long)
dat.long$id <- as.factor(dat.long$id)              # convert id into a factor

# (a) DiD - Fixed Effects Model
summary(lm(math ~ -1 + time + T:time + id, dat.long))
summary(lm(math ~ -1 + time + I(T*time) + id, dat.long))   # alternative

# alternative specifications in R (with multicollinearities/singularities)
summary(out1 <- lm(math ~ T + time + T:time + id, dat.long))  # id1 dropped / id999 NA
alias(out1)                                         # check aliasing structure

summary(lm(math ~ time + T:time + id, dat.long))   # id1 dropped

# with time-constant covariates (note the interaction term!)
summary(lm(math ~ -1 + time + T:time + SES:time + id, dat.long))


# (b) DiD - Random Effects Model
summary(lmer(math ~ T + time + T:time + (1 | id), dat.long))
summary(lmer(math ~ T + time + T:time + SES + (1 | id), dat.long))


#6) ANCOVA/Regression -Covariate adjustment with P

# variance-of-treatment weighted ATE
summary(lm(Y ~ T + P, data))
summary(lm(Y ~ T + P + SES, data))    # SES might increase bias

# ATE
data$P.c <- with(data, P - mean(P))
data$SES.c <- with(data, SES - mean(SES))
summary(lm(Y ~ T + P.c + T:P.c, data))
summary(lm(Y ~ T + P.c + SES.c + T:P.c + T:SES.c, data))

# ATT T=mean and add interactions
data$P.Tc <- with(data, P - mean(P[T == 1]))
data$SES.Tc <- with(data, SES - mean(SES[T == 1]))
summary(lm(Y ~ T + P.Tc + T:P.Tc, data))
summary(lm(Y ~ T + P.Tc + SES.Tc + T:P.Tc + T:SES.Tc, data))



