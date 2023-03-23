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



#(non)compliance table

(tab <- with(data, table(R, T)))
addmargins(prop.table(tab, 1), 2)
round(sapply(data, mean), 3)
round(sapply(data, sd), 3)

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

# transform data into long format (i.e., two rows for each student) 
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
# summary(lm(math ~ -1 + time + I(T*time) + id, dat.long))   # alternative

# alternative specifications in R (with multicollinearities/singularities)
summary(out <- lm(math ~ T + time + T:time + id, dat.long))  # id1 dropped / id999 NA
alias(out)                                         # check aliasing structure

summary(lm(math ~ time + T:time + id, dat.long))   # id1 dropped
# no aliasing because R assumes you know what the meaning of the intercept is

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


data <- data[sample(1:nrow(data), 300), ]

