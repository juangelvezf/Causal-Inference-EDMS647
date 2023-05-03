### -----------------------------------
## 
## Script name: Analysis #2
## 
## Author: Juan Gelvez-Ferreira
## Date created:April, 5th
## Email: j.gelvez@umd.edu
##
## ------------------------------------

# clear all the space objects
rm(list=ls()) 

# Load up the packages 

packages = c("tidyverse","readxl","ggplot2","skimr","writexl","foreign","stargazer","xtable")

package.check <- lapply(packages, FUN = function(x) {
if (!require(x, character.only = TRUE)) {
install.packages(x, dependencies = TRUE)
library(x, character.only = TRUE)
  
}})


# open data
# database_name <- rio::import("/.../name_database.csv")
data <- rio::import(here::here("data","shadish&clark_imputed_QE_nooutcome.sav"))

#summary dataset
skim(data)


##### Part 1
select.diff <- function(x, grp, wt = rep(1, length(x))) 
{
  # computes balance statistics: standardized mean difference and variance ratio
  # and performs a regression test
  # also works for dichotomeous categorical variables (but not for more than two categories)
  # x    ... baseline covariate
  # grp  ... grouping variable (treatment/assignment) - factor
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

overlap <- function(x, z, lab = NULL, bin = 20)
{
  # plot a histogram of a covariate by group
  # x   ... numeric vector (covariate)
  # z   ... treatment indicator (dummy)
  # lab ... label for title and x-axis
  # bin ... number of bins for histogram
  
  r1 <- range(x)
  if (!is.numeric(z)) z <- as.numeric(z) - 1
  c.dat <- hist(x[z == 0], seq(r1[1], r1[2], length = bin), plot = F)  # histogram data for control group
  t.dat <- hist(x[z == 1], seq(r1[1], r1[2], length = bin), plot = F)  # histogram data for treatm. group
  t.dat$counts <- -t.dat$counts
  plot(c.dat, axes = F, ylim = c(min(t.dat$counts), max(c.dat$counts)),
       main = lab, xlab = lab)
  plot(t.dat, add = TRUE, density = 30)
  axis(1)
  ax.loc <- axis(2, labels = F)
  axis(2, at = ax.loc, labels = abs(ax.loc))
  y <- par('usr')[3:4]
  text(rep(max(x), 2), c(y[2] - diff(y)*.05, y[1] + diff(y)*.05), 
       c('Control', 'Treatment'), adj = 1, xpd = TRUE)
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


discard <- function(lps, grp, caliper = .05)
{
  # creates an index for discarding non-overlapping cases (with caliper)
  # returns logical vector (F = overlapping, T = non-overlapping)
  # lps     ... propensity score logit
  # grp     ... treatment indicator
  # caliper ... caliper in SD of lps
  
  ovl <- max(tapply(lps, grp, min)) - sd(lps) * caliper   # left limit of overlap
  ovr <- min(tapply(lps, grp, max)) + sd(lps) * caliper   # right limit of overlap
  ind <- lps > ovr | lps < ovl
  cat('Number of non-overlapping cases:', sum(ind), '::: breaks:', ovl, ovr, '\n')
  ind
}


########## Part 2 - balance

# treatment variable (vm) 
table(data$vm)     # frequency distribution

# ::::: create a vector with names of covariates (for checking imbalance) ::::: 
# adjustment set according to the causal graph ????????????һЩfancier?Ķ???
adj.set <- c("mathpre", "vocabpre", "actcomp", "hsgpaar", "collgpaa",
             "numbmath", "likemath", "likelit", "preflit", "majormi", "mars", 
             "cauc", "afram", "other", "male")

# all covariates (the entire "kitchen sink")
all.set <- c("mathpre", "vocabpre", "actcomp", "hsgpaar", "collgpaa",
             "numbmath", "likemath", "likelit", "preflit", "majormi", "mars", 
             "cauc", "afram", "other", "male",
             "pextra", "pagree", "pconsc", "pemot", "pintell", "beck",
             "age", "married", "momdegr", "daddegr", "credit")

# ::::: get simple summary statistics for baseline covariates :::::
summary(data[, all.set])
stargazer(data)


# ::::: test imbalance in observed covariates :::::
# ::::: apply select.diff() to all covariates: "imbalance statistics" :::::
imbal <- t(sapply(data[, all.set], select.diff, grp = data$vm))
round(imbal, 3)
stargazer(imbal)


# ::::: plot standardized mean difference & variance ratio :::::
B <- imbal[, "Std.Mean.Diff"]
R <- imbal[, "Var.Ratio"]
windows()
plot(B, R, xlim = c(-1, 1), pch = 16,
     main = 'Imbalance', xlab = 'Std. Mean Difference', ylab = 'Variance Ratio')
abline(h = 1, v = 0)
abline(h = c(4/5, 5/4), v = c(-.1, .1), lty = 3)   # add Rubin's benchmarks

# add labels for imbalanced covariates
ind <- (abs(B) > .1 | R < 4/5 | R > 5/4) 
text(B[ind], R[ind], all.set[ind], pos = 2 + 2*(B[ind] > 0), offset = .4, cex = .7, xpd = T)


############# PSM #############

# ::::: create initial PS model - simple main effects model :::::

# use all covariates, i.e., the entire "kitchen sink"
mdl1 <- as.formula(vm ~ mathpre + vocabpre + actcomp + hsgpaar + collgpaa +
                     numbmath + likemath + likelit + preflit + majormi +
                     mars + cauc + afram + male +
                     pextra + pagree + pconsc + pemot + pintell + beck +
                     age + married + momdegr + daddegr + credit) 

out1 <- glm(mdl1, data = data, family = 'binomial')
summary(out1)



# get PS and PS-logit of initial model :::::
out1$fitted.values
data$ps <- out1$fitted                        # fitted values are the PS
data$lps <- log(data$ps / (1 - data$ps)) # PS-logit = log(PS/(1-PS))
View(data) 

# create index for deleting non-overlapping cases :::::
(del.ind <- discard(data$lps, data$vm, caliper = .05)) ###0.05??sd of logit ps, ????ֵΪtrue?ı?ɾ??        
overlap(data$lps[!del.ind], data$vm[!del.ind], bin = 30)


# ::::: inverse-propensity weighting for estimating ATT :::::
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# ::::: compute weights for all cases :::::
# for all cases
data$z <- ifelse(data$vm == 'Mathematics', 0, 1)     # dummy variable for treatment indicator
data$iptw <- with(data, z + (((1 - z)*ps) / (1 - ps))) # computation of IPTWs
View(data)
ncol(data)
# ::::: compute weights for overlapping cases only :::::
data$iptwo <- data$iptw
data$iptwo[del.ind] <- 0    # set weights for non-overlapping cases to zero



# ::::::::::::::::::::::::::
# ::::: balance checks :::::
# ::::::::::::::::::::::::::

# -> check balance on all baseline covariates (also the ones not in the model)
# ::::: apply select.diff() to all covariates: "imbalance statistics" :::::
(imbal <- t(sapply(data[, all.set], select.diff, grp = data$vm, wt = data$iptw)))


# ::::: plot standardized mean difference & variance ratio :::::
B <- imbal[, "Std.Mean.Diff"]
R <- imbal[, "Var.Ratio"]
windows()
plot(B, R, xlim = c(-1, 1), pch = 16,
     main = 'Imbalance', xlab = 'Std. Mean Difference', ylab = 'Variance Ratio')
abline(h = 1, v = 0)
abline(h = c(4/5, 5/4), v = c(-.1, .1), lty = 3)

# ::::: add labels for imbalanced covariates :::::
ind <- (abs(B) > .1 | R < 4/5 | R > 5/4) 
text(B[ind], R[ind], all.set[ind], pos = 2 + 2*(B[ind] > 0), offset = .4, cex = .7, xpd = T)


###effect#####
data.out <- rio::import(here::here("data","shadish&clark_imputed_QE.sav"))
data$vocaball <- data.out$vocaball

# ::::: prima facie effect, i.e., without any PS or covariance adjustment (biased estimate) ::::
summary(lm(vocaball ~ vm, data = data)) 

# ::::: ATT with PS adjustment: inverse-propensity weighting :::::
summary(lm(vocaball ~ vm, data = data, weights = iptw))   # with all cases
summary(lm(vocaball ~ vm, data = data, weights = iptwo))  # with overlapping cases

# ::::: ATT with additional covariance adjustment (doubly robust or mixed method) :::::
# create formula for outcome model (use same covariates as in PS model)
mdl2 <- as.formula(vocaball ~ vm + mathpre + vocabpre + actcomp + hsgpaar + collgpaa +
                     numbmath + likemath + likelit + preflit + majormi +
                     mars + cauc + afram + male +
                     pextra + pagree + pconsc + pemot + pintell + beck +
                     age + married + momdegr + daddegr + credit) 
summary(lm(mdl2, data = data, weights = iptw))
summary(lm(mdl2, data = data, weights = iptwo))                # with overlapping cases




############################## 
### PS stratification for estimating ATT


# ::::: compute quintiles (5 strata or subclasses) for all cases  :::::
data$ps5 <- cut(data$lps, quantile(data$lps, seq(0, 1, by = .2)), 
               include.lowest = TRUE) 


# crosstable (treatment x strata)
table(data$vm, data$ps5)


# ::::: compute individual stratum weights :::::
(O <- table(data$vm, data$ps5))   # observed table
data$strwt<-NULL
data$strwt[data$lps<=-0.35]<-79/42*9/33
data$strwt[data$lps>-0.35 & data$lps<=0.448]<-79/42*25/17
data$strwt[data$lps>0.448 & data$lps<=1.05]<-79/42*27/15
data$strwt[data$lps>1.05 & data$lps<=1.64]<-79/42*33/9
data$strwt[data$lps>1.64 ]<-79/42*37/5
#####treated group ?????˵?1
data$strwt[data$z==1]<-1
View(data)

#balance checks - check balance on all baseline covariates (also the ones not in the model)

# ::::: apply select.diff() to all covariates: "imbalance statistics" :::::
(imbal <- t(sapply(data[, all.set], select.diff, grp = data$vm, wt = data$strwt)))
# ::::: plot standardized mean difference & variance ratio 
B <- imbal[, "Std.Mean.Diff"]
R <- imbal[, "Var.Ratio"]
windows()
plot(B, R, xlim = c(-1, 1), pch = 16,
     main = 'Imbalance', xlab = 'Std. Mean Difference', ylab = 'Variance Ratio')
abline(h = 1, v = 0)
abline(h = c(4/5, 5/4), v = c(-.1, .1), lty = 3)

# add labels for imbalanced covariates 
ind <- (abs(B) > .1 | R < 4/5 | R > 5/4) 
text(B[ind], R[ind], all.set[ind], pos = 2 + 2*(B[ind] > 0), offset = .4, cex = .7, xpd = T)

##ATT stratification

# ::::: ATE with PS adjustment: PS stratification using marginal mean weights :::::
summary(lm(vocaball ~ vm, data = data, weights = strwt))   # with all cases

# ::::: ATE with additional covariance adjustment (doubly robust or mixed method) :::::
# create formula for outcome model (use same covariates as in PS model)
mdl2 <- as.formula(vocaball ~ vm + mathpre + vocabpre + actcomp + hsgpaar + collgpaa +
                     numbmath + likemath + likelit + preflit + majormi +
                     mars + cauc + afram + male +
                     pextra + pagree + pconsc + pemot + pintell + beck +
                     age + married + momdegr + daddegr + credit) 
summary(lm(mdl2, data = data, weights = strwt))
  





