### -----------------------------------
## 
## Script name: Analysis #3
## 
## Author: Juan Gelvez-Ferreira
## Date created:My 3rd, 2023
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
dat <- rio::import(here::here("data","nclb.csv"))

#summary dataset
dim(dat)
names(dat)
str(dat)
head(dat) 
tail(dat)

# ::::: variables :::::
# state  ... factor indicating the states
# year   ... year of observation
# s.prof ... proficiency rates according to state tests (Table 2 in paper)
# math4  ... math achievement score for 4th graders
# math8  ... math achievement score for 8th graders
# read4  ... reading achievement score for 4th graders
# flunch ... percent of students with free-lunch status (time-varying)
# puptch ... pupil-teacher-ratio (time varying)


# ::::: delete cases (years) where all three outcomes are missing :::::
# ::::: (is not necessary but creates a smaller data set) :::::
head(dat)   # note for some years all outcomes are missing (e.g. Alabama 1991)
dat <- subset(dat, !(is.na(math8)))
head(dat)

# ::::: create proficiency & standards groups (page 10-12 of paper) :::::
# ::::: note: high proficiency rates imply low standards! ::::::::::::
# proficiency categories
dat$prof3 <- cut(dat$s.prof, c(0, 50, 75, 100), labels = c('low', 'medium', 'high'), 
                 right = F)
View(dat)
# standard is inverse to proficiency (100% - %proficient)
dat$standard <- 100 - dat$s.prof
# standards categories
dat$stand3 <- cut(dat$standard, c(0, 25, 50, 100), labels = c('low', 'medium', 'high'))

# check coding
table(dat$prof3, dat$stand3)

# ::::: create policy variable (indicator of NCLB intervention) :::::::::::::::
# ::::: intervention in 2002: thus, 2003 is first year after intervention :::::
dat$policy <- ifelse(dat$year > 2002, 1, 0)

# ::::: create year variable centered at intervention (2002) :::::
dat$year02 <- dat$year - 2002

# check coding
table(dat$year, dat$policy)
table(dat$year02, dat$policy)

# ::::: remove New York and Vermont from data set (page 10, footnote)
dat <- subset(dat, state != 'New York' & state != 'Vermont')



#Part 2. Estimate a simple ITS model for all states together (without any comparison group)::
summary(out<-lm(math8~year02+policy+year02:policy,data = dat))
#2.	Get the predicted NCLB effect for the year 2011
2.0945+9*0.1620


#Part 3. Estimate the NCLB effect using a CITS design, i.e., use the standards grouping of states:::
dat$stand3<-as.factor(dat$stand3)
summary(out<-lm(math8~policy+year02+stand3+
                  policy:year02+policy:stand3+year02:stand3+
                  policy:year02:stand3,data = dat))


#:Part 4. Estimate the NCLB effect using a fixed effects CITS model
levels(dat$stand3)
dat$stand3 <- ordered(dat$stand3, levels = c("high", "medium", "low"))



#1.	Run the same regression as in part 3 but include state fixed effects. (Fixed effects are modeled by including dummy variables for the states.) 

summary(out1<-lm(math8~policy:stand3+year02:stand3+
                   policy:year02:stand3+
                   as.factor(dat$state)+
                   flunch+puptch,data = dat))

#2. state and time fixed effects
summary(out2<-lm(math8~policy:stand3+year02:stand3+
                   policy:year02:stand3+
                   as.factor(dat$state)+as.factor(dat$year)+
                   flunch+puptch,data = dat))


stargazer(out1,out2,single.row = TRUE,no.space = T,notes.align = "l")


#Results
windows()
View(dat)

table(dat$year)

aggregate(x=dat$math8,
          by=list(dat$stand3,dat$year),
          FUN=mean)

dat$mean_math8_year_stand3<-0
dat$mean_math8_year_stand3[which(dat$year==1990 & dat$stand3=="low")]<-266.7594
dat$mean_math8_year_stand3[which(dat$year==1990 & dat$stand3=="medium")]<-264.536
dat$mean_math8_year_stand3[which(dat$year==1990 & dat$stand3=="high")]<-255.4753
dat$mean_math8_year_stand3[which(dat$year==1992 & dat$stand3=="low")]<-270.1209
dat$mean_math8_year_stand3[which(dat$year==1992 & dat$stand3=="medium")]<-265.9958
dat$mean_math8_year_stand3[which(dat$year==1992 & dat$stand3=="high")]<-263.4548
dat$mean_math8_year_stand3[which(dat$year==1996 & dat$stand3=="low")]<-273.8240
dat$mean_math8_year_stand3[which(dat$year==1996 & dat$stand3=="medium")]<-270.0279
dat$mean_math8_year_stand3[which(dat$year==1996 & dat$stand3=="high")]<-266.8702
dat$mean_math8_year_stand3[which(dat$year==2000 & dat$stand3=="low")]<-274.9010
dat$mean_math8_year_stand3[which(dat$year==2000 & dat$stand3=="medium")]<-272.4114
dat$mean_math8_year_stand3[which(dat$year==2000 & dat$stand3=="high")]<-266.0665
dat$mean_math8_year_stand3[which(dat$year==2003 & dat$stand3=="low")]<-280.7218
dat$mean_math8_year_stand3[which(dat$year==2003 & dat$stand3=="medium")]<-276.4957
dat$mean_math8_year_stand3[which(dat$year==2003 & dat$stand3=="high")]<-272.9392
dat$mean_math8_year_stand3[which(dat$year==2005 & dat$stand3=="low")]<-281.4311
dat$mean_math8_year_stand3[which(dat$year==2005 & dat$stand3=="medium")]<-277.3382
dat$mean_math8_year_stand3[which(dat$year==2005 & dat$stand3=="high")]<-274.5448
dat$mean_math8_year_stand3[which(dat$year==2007 & dat$stand3=="low")]<-283.9525
dat$mean_math8_year_stand3[which(dat$year==2007 & dat$stand3=="medium")]<-280.305
dat$mean_math8_year_stand3[which(dat$year==2007 & dat$stand3=="high")]<-277.6355
dat$mean_math8_year_stand3[which(dat$year==2009 & dat$stand3=="low")]<-285.8688
dat$mean_math8_year_stand3[which(dat$year==2009 & dat$stand3=="medium")]<-281.9470
dat$mean_math8_year_stand3[which(dat$year==2009 & dat$stand3=="high")]<-279.5825
dat$mean_math8_year_stand3[which(dat$year==2011 & dat$stand3=="low")]<-286.8182
dat$mean_math8_year_stand3[which(dat$year==2011 & dat$stand3=="medium")]<-282.72
dat$mean_math8_year_stand3[which(dat$year==2011 & dat$stand3=="high")]<-281.6154



windows()
plot(mean_math8_year_stand3 ~ year, data = dat, cex = .7, col = as.numeric(stand3),
     main = 'Math Achievement Scores (8th Graders)')


abline(v = 2002)
for (i in c(1,2,3)) {
  m.tab <- with(dat[dat$stand3 == levels(dat$stand3)[i], ], tapply(math8, year, mean))
  m.tab <- m.tab[!is.na(m.tab)]   # remove NAs
  lines(as.numeric(names(m.tab[1:4])), m.tab[1:4], col = i, type = 'o', lwd = 1, lty = 2)
  lines(as.numeric(names(m.tab[5:9])), m.tab[5:9], col = i, type = 'o', lwd = 1, lty = 2)
}
legend('topleft', c('High proficiency standard', 'Medium proficiency standard',"Low proficiency standard"),
       col = c(1,2,3), pch = 1, lwd = 2, bty = 'n')



# add regression lines to plot 

for (i in c(1,2,3)) {
  pre.inv <- predict(out2, data.frame(policy = 0, year02 = -12:0, 
                                      flunch=dat$flunch[which(dat$policy==0 & dat$year02==-12:0)],
                                      puptch=dat$puptch[which(dat$policy==0 & dat$year02==-12:0)],
                                      stand3 = levels(dat$stand3)[i]))
  post.inv <- predict(out2, data.frame(policy = 1, year02 = 0:9, 
                                       flunch=dat$flunch[which(dat$policy==0 & dat$year02==-12:0)],
                                       puptch=dat$puptch[which(dat$policy==0 & dat$year02==-12:0)],
                                       stand3 = levels(dat$stand3)[i]))
  lines(1990:2002, pre.inv, col = i, lwd = 2)
  lines(2002:2011, post.inv, col = i, lwd = 2)
}


dat$predict_value<-predict(out2)
lines(1990:2002,dat$predict_value[which(dat$year<=2002)])
lines(2002:2011,dat$predict_value[which(dat$year>2002)])

# extrapolations
for (i in c(1,2,3)) {
  pre.extrap <- predict(out2, data.frame(policy = 0, year02 = 0:9, 
                                         stand3 = levels(dat$stand3)[i]))
  lines(2002:2011, pre.extrap, col = i, lwd = 2, lty = 2)
}
