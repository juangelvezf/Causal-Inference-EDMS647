
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::::::::: NCLB data of Wong, Cook & Steiner (2015) :::::::::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# >>>>> read the paper by Wong, Cook & Steiner and replicate parts of their analyses
# >>>>> this script file also helps you to read the data
# >>>>> and to create some variables for the analysis

# ::::: set working directory :::::
setwd("P:/teaching/uw/QE/data&syntax")  # change to your working directory

# ::::: read data: .csv-file :::::
dat <- read.csv('nclb.csv')   

# data structure
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
dat <- subset(dat, !(is.na(math4) & is.na(math8) & is.na(read4)))
head(dat)

# ::::: create proficiency & standards groups (page 10-12 of paper) :::::
# ::::: note: high proficiency rates imply low standards! ::::::::::::
# proficiency categories
dat$prof3 <- cut(dat$s.prof, c(0, 50, 75, 100), labels = c('low', 'medium', 'high'), 
                 right = F)

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


# ::::: now run the models according to the assignment :::::

# Notes:
# (0) You only need to do the analysis for the 8th Grade Math outcome 
#     (only for the categorical standards measure (low, medium, high); no need to do it for the continuous one)
# (1) There are many ways to parameterize the equation on page 16;
#     the equation only represents one possibility.
#     Thus, even if your equation in R or another program package looks different 
#     you might get exactly the same treatment effects (though different coefs for other predictors)
# (2) If you are not sure how to implement this model, it is a good idea to compute the dummies
#     and interaction terms on your own (instead of letting doing it the lm() function)
# (3) You should be able to reproduce the regression coefficients given in Table 7.
#     Your standard errors will be slightly smaller though.
#     If you want, try to estimate the total effect in 2011 (but there is no need to do it).
#     Table 7 only contains the coefs for the High vs. Low Standards contrast though the equation on p16
#     also estimates the Medium vs. Low Standards contrast simultaneously.
#     NOTE that Table 7 contains a typo: For 4th Grade Math the difference in slopes 
#     should be 0.57 instead of the reported 0.13.
# (4) The text just below the equation on p16 says that the model includes state- and time-fixed effects,
#     i.e.,suggesting that dummy variables are needed for states and time points.
# (5) You need to use the centered year variable 'year02', 
#     otherwise you don't get the correct intercept change
# (6) Once you suceeded in replicating the effects, think about why the specified model correctly estimates
#     the treatment effects of interest.



