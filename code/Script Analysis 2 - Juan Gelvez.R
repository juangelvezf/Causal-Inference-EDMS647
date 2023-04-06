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

packages = c("tidyverse","readxl","ggplot2","skimr","writexl","foreign")

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


