getwd() #want it to be repro/R (repro) for me

library(assertr)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(boot)
library(mvnormtest)
library(ggplot2)
library(magrittr)
library(data.table)
library(smatr)
library(rstatix)

theme_set(theme_bw())

###############################################################
### Load in our main data sets ################################
###############################################################

SMI_zoos <- read.csv("processed data/2021SMI.csv", header = TRUE)
view(SMI_zoos) #contains wild frogs caught only in 2021
dim(SMI_zoos) #110 rows, 9 columns
SMI_zoos$EB <- as.factor(SMI_zoos$EB) #as factor. Two levels: 0=EB 1=not (OK)
SMI_zoos$pop <- as.factor(SMI_zoos$pop) #factor with 6 levels
SMI_zoos$locale <- as.factor(SMI_zoos$locale) #factor with 2 levels (Wild, Zoo)
#yr_birth <- as.Date(SMI_zoos$birth_yr, format = "%Y") # make new column or set birth_yr as numeric?
str(SMI_zoos)
summary(SMI_zoos)

mass_zoos <- read.csv("processed data/2021mass.csv", header = TRUE)
view(mass_zoos)
dim(mass_zoos) #64 rows, 6 columns
str(mass_zoos) #set EB as factor
mass_zoos$EB <- as.factor(mass_zoos$EB) #as factor. Two levels: 0=EB 1=not (OK)
summary(mass_zoos)

wild_SMI <- read.csv("processed data/FULLwildSMI.csv", header = TRUE)
view(wild_SMI) # data from all wild frogs caught from 2011-2022
dim(wild_SMI) #497 rows, 6 columns
wild_SMI$frog_id <- as.factor(wild_SMI$frog_id) #as factor. 491 levels (i.e. individual frogs)
wild_SMI$pop <- as.factor(wild_SMI$pop) #as factor. 5 levels: "CH" "MS" "MT" "MV" "ST"
str(wild_SMI)
summary(wild_SMI)

prepo <-read.csv("processed data/prepost_SMI.csv", header = TRUE) #have now added calculated svl (using linear regressions below)
view(prepo)
dim(prepo) #87 rows, 12 columns
prepo$pop <- as.factor(prepo$pop)
prepo$EB <- as.factor(prepo$EB)
prepo$birth_yr <- as.factor(prepo$birth_yr) #9 levels
str(prepo)
summary(prepo)
