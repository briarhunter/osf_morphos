getwd()

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

theme_set(theme_bw())

SMI_zoos <- read.csv("2021SMI.csv", header = TRUE)
head(SMI_zoos)
view(SMI_zoos)
dim(SMI_zoos) #110 rows, 9 columns
str(SMI_zoos) #set EB as factor ##set birth_yr as date/numeric?
SMI_zoos$EB <- as.factor(SMI_zoos$EB) #as factor. Two levels: 0=EB 1=not (OK)
SMI_zoos$pop <- as.factor(SMI_zoos$pop) #factor with 6 levels
SMI_zoos$locale <- as.factor(SMI_zoos$locale) #factor with 2 levels (Wild, Zoo)
#yr_birth <- as.Date(SMI_zoos$birth_yr, format = "%Y") # make new column or set birth_yr as numeric?
summary(SMI_zoos)

mass_zoos <- read.csv("2021mass.csv", header = TRUE)
head(mass_zoos)
view(mass_zoos)
dim(mass_zoos) #64 rows, 6 columns
str(mass_zoos) #set EB as factor
mass_zoos$EB <- as.factor(mass_zoos$EB) #as factor. Two levels: 0=EB 1=not (OK)
summary(mass_zoos)

wild_SMI <- read.csv("2021wildSMI.csv", header = TRUE)
head(wild_SMI)
view(wild_SMI)
dim(wild_SMI) #502 rows, 5 columns
str(wild_SMI)
wild_SMI$frog_id <- as.factor(wild_SMI$frog_id) #as factor. 490 levels (i.e. individual frogs)
wild_SMI$pop <- as.factor(wild_SMI$pop) #as factor. 6 levels 
#should not have 6 levels. need to trim white space from "MS "
wild_SMI <- wild_SMI %>% 
  mutate(pop = str_trim(pop, side = "both"))
levels(wild_SMI$pop) #now has only 5 levels: "CH" "MS" "MT" "MV" "ST"
summary(wild_SMI)

prepo <-read.csv("prepost_SMI.csv", header = TRUE) #have now added calculated svl (using linear regressions below)
head(prepo)
view(prepo)
dim(prepo) #87 rows, 12 columns
prepo$pop <- as.factor(prepo$pop)
prepo$EB <- as.factor(prepo$EB)
prepo$birth_yr <- as.factor(prepo$birth_yr) #9 levels
str(prepo)
summary(prepo)

#visualize the data. looking for normalcy. 
hist(SMI_zoos$age_yrs, main = "OSF age")
hist(SMI_zoos$mass, main = "OSF mass")
hist(SMI_zoos$SVL, main = "OSF SVL")
#none looking super normal
hist(mass_zoos$mass_2021, main = "OSF mass 2021")
hist(mass_zoos$mass_2022, main = "OSF mass 2022") #looks fairly normal
#wild data
hist(wild_SMI$avg_mass, main = "wild mass")
hist(wild_SMI$avg_SVL, main = "wild SVL") #looks fairly normal

ggdensity(SMI_zoos$mass, main = "2021 Mass", xlab = "mass (g)")
ggdensity(wild_SMI$avg_mass, main = "mass for wild frogs", xlab = "mass (g)")
ggdensity(wild_SMI$avg_SVL, main = "SVL for wild frogs", xlab = "length (mm)")

#Shapiro-wilks test for normalcy 
shapiro.test(SMI_zoos$age_yrs)
shapiro.test(SMI_zoos$mass)
shapiro.test(SMI_zoos$SVL)

ggqqplot(SMI_zoos$age_yrs, na.rm = TRUE)
ggqqplot(SMI_zoos$mass) #looks the closest to normal distribution
ggqqplot(SMI_zoos$SVL)
#pre-post data
ggqqplot(prepo$mass_pre20, main = "qq-plot mass pre-brum 2020", na.rm = TRUE)
ggqqplot(prepo$svl_pre20, na.rm = TRUE)
ggqqplot(prepo$mass_post21, na.rm = TRUE)
ggqqplot(prepo$svl_post21, na.rm = TRUE)
ggqqplot(prepo$mass_pre_21, main = "qq-plot mass pre-brum 2021", na.rm = TRUE)
ggqqplot(prepo$svl_pre21, na.rm = TRUE) #these actually all look close to normal if not normal

plot(mass ~ SVL, data = SMI_zoos, main = "mass by SVL")
plot(svl_pre20 ~ mass_pre20, data = prepo, main = "svl by mass pre-brumation 2020")
plot(svl_post21 ~ mass_post21, data = prepo, main = "svl by mass post-brumation 2021")
plot(svl_pre21 ~ mass_pre_21, data = prepo, main = "svl by mass pre-brumation 2021")

ggplot(data = mass_zoos, aes(pop, mass_2021, #total hours in amplexus with status (ER, EB, OK)
                    colour = EB))+ #how to make EB a factor for this plot and/or permanently
  geom_bar()+ ##started trying to make box plot - fix
  labs(title = "OSF mass in 2021 by population") #should be a box-whiskers or box plot
###not working - also want to turn into box plot? 

#check mass_zoos data
ggdensity(mass_zoos$mass_2022, na.rm = TRUE)
ggdensity(mass_zoos$mass_2021, na.rm = TRUE)

shapiro.test(mass_zoos$mass_2022) #nearly but not quite normal - non-parametric distribution
shapiro.test(mass_zoos$mass_2021)

#shapiro for wild data
shapiro.test(wild_SMI$avg_mass)
shapiro.test(wild_SMI$avg_SVL)
#shapiro for pre-post data
shapiro.test(prepo$mass_pre20) #normal
shapiro.test(prepo$length_pre20)
shapiro.test(prepo$svl_pre20) #add calculated svl values to check for normalcy
shapiro.test(prepo$mass_post21)
shapiro.test(prepo$length_post21) #same as below
shapiro.test(prepo$svl_post21) #normal
shapiro.test(prepo$mass_pre_21) #normal
shapiro.test(prepo$svl_pre21) #nearly normal but not quite

#done visualizing. get your bootstraps fastened
#boostrap ci's for non-normal variables (aka all of them)

my_mean <- function(data, indices) { #create function to calculate mean
  return( mean(data[indices]))
}

set.seed(100) #reproducibility

#filter out NAs 
age_na <- SMI_zoos %>% 
  filter(!is.na(age_yrs))

age_boot <- boot(age_na$age_yrs, my_mean, 1000) #bootstrap the mean for age
age_boot 
plot(age_boot)
boot.ci(boot_out <- age_boot, #confidence intervals for age
        type = c("norm", "basic", "perc", "bca"))

mass_boot <- boot(SMI_zoos$mass, my_mean, 1000) 
mass_boot
plot(mass_boot)
boot.ci(boot_out <- mass_boot,
        type = c("norm", "basic", "perc", "bca"))

SVL_boot <- boot(SMI_zoos$SVL, my_mean, 1000)
SVL_boot 
plot(SVL_boot)
boot.ci(boot_out <- SVL_boot,
        type = c("norm", "basic", "perc", "bca"))

mass22_boot <- boot(mass_zoos$mass_2022, my_mean, 1000) 
mass22_boot
plot(mass22_boot)
boot.ci(boot_out <- mass22_boot,
        type = c("norm", "basic", "perc", "bca"))

mass21_boot <- boot(mass_zoos$mass_2021, my_mean, 1000) 
mass21_boot
plot(mass21_boot)
boot.ci(boot_out <- mass21_boot,
        type = c("norm", "basic", "perc", "bca"))

Wmass_boot <- boot(wild_SMI$avg_mass, my_mean, 1000) 
Wmass_boot
plot(Wmass_boot)
boot.ci(boot_out <- Wmass_boot,
        type = c("norm", "basic", "perc", "bca"))

Wsvl_boot <- boot(wild_SMI$avg_SVL, my_mean, 1000) 
Wsvl_boot
plot(Wsvl_boot)
boot.ci(boot_out <- Wsvl_boot,
        type = c("norm", "basic", "perc", "bca"))

prelen_boot <- boot(prepo$svl_pre20, my_mean, 1000) 
prelen_boot
plot(prelen_boot)
boot.ci(boot_out <- prelen_boot,
        type = c("norm", "basic", "perc", "bca"))

#filter out NAs
post_2021 <- prepo %>% 
  filter(!is.na(mass_post21))

pomass_boot <- boot(post_2021$mass_post21, my_mean, 1000) #filter out NA first
pomass_boot
plot(pomass_boot)
boot.ci(boot_out <- pomass_boot,
        type = c("norm", "basic", "perc", "bca"))

#filter out NAs
pre_2021 <- prepo %>% 
  filter(!is.na(svl_pre21))

pre_boot <- boot(pre_2021$svl_pre21, my_mean, 1000) 
pre_boot
plot(pre_boot)
boot.ci(boot_out <- pre_boot,
        type = c("norm", "basic", "perc", "bca"))

#for normal distribution (pre-brum mass 2020, pre-brum mass 2021)
prebru_m_mean <- mean(prepo$mass_pre20)
prebru_m_mean #45.89575
prebru_m_error <- qnorm(0.975)*sd(prepo$mass_pre20)/sqrt(length(prepo$mass_pre20))
prebru_m_error
left <- prebru_m_mean - prebru_m_error
right <- prebru_m_mean + prebru_m_error
print(c(left,right)) #(41.74126 50.05024)

#filter out NAs
po_2021 <- prepo %>% 
  filter(!is.na(svl_post21))

pobru_svl_mean <- mean(po_2021$svl_post21)
pobru_svl_mean #35.59268
pobru_svl_error <- qnorm(0.975)*sd(po_2021$svl_post21)/sqrt(length(po_2021$svl_post21))
pobru_svl_error
left <- pobru_svl_mean - pobru_svl_error
right <- pobru_svl_mean + pobru_svl_error
print(c(left,right)) #(32.58684, 38.59852)

prebru_mean <- mean(pre_2021$mass_pre_21) #use same NA filtered out dataset as svl_pre21
prebru_mean #52.5812
prebru_error <- qnorm(0.975)*sd(pre_2021$mass_pre_21)/sqrt(length(pre_2021$mass_pre_21))
prebru_error
left <- prebru_mean - prebru_error
right <- prebru_mean + prebru_error
print(c(left,right)) #(49.10911 56.05329)

##############################################################
####linear regressions to predict SVL for VanAqua SUL values
##############################################################
SULreg <- read.csv("SULregression.csv", header = TRUE) #only VanAqua frogs
head(SULreg)
view(SULreg)
dim(SULreg) #30 rows, 5 columns
str(SULreg)
summary(SULreg)
#eventually hopefully replace these SUL/SVL measurements with ones taken on same day from same individuals in triplicate
#for now using what we have (as of Sept 2022)

#linear regression of SVL by SUL - to predict SVL from SUL
lmSVL1 <- lm(SVL_spr2021 ~ SUL_2020, data = SULreg)
summary(lmSVL1)

lmSVL2 <- lm(SVL_Nov2021 ~ SUL_2020, data = SULreg)
summary(lmSVL2)
#plot variables again but add regression line
plot(SVL_spr2021 ~ SUL_2020 , data = SULreg, main = "2021 spring SVL by 2020 SUL") # n=30
abline(lmSVL1)
plot(lmSVL1$residuals, pch = 16, col = "red", main = "sprSVL ~ SUL residuals") #plot the residuals - do they look random?
plot(cooks.distance(lmSVL1), pch = 16, col = "blue", main = "sprSVL ~ SUL cooks distance") #plot cooks distance - see at least one point clearly not following pattern
#2017 WCEM04-05 has a spring SVL of 57.86mm which is much lower than most others - might be outlier? Check if recorded correctly

plot(SVL_Nov2021 ~ SUL_2020, data = SULreg, main = "2021 fall SVL by 2020 SUL") # n=20
abline(lmSVL2)
plot(lmSVL2$residuals, pch = 16, col = "red") #these residuals look random

#SVL_spr2021 on SUL is the better model, but still not great. 
#let's see if any transformations improve the model fit
ggplot(SULreg, aes(SUL_2020, SVL_spr2021))+
  geom_point()+
  scale_x_log10() + scale_y_log10() #doesn't look any different

#try a log transformation on the regression
lmSVL3 <- lm(log(SVL_spr2021) ~ log(SUL_2020), data = SULreg)
summary(lmSVL3) #improves the model but only slightly
plot(log(SVL_spr2021) ~ log(SUL_2020), data = SULreg, main = "2021 spring SVL by 2020 SUL") 
abline(lmSVL3)
plot(lmSVL3$residuals, pch = 16, col = "red", main = "sprSVL ~ SUL residuals") 
plot(cooks.distance(lmSVL3), pch = 16, col = "blue", main = "sprSVL ~ SUL cooks distance") #now we see two influential points...

#using non-transformed regression our equation is: SVL = 0.889x + 5.1677, R2 = 0.4573
#create predicted SVL column using equation and SUL values from SULreg
lmSVL1 <- lm(SVL_spr2021 ~ SUL_2020, data = SULreg)
summary(lmSVL1) #lmSVL1 is our model
predSVL <- data.frame(SUL_2020 = prepo[43:87, 6]) #turn SUL values into data frame so we can predict their SVL
predicted <- predict(lmSVL1, newdata = predSVL)
predSVL <- predSVL %>% 
  mutate(SUL_SVL = predicted) #predict SVL from previously defined SUL values

# prepo <- prepo %>% 
#   mutate(SUL_SVL = rbind(prepo[1:42, 6], predicted) %>% 
#   relocate(SUL_SVL, .after = 6)

####trying to bind GVZ SVL values (rows 1:42) to predicted VA values for 43:87 but not working
##may need to just edit data in excel and sort out reproducible method later

predSVL2 <- data.frame(SUL_2020 = prepo[43:87, 8]) #turn SUL 2021 values into data frame so we can predict their SVL
predicted21 <- predict(lmSVL1, newdata = predSVL2)
predSVL <- predSVL %>% 
  mutate(SUL21_SVL = predicted21) #added column of predicted 2021 SUL values from VanAqua
view(predSVL)

##############################################################
#linear regressions of mass by length
plot(mass_pre20 ~ svl_pre20, data = prepo, main = "mass by svl pre-brumation 2020")
plot(mass_post21 ~ svl_post21, data = prepo, main = "mass by svl post-brumation 2021")
plot(mass_pre_21 ~ svl_pre21, data = prepo, main = "mass by svl pre-brumation 2021")
plot(avg_mass ~ avg_SVL, data = wild_SMI, main = "mass by svl for wild frogs")

######################################################
#Use steph's code ################# Linear Regressions
######################################################

str(prepo)
#subset prepo data frame by populations: GVZ, VA
prepo.GVZ <- subset(prepo, pop == "GVZ")
str(prepo.GVZ)
prepo.VA <- subset(prepo, pop == "VA")
str(prepo.VA)

str(wild_SMI) #n=502
#subset wild data by populations
wild_SMI_CH <- subset(wild_SMI, pop =="CH") #n=7
wild_SMI_MS <- subset(wild_SMI, pop =="MS") #n=155
wild_SMI_MT <- subset(wild_SMI, pop =="MT") #n=1
wild_SMI_MV <- subset(wild_SMI, pop =="MV") #n=336
wild_SMI_ST <- subset(wild_SMI, pop =="ST") #n=3


### Mass (dependant) vs length (independent)
############################ Pre-brumation (i.e. Nov) 2020
plot(prepo$svl_pre20, prepo$mass_pre20, pch = 16, cex = 1.3,
     col = (c('blue', 'green')[as.numeric(prepo$pop)]),
     xlab = "snout-vent length (mm)", ylab = "mass (g)")
legend("bottomright", title = "Population", c("GVZoo", "VanAqua"), fill = c('blue', 'green'), cex = 0.8)
abline(lm(mass_pre20 ~ svl_pre20, data = prepo), col = "black")
lm(mass_pre20 ~ svl_pre20, data = prepo)
summary(lm(mass_pre20 ~ svl_pre20, data = prepo))
abline(lm(mass_pre20 ~ svl_pre20, data = prepo.GVZ), col = "blue")
summary(lm(mass_pre20 ~ svl_pre20, data = prepo.GVZ))
abline(lm(mass_pre20 ~ svl_pre20, data = prepo.VA), col = "green")
summary(lm(mass_pre20 ~ svl_pre20, data = prepo.VA))
legend("topleft", title = "Regression lines", c("n=87, Adjusted R-squared: 0.8325, p-value: 2.2e-16",
                                                "n=42, Adjusted R-squared: 0.6823, p-value: 9.996e-12",
                                                "n=45, Adjusted R-squared: 0.8839, p-value: 2.2e-16"),
       fill = c('black', 'blue', 'green'), cex = 0.8)
title("Pre-brumation (Nov) 2020")

#Log-transformation
prepo$log.svl_pre20 = log(prepo$svl_pre20)
prepo$log.mass_pre20 = log(prepo$mass_pre20)
prepo.GVZ$log.svl_pre20 = log(prepo.GVZ$svl_pre20)
prepo.VA$log.svl_pre20 = log(prepo.VA$svl_pre20)
prepo.GVZ$log.mass_pre20 = log(prepo.GVZ$mass_pre20)
prepo.VA$log.mass_pre20 = log(prepo.VA$mass_pre20)

### Log Mass vs Log Length for 
############################ Pre-brumation (i.e. Nov) 2020
plot(prepo$log.svl_pre20, prepo$log.mass_pre20, pch = 16, cex = 1.3,
     col = (c('blue', 'green')[as.numeric(prepo$pop)]),
     xlab = "Log transformed snout-vent length", ylab = "Log transformed mass")
legend("topleft", title = "Population", c("GVZoo", "VanAqua"), fill = c('blue', 'green'), cex = 0.8)
abline(lm(log.mass_pre20 ~ log.svl_pre20, data = prepo), col = "black")
lm(log.mass_pre20 ~ log.svl_pre20, data = prepo)
summary(lm(log.mass_pre20 ~ log.svl_pre20, data = prepo))
abline(lm(log.mass_pre20 ~ log.svl_pre20, data = prepo.GVZ), col = "blue")
summary(lm(log.mass_pre20 ~ log.svl_pre20, data = prepo.GVZ))
abline(lm(log.mass_pre20 ~ log.svl_pre20, data = prepo.VA), col = "green")
summary(lm(log.mass_pre20 ~ log.svl_pre20, data = prepo.VA))
legend("bottomright", title = "Regression lines", c("n=87, Adjusted R-squared: 0.9227, p-value: 2.2e-16",
                                                "n=42, Adjusted R-squared: 0.6915, p-value: 5.527e-12",
                                                "n=45, Adjusted R-squared: 0.9515, p-value: 2.2e-16"),
       fill = c('black', 'blue', 'green'), cex = 0.8)
title("Pre-brumation (Nov) 2020")

### Mass (dependant) vs length (independent)
############################ Post-brumation (i.e. Mar) 2021
plot(prepo$svl_post21, prepo$mass_post21, pch = 16, cex = 1.3,
     col = (c('blue', 'green')[as.numeric(prepo$pop)]),
     xlab = "snout-vent length (mm)", ylab = "mass (g)")
legend("bottomright", title = "Population", c("GVZoo", "VanAqua"), fill = c('blue', 'green'), cex = 0.8)
abline(lm(mass_post21 ~ svl_post21, data = prepo), col = "black")
lm(mass_post21 ~ svl_post21, data = prepo)
summary(lm(mass_post21 ~ svl_post21, data = prepo))
abline(lm(mass_post21 ~ svl_post21, data = prepo.GVZ), col = "blue")
summary(lm(mass_post21 ~ svl_post21, data = prepo.GVZ))
abline(lm(mass_post21 ~ svl_post21, data = prepo.VA), col = "green")
summary(lm(mass_post21 ~ svl_post21, data = prepo.VA))
legend("topleft", title = "Regression lines", c("n=55, Adjusted R-squared: 0.4238, p-value: 4.462e-08",
                                                "n=40, Adjusted R-squared: 0.2616, p-value: 0.0004406",
                                                "n=15, Adjusted R-squared: 0.7145, p-value: 4.422e-05"),
       fill = c('black', 'blue', 'green'), cex = 0.6)
title("Post-brumation (Mar) 2021")

#Log-transformation
prepo$log.svl_post21 = log(prepo$svl_post21)
prepo$log.mass_post21 = log(prepo$mass_post21)
prepo.GVZ$log.svl_post21 = log(prepo.GVZ$svl_post21)
prepo.VA$log.svl_post21 = log(prepo.VA$svl_post21)
prepo.GVZ$log.mass_post21 = log(prepo.GVZ$mass_post21)
prepo.VA$log.mass_post21 = log(prepo.VA$mass_post21)
wild_SMI$log.avg_mass = log(wild_SMI$avg_mass)
wild_SMI$log.avg_SVL = log(wild_SMI$avg_SVL)

### Log Mass vs Log Length for 
############################ Post-brumation (i.e. Mar) 2021
plot(prepo$log.svl_post21, prepo$log.mass_post21, pch = 16, cex = 1.3,
     col = (c('blue', 'green')[as.numeric(prepo$pop)]),
     xlab = "Log transformed snout-vent length", ylab = "Log transformed mass")
legend("bottomright", title = "Population", c("GVZoo", "VanAqua"), fill = c('blue', 'green'), cex = 0.8)
abline(lm(log.mass_post21 ~ log.svl_post21, data = prepo), col = "black")
lm(log.mass_post21 ~ log.svl_post21, data = prepo)
summary(lm(log.mass_post21 ~ log.svl_post21, data = prepo))
abline(lm(log.mass_post21 ~ log.svl_post21, data = prepo.GVZ), col = "blue")
summary(lm(log.mass_post21 ~ log.svl_post21, data = prepo.GVZ))
abline(lm(log.mass_post21 ~ log.svl_post21, data = prepo.VA), col = "green")
summary(lm(log.mass_post21 ~ log.svl_post21, data = prepo.VA))
legend("topleft", title = "Regression lines", c("n=55, Adjusted R-squared: 0.4555, p-value: 9.62e-09",
                                                    "n=40, Adjusted R-squared: 0.2462, p-value: 0.000668",
                                                    "n=15, Adjusted R-squared: 0.7341, p-value: 2.761e-05"),
       fill = c('black', 'blue', 'green'), cex = 0.6)
title("Post-brumation (Mar) 2021")


# Mass (dependant) vs length (independent)
############################ Pre-brumation (i.e. Nov) 2021
plot(prepo$svl_pre21, prepo$mass_pre_21, pch = 16, cex = 1.3,
     col = (c('blue', 'green')[as.numeric(prepo$pop)]),
     xlab = "snout-vent length (mm)", ylab = "mass (g)")
legend("topleft", title = "Population", c("GVZoo", "VanAqua"), fill = c('blue', 'green'), cex = 0.8)
abline(lm(mass_pre_21 ~ svl_pre21, data = prepo), col = "black")
lm(mass_pre_21 ~ svl_pre21, data = prepo)
summary(lm(mass_pre_21 ~ svl_pre21, data = prepo))
abline(lm(mass_pre_21 ~ svl_pre21, data = prepo.GVZ), col = "blue")
summary(lm(mass_pre_21 ~ svl_pre21, data = prepo.GVZ))
abline(lm(mass_pre_21 ~ svl_pre21, data = prepo.VA), col = "green")
summary(lm(mass_pre_21 ~ svl_pre21, data = prepo.VA))
legend("bottomright", title = "Regression lines", c("n=75, Adjusted R-squared: 0.3753, p-value: 3.116e-09",
                                                "n=40, Adjusted R-squared: 0.0288, p-value: 0.1502",
                                                "n=35, Adjusted R-squared: 0.6113, p-value: 1.78e-08"),
       fill = c('black', 'blue', 'green'), cex = 0.7)
title("Pre-brumation (Nov) 2021")

#Log-transformation
prepo$log.svl_pre21 = log(prepo$svl_pre21)
prepo$log.mass_pre_21 = log(prepo$mass_pre_21)
prepo.GVZ$log.svl_pre21 = log(prepo.GVZ$svl_pre21)
prepo.VA$log.svl_pre21 = log(prepo.VA$svl_pre21)
prepo.GVZ$log.mass_pre_21 = log(prepo.GVZ$mass_pre_21)
prepo.VA$log.mass_pre_21 = log(prepo.VA$mass_pre_21)

# Log Mass vs Log Length for 
############################ Pre-brumation (i.e. Nov) 2021
plot(prepo$log.svl_pre21, prepo$log.mass_pre_21, pch = 16, cex = 1.3,
     col = (c('blue', 'green')[as.numeric(prepo$pop)]),
     xlab = "Log transformed snout-vent length", ylab = "Log transformed mass")
legend("topleft", title = "Population", c("GVZoo", "VanAqua"), fill = c('blue', 'green'), cex = 0.8)
abline(lm(log.mass_pre_21 ~ log.svl_pre21, data = prepo), col = "black")
lm(log.mass_pre_21 ~ log.svl_pre21, data = prepo)
summary(lm(log.mass_pre_21 ~ log.svl_pre21, data = prepo))
abline(lm(log.mass_pre_21 ~ log.svl_pre21, data = prepo.GVZ), col = "blue")
summary(lm(log.mass_pre_21 ~ log.svl_pre21, data = prepo.GVZ))
abline(lm(log.mass_pre_21 ~ log.svl_pre21, data = prepo.VA), col = "green")
summary(lm(log.mass_pre_21 ~ log.svl_pre21, data = prepo.VA))
legend("bottomright", title = "Regression lines", c("n=75, Adjusted R-squared: 0.4983, p-value: 9.185e-13",
                                                    "n=40, Adjusted R-squared: 0.02358, p-value: 1716",
                                                    "n=35, Adjusted R-squared: 0.7226, p-value: 6.32e-11"),
       fill = c('black', 'blue', 'green'), cex = 0.6)
title("Pre-brumation (Nov) 2021")

##################################### The pre-brum 2020 regression is the best (highest R-squared) for GVZoo
### Will use this regression to calculate GVZoo's own bsma #################################################
#################### Will use larger VA dataset to calculate bsma for that population ######################

# Mass (dependant) vs length (independent)
############################ Wild Frog Data - full dataset (from 2012-2022*)
plot(wild_SMI$avg_SVL, wild_SMI$avg_mass, pch = 16, cex = 1.0,
     col = (c('blue', 'green', 'purple', 'orange', 'red')[as.numeric(wild_SMI$pop)]),
     xlab = "snout-vent length (mm)", ylab = "mass (g)")
legend("bottomright", title = "Population", c("Chaplin", "Maria Slough", "Mountain Slough", "Morris Valley", "Semmihault"),
       fill = c('blue', 'green', 'purple', 'orange', 'red'), cex = 0.6)
abline(lm(avg_mass ~ avg_SVL, data = wild_SMI), col = "black")
lm(avg_mass ~ avg_SVL, data = wild_SMI)
summary(lm(avg_mass ~ avg_SVL, data = wild_SMI))
abline(lm(avg_mass ~ avg_SVL, data = wild_SMI_CH), col = "blue")
summary(lm(avg_mass ~ avg_SVL, data = wild_SMI_CH))
abline(lm(avg_mass ~ avg_SVL, data = wild_SMI_MS), col = "green")
summary(lm(avg_mass ~ avg_SVL, data = wild_SMI_MS))
abline(lm(avg_mass ~ avg_SVL, data = wild_SMI_MV), col = "orange")
summary(lm(avg_mass ~ avg_SVL, data = wild_SMI_MV))
abline(lm(avg_mass ~ avg_SVL, data = wild_SMI_ST), col = "red")
summary(lm(avg_mass ~ avg_SVL, data = wild_SMI_ST))
#no abline for MT pop because only 1 observation
legend("topright", title = "Regression lines", c("n=502, Adjusted R-squared: 0.5563, p-value: 2.2e-16",
                                                    "n=7, Adjusted R-squared: 0.3608, p-value: 0.0904",
                                                    "n=155, Adjusted R-squared: 0.6485, p-value: 2.2e-16",
                                                    "n=336, Adjusted R-squared: 0.5205, p-value: 2.2e-16",
                                                    "n=3, Adjusted R-squared: 0.7963, p-value: 0.2068"),
       fill = c('black', 'blue', 'green', 'orange', 'red'), cex = 0.45)
title("Full Wild M by L")

#Log-transformation
wild_SMI$log.avg_SVL = log(wild_SMI$avg_SVL)
wild_SMI$log.avg_mass = log(wild_SMI$avg_mass)
wild_SMI_CH$log.avg_SVL = log(wild_SMI_CH$avg_SVL)
wild_SMI_MS$log.avg_SVL = log(wild_SMI_MS$avg_SVL)
wild_SMI_MV$log.avg_SVL = log(wild_SMI_MV$avg_SVL)
wild_SMI_ST$log.avg_SVL = log(wild_SMI_ST$avg_SVL)
wild_SMI_CH$log.avg_mass = log(wild_SMI_CH$avg_mass)
wild_SMI_MS$log.avg_mass = log(wild_SMI_MS$avg_mass)
wild_SMI_MV$log.avg_mass = log(wild_SMI_MV$avg_mass)
wild_SMI_ST$log.avg_mass = log(wild_SMI_ST$avg_mass)

# Log Mass vs Log Length for 
############################ Wild Frog Data - full dataset (from 2012-2022*)
plot(wild_SMI$log.avg_SVL, wild_SMI$log.avg_mass, pch = 16, cex = 1.0,
     col = (c('blue', 'green', 'purple', 'orange', 'red')[as.numeric(wild_SMI$pop)]),
     xlab = "snout-vent length (mm)", ylab = "mass (g)")
legend("topleft", title = "Population", c("Chaplin", "Maria Slough", "Mountain Slough", "Morris Valley", "Semmihault"),
       fill = c('blue', 'green', 'purple', 'orange', 'red'), cex = 0.6)
abline(lm(log.avg_mass ~ log.avg_SVL, data = wild_SMI), col = "black")
lm(log.avg_mass ~ log.avg_SVL, data = wild_SMI)
summary(lm(log.avg_mass ~ log.avg_SVL, data = wild_SMI))
abline(lm(log.avg_mass ~ log.avg_SVL, data = wild_SMI_CH), col = "blue")
summary(lm(log.avg_mass ~ log.avg_SVL, data = wild_SMI_CH))
abline(lm(log.avg_mass ~ log.avg_SVL, data = wild_SMI_MS), col = "green")
summary(lm(log.avg_mass ~ log.avg_SVL, data = wild_SMI_MS))
abline(lm(log.avg_mass ~ log.avg_SVL, data = wild_SMI_MV), col = "orange")
summary(lm(log.avg_mass ~ log.avg_SVL, data = wild_SMI_MV))
abline(lm(log.avg_mass ~ log.avg_SVL, data = wild_SMI_ST), col = "red")
summary(lm(log.avg_mass ~ log.avg_SVL, data = wild_SMI_ST))
#no abline for MT pop because only 1 observation
legend("bottomright", title = "Regression lines", c("n=502, Adjusted R-squared: 0.5931, p-value: 2.2e-16",
                                                 "n=7, Adjusted R-squared: 0.2854, p-value: 0.1247",
                                                 "n=155, Adjusted R-squared: 0.6651, p-value: 2.2e-16",
                                                 "n=336, Adjusted R-squared: 0.5663, p-value: 2.2e-16",
                                                 "n=3, Adjusted R-squared: 0.7893, p-value: 0.2105"),
       fill = c('black', 'blue', 'green', 'orange', 'red'), cex = 0.5)
title("Log of M by L Full Wild dataset")
########## This graph doesn't look normal. need to come back to and figure out what's going wrong #####

################################################################################
# Calculate scaled mass index (SMI)
################################################################################

# Scaled mass index, Pieg & Green 2009

# Scaled mass index (SMI): ^Mi = Mi (Lo/Li)^bsma
# where: ^Mi is scaled mass: predicted body mass for individual i when the linear body measure is standardized to L0.
#         Mi is mass of individual i
#         Lo is an arbitraty value of length (e.g. arithmetric mean of L for the population of study)
#         Li is the linear body measurement of individual i
#         bsma is the scaling exponent: calculated indirectly by dividing the slope from an OLS regression (bOLS) 
#by the Pearson’s correlation coefficient r (LaBarbera 1989), or directly using online software (Bohonak and van der Linde 2004).
# calculate a different bsma for each population (VA, TZ, GVZ, wild) to account for different growth patterns in each

# calculate bsma manually (described in Pieg & Green 2009 p.1886)
#for Wild frogs - using wild allometric growth as standard for scaling exponent
lm(wild_SMI$log.avg_mass ~ wild_SMI$log.avg_SVL) #using log
slope <- 2.336 #from variable above
cor.test(wild_SMI$log.avg_SVL, wild_SMI$log.avg_mass, method = c("pearson")) #using log again 
pcorr <- 0.7706459 #from above (Pearson's product-moment correlation)
bsma.w <- slope / pcorr #manual calculation of bsma (scaling exponent)
bsma.w #3.031224

#calculate bsma again but this time automated using smatr() - Standardized Major Axis
library(smatr)
sma(wild_SMI$log.avg_mass ~ wild_SMI$log.avg_SVL) #use log #the slope of the estimate is the bsma 
bsma.w <- 3.031213

##calculate bsma for each GVZoo based on pre20 data - the best regression for GVZ
##### each population have their own scaling exponent - VA will be calculated later
sma(prepo.GVZ$log.mass_pre20 ~ prepo.GVZ$log.svl_pre20)
bsma.GVZ <- 3.183815

#calculate Lo
Lo <- mean(wild_SMI$avg_SVL, na.rm = TRUE)
Lo #71.37092

Lo.GVZ <- mean(prepo.GVZ$svl_pre20, na.rm = TRUE)
Lo.GVZ #67.71429

Lo.VA <- mean(prepo.VA$svl_pre20, na.rm = TRUE)
Lo.VA #64.59044

#######create function for ScaledMassIndex using ^Mi = Mi ((Lo/Li)^bsma)
##using bsma from MbyL regression of wild frog data
ScaledMassIndex.w <-
  function(x, y) {
    y * ((Lo / x) ^ bsma.w)
  }

wild_SMI$SMI <- ScaledMassIndex.w(wild_SMI$avg_SVL, wild_SMI$avg_mass)
head(wild_SMI)

#### make SMI equation using GVZ bsma - but keep same Lo.w because this is arbitrary chosen length
ScaledMassIndex.gvz <-
  function(x, y) {
    y * ((Lo / x) ^ bsma.GVZ)
  }

### calculate bsma for VA using smatr() - Standardized Major Axis ###
sma(VA_age$log.mass ~ VA_age$log.svl) #use log #the slope of the estimate is the bsma 
VA.bsma <- 3.949900
#check if different if just using VA adults
sma(VA_adults2$log.mass ~ VA_adults2$log.svl)
VA.a.bsma <- 3.202558
#wild bsma was 3.0312
#bsma from VA adults is closer to this and closer to slope of 3 in general (3 supposed to be best?)
########################
#calculate Lo
Lo.VA <- mean(VA_age$svl, na.rm = TRUE)
Lo.VA #55.09706
Lo.VA.a <- mean(VA_adults2$svl, na.rm = TRUE)
Lo.VA.a #64.98735
#wild Lo was 71.3709 - but the Lo should be arbitrary

########Create new SMI functions for new bsma using old Lo
ScaledMassIndex.VA <- function(x, y) {
  y * ((Lo / x) ^ VA.bsma)
}

ScaledMassIndex.va <- function(x, y) {
  y * ((Lo / x) ^ VA.a.bsma)
}

#calculate SMI for GVZ and VA data in "prepo" data frame using wild bsma
prepo$SMIpre20 <- ScaledMassIndex.w(prepo$svl_pre20, prepo$mass_pre20)
prepo$SMIpo21 <- ScaledMassIndex.w(prepo$svl_post21, prepo$mass_post21)
prepo$SMIpre21 <- ScaledMassIndex.w(prepo$svl_pre21, prepo$mass_pre_21)
view(prepo)

#plot SMI
######################################################
#GRAPHING#############################################
plot(wild_SMI$pop, wild_SMI$SMI, xlab = "Population", ylab = "Scaled Mass Index (g)", 
     main = "Scaled Mass Index for wild frog populations",
     col = (c("blue", "green", "purple", "orange", "red")))

plot(prepo$pop, prepo$SMIpre20, xlab = "Population", ylab = "Scaled Mass Index (g)",
     main = "SMI for zoos pre-brumation (Nov) 2020",
     col = (c("blue", "green")))
plot(prepo$pop, prepo$SMIpo21, xlab = "Population", ylab = "Scaled Mass Index (g)",
     main = "SMI for zoos post-brumation (Mar) 2021",
     col = (c("blue", "green")))
plot(prepo$pop, prepo$SMIpre21, xlab = "Population", ylab = "Scaled Mass Index (g)",
     main = "SMI for zoos pre-brumation (Nov) 2021",
     col = (c("blue", "green")))

#nicer plots
ggplot(data = wild_SMI, aes(x = pop, y = SMI, fill=factor(pop, labels = c("Chaplin", "Maria Slough", "Mountain Slough",
                                                                          "Morris Valley", "Semmihault"))))+
  geom_boxplot(show.legend = FALSE)+
  labs(x = "Population", y = "Scaled Mass Index (g)", title = "SMI for wild populations")+
  theme_classic()+
  scale_fill_manual(values = c("blue", "green", "purple", "orange", "red"))+
  theme(legend.title = element_blank())+
  scale_x_discrete(labels=c("CH" = "Chaplin", "MS" = "Maria Slough", "MT" = "Mountain", "MV" = "Morris Valley", "ST" = "Semmihault"))+
  theme(text = element_text(family = "Arial"))+
  theme(text = element_text(size = 12))

ggplot(data = prepo, aes(x = pop, y = SMIpre20, fill=factor(pop, labels = c("GVZ", "VA"))))+
  geom_boxplot(show.legend = FALSE)+
  labs(x = "Population", y = "Scaled Mass Index (g)", title = "SMI in zoos pre-brumation 2020")+
  theme_classic()+
  scale_fill_manual(values = c("blue", "green"))+
  theme(legend.title = element_blank())+
  scale_x_discrete(labels=c("GVZ" = "Greater Vancouver Zoo", "VA" = "Vancouer Aquarium"))+
  theme(text = element_text(family = "Arial"))+
  theme(text = element_text(size = 12))

ggplot(data = prepo, aes(x = pop, y = SMIpo21, fill=factor(pop, labels = c("GVZ", "VA"))))+
  geom_boxplot(show.legend = FALSE)+
  labs(x = "Population", y = "Scaled Mass Index (g)", title = "SMI in zoos post-brumation 2021")+
  theme_classic()+
  scale_fill_manual(values = c("blue", "green"))+
  theme(legend.title = element_blank())+
  scale_x_discrete(labels=c("GVZ" = "Greater Vancouver Zoo", "VA" = "Vancouer Aquarium"))+
  theme(text = element_text(family = "Arial"))+
  theme(text = element_text(size = 12))

ggplot(data = prepo, aes(x = pop, y = SMIpre21, fill=factor(pop, labels = c("GVZ", "VA"))))+
  geom_boxplot(show.legend = FALSE)+
  labs(x = "Population", y = "Scaled Mass Index (g)", title = "SMI in zoos pre-brumation 2021")+
  theme_classic()+
  scale_fill_manual(values = c("blue", "green"))+
  theme(legend.title = element_blank())+
  scale_x_discrete(labels=c("GVZ" = "Greater Vancouver Zoo", "VA" = "Vancouer Aquarium"))+
  theme(text = element_text(family = "Arial"))+
  theme(text = element_text(size = 12))

#subset for age
prepo_age <- subset(prepo, !is.na(birth_yr))
ggplot(data = prepo_age, aes(x = birth_yr, y = SMIpre21, fill=factor(pop, labels = c("GVZ", "VA"))))+
  geom_boxplot(show.legend = TRUE)+
  labs(x = "Birth Year", y = "Scaled Mass Index (g)", title = "SMI in zoos pre-brumation 2021")+
  theme_classic()+
  scale_fill_manual(values = c("blue", "green"))+
  scale_fill_discrete(name = "Zoo")+
  theme(legend.position = "top")+
  theme(text = element_text(family = "Arial"))+
  theme(text = element_text(size = 12))

  ggplot(data = prepo_age, aes(x = birth_yr, y = SMIpre20, fill=factor(pop, labels = c("GVZ", "VA"))))+
    geom_boxplot(show.legend = TRUE)+
    labs(x = "Birth Year", y = "Scaled Mass Index (g)", title = "SMI in zoos pre-brumation 2020")+
    theme_classic()+
    scale_fill_manual(values = c("blue", "green"))+
    scale_fill_discrete(name = "Zoo")+
    theme(legend.position = "top")+
    theme(text = element_text(family = "Arial"))+
    theme(text = element_text(size = 12))

  
##show by status and facet.by pop
ggboxplot(prepo, x = "EB", y = "SMIpre20", facet.by = "pop")+
  labs(x = "Status", y = "Scaled Mass Index (g)", title = "SMI by status in fall 2020")+
  scale_x_discrete(labels=c("0" = "egg bound", "1" = "OK"))

ggboxplot(prepo, x = "EB", y = "SMIpo21", facet.by = "pop")+
  labs(x = "Status", y = "Scaled Mass Index (g)", title = "SMI by status in spring 2021")+
  scale_x_discrete(labels=c("0" = "egg bound", "1" = "OK"))

ggboxplot(prepo, x = "EB", y = "SMIpre21", facet.by = "pop")+
  labs(x = "Status", y = "Scaled Mass Index (g)", title = "SMI by status in fall 2021")+
  scale_x_discrete(labels=c("0" = "egg bound", "1" = "OK"))


###############change data frame to stacked - columns for season (pre-brumate, post-brumate), WC (Wild or Captive)
###will then be able to compare all on same graph using facet.by()
# ggboxplot(wild_SMI, x = "season", y = "SMI", facet.by = "population")
####################################################################################################
###############subset prepo to just mass and svl and transform to long / stacked form ##############
####################################################################################################
prepo.long <- prepo[, c(1:5, 7:8, 10:12)]
prepo.l <- melt(setDT(prepo.long), id=1:4, measure=patterns("^mass", "^svl"),
               value.name=c("mass", "svl"), variable.name="when", na.rm = FALSE)
prepo.l <- prepo.l %>% 
  mutate(season = case_when(when == "1" ~ "pre20", when == "2" ~ "post21", when == "3" ~ "pre21")) %>%
  relocate(season, .after = EB) 
prepo.l$when <- NULL #drop column now that we have proper season
view(prepo.l)

################# subset out GVZoo and VA into own dataframes to calculate separate SMI's with own bsma ########
prepo.gvz <- subset(prepo.l, pop == "GVZ")
prepo.va <- subset(prepo.l, pop == "VA")

############################################################# Adding in missing bits ######################

#########################################################################################
# Calculate SMI for full 2021 snapshot of zoos and wild##################################
# use SMI equations from above with bsma for each individual pop (need one for TZ still)#
#########################################################################################
# Regressions first

str(SMI_zoos)
#subset SMI_zoos data frame by populations: CH, GVZ, MS, MV, TZ, VA
SMI_zoos.CH <- subset(SMI_zoos, pop == "CH")
str(SMI_zoos.CH) #n=7
SMI_zoos.GVZ <- subset(SMI_zoos, pop == "GVZ")
str(SMI_zoos.GVZ) #n=41
SMI_zoos.MS <- subset(SMI_zoos, pop == "MS")
str(SMI_zoos.MS) #n=25
SMI_zoos.MV <- subset(SMI_zoos, pop == "MV")
str(SMI_zoos.MV) #n=13
SMI_zoos.TZ <- subset(SMI_zoos, pop == "TZ")
str(SMI_zoos.TZ) #n=9
SMI_zoos.VA <- subset(SMI_zoos, pop == "VA")
str(SMI_zoos.VA) #n=15
#subset by locale (zoos vs wild)
SMI_zoos.z <- subset(SMI_zoos, locale == "Zoo")
str(SMI_zoos.z) #n=65
SMI_zoos.w <- subset(SMI_zoos, locale == "Wild")
str(SMI_zoos.w) #n=45


### Mass (dependant) vs length (independent)
############################ 
plot(SMI_zoos$SVL, SMI_zoos$mass, pch = 16, cex = 1.3,
     col = (c('blue', 'purple', 'green', 'orange', 'yellow', 'red')[as.numeric(SMI_zoos$pop)]),
     xlab = "snout-vent length (mm)", ylab = "mass (g)")
legend("bottomright", title = "Population", c("Chaplin", "GVZoo", "Maria Slough", "Morris Valley", "TZoo", "VanAqua"), 
       fill = c('blue', 'purple', 'green', 'orange', 'yellow', 'red'), cex = 0.6)
abline(lm(mass ~ SVL, data = SMI_zoos), col = "black")
lm(mass ~ SVL, data = SMI_zoos)
summary(lm(mass ~ SVL, data = SMI_zoos))
abline(lm(mass ~ SVL, data = SMI_zoos.CH), col = "blue")
summary(lm(mass ~ SVL, data = SMI_zoos.CH))
abline(lm(mass ~ SVL, data = SMI_zoos.GVZ), col = "purple")
summary(lm(mass ~ SVL, data = SMI_zoos.GVZ))
abline(lm(mass ~ SVL, data = SMI_zoos.MS), col = "green")
summary(lm(mass ~ SVL, data = SMI_zoos.MS))
abline(lm(mass ~ SVL, data = SMI_zoos.MV), col = "orange")
summary(lm(mass ~ SVL, data = SMI_zoos.MV))
abline(lm(mass ~ SVL, data = SMI_zoos.TZ), col = "yellow")
summary(lm(mass ~ SVL, data = SMI_zoos.TZ))
abline(lm(mass ~ SVL, data = SMI_zoos.VA), col = "red")
summary(lm(mass ~ SVL, data = SMI_zoos.VA))
legend("topleft", title = "Regression lines", c("n=7, Adjusted R-squared: 0.5825, p-value: 0.02806",
                                                "n=41, Adjusted R-squared: 0.252, p-value: 0.000489",
                                                "n=25, Adjusted R-squared: 0.6651, p-value: 4.14e-07",
                                                "n=13, Adjusted R-squared: 0.6776, p-value: 0.0003331",
                                                "n=9, Adjusted R-squared: 0.4312, p-value: 0.03256",
                                                "n=15, Adjusted R-squared: 0.7145, p-value: 4.422e-05"),
       fill = c('blue', 'purple', 'green', 'orange', 'yellow', 'red'), cex = 0.6)
title("2021 spring Mass by SVL")

#Log-transformation
SMI_zoos$log.SVL = log(SMI_zoos$SVL)
SMI_zoos$log.mass = log(SMI_zoos$mass)
SMI_zoos.CH$log.SVL = log(SMI_zoos.CH$SVL)
SMI_zoos.CH$log.mass = log(SMI_zoos.CH$mass)
SMI_zoos.GVZ$log.SVL = log(SMI_zoos.GVZ$SVL)
SMI_zoos.GVZ$log.mass = log(SMI_zoos.GVZ$mass)
SMI_zoos.MS$log.SVL = log(SMI_zoos.MS$SVL)
SMI_zoos.MS$log.mass = log(SMI_zoos.MS$mass)
SMI_zoos.MV$log.SVL = log(SMI_zoos.MV$SVL)
SMI_zoos.MV$log.mass = log(SMI_zoos.MV$mass)
SMI_zoos.TZ$log.SVL = log(SMI_zoos.TZ$SVL)
SMI_zoos.TZ$log.mass = log(SMI_zoos.TZ$mass)
SMI_zoos.VA$log.SVL = log(SMI_zoos.VA$SVL)
SMI_zoos.VA$log.mass = log(SMI_zoos.VA$mass)

# Log Mass vs Log Length
############################ 
plot(SMI_zoos$log.SVL, SMI_zoos$log.mass, pch = 16, cex = 1.3,
     col = (c('blue', 'purple', 'green', 'orange', 'yellow', 'red')[as.numeric(SMI_zoos$pop)]),
     xlab = "Log transformed snout-vent length", ylab = "Log transformed mass (g)")
legend("bottomright", title = "Population", c("Chaplin", "GVZoo", "Maria Slough", "Morris Valley", "TZoo", "VanAqua"), 
       fill = c('blue', 'purple', 'green', 'orange', 'yellow', 'red'), cex = 0.6)
abline(lm(log.mass ~ log.SVL, data = SMI_zoos), col = "black")
lm(log.mass ~ log.SVL, data = SMI_zoos)
summary(lm(log.mass ~ log.SVL, data = SMI_zoos))
abline(lm(log.mass ~ log.SVL, data = SMI_zoos.CH), col = "blue")
summary(lm(log.mass ~ log.SVL, data = SMI_zoos.CH))
abline(lm(log.mass ~ log.SVL, data = SMI_zoos.GVZ), col = "purple")
summary(lm(log.mass ~ log.SVL, data = SMI_zoos.GVZ))
abline(lm(log.mass ~ log.SVL, data = SMI_zoos.MS), col = "green")
summary(lm(log.mass ~ log.SVL, data = SMI_zoos.MS))
abline(lm(log.mass ~ log.SVL, data = SMI_zoos.MV), col = "orange")
summary(lm(log.mass ~ log.SVL, data = SMI_zoos.MV))
abline(lm(log.mass ~ log.SVL, data = SMI_zoos.TZ), col = "yellow")
summary(lm(log.mass ~ log.SVL, data = SMI_zoos.TZ))
abline(lm(log.mass ~ log.SVL, data = SMI_zoos.VA), col = "red")
summary(lm(log.mass ~ log.SVL, data = SMI_zoos.VA))
legend("topleft", title = "Regression lines", c("n=7, Adjusted R-squared: 0.5935, p-value: 0.02613",
                                                "n=41, Adjusted R-squared: 0.2388, p-value: 0.0007024",
                                                "n=25, Adjusted R-squared: 0.6846, p-value: 2.048e-07",
                                                "n=13, Adjusted R-squared: 0.6446, p-value: 0.0005799",
                                                "n=9, Adjusted R-squared: 0.4231, p-value: 0.03438",
                                                "n=15, Adjusted R-squared: 0.7345, p-value: 2.73e-05"),
       fill = c('blue', 'purple', 'green', 'orange', 'yellow', 'red'), cex = 0.5)
title("Log of MbyL spring 2021")

#################the un-transformed regression is the best R-squared for TZoo (tho still not very good) 
## use it for calculating bsma
sma(SMI_zoos.TZ$log.mass ~ SMI_zoos.TZ$log.SVL)
bsma.tz <- 2.379120

ScaledMassIndex.tz <-
  function(x, y) {
    y * ((Lo / x) ^ bsma.tz)
  }


#########Try Log Mass by SVL divided by Zoo vs Wild (rather than pop) #####################
#log transformation
SMI_zoos.w$log.SVL = log(SMI_zoos.w$SVL)
SMI_zoos.w$log.mass = log(SMI_zoos.w$mass)
SMI_zoos.z$log.SVL = log(SMI_zoos.z$SVL)
SMI_zoos.z$log.mass = log(SMI_zoos.z$mass)

plot(SMI_zoos$log.SVL, SMI_zoos$log.mass, pch = 16, cex = 1.3,
     col = (c('blue', 'purple')[as.numeric(SMI_zoos$locale)]),
     xlab = "Log transformed snout-vent length", ylab = "Log transformed mass (g)")
legend("bottomright", title = "Current Location", c("Wild", "Zoo"), 
       fill = c('blue', 'purple'), cex = 0.7)
abline(lm(log.mass ~ log.SVL, data = SMI_zoos), col = "black")
lm(log.mass ~ log.SVL, data = SMI_zoos)
summary(lm(log.mass ~ log.SVL, data = SMI_zoos))
abline(lm(log.mass ~ log.SVL, data = SMI_zoos.w), col = "blue")
summary(lm(log.mass ~ log.SVL, data = SMI_zoos.w))
abline(lm(log.mass ~ log.SVL, data = SMI_zoos.z), col = "purple")
summary(lm(log.mass ~ log.SVL, data = SMI_zoos.z))
legend("topleft", title = "Regression lines", c("n=45, Adjusted R-squared: 0.5984, p-value: 2.823e-10",
                                                "n=65, Adjusted R-squared: 0.3304, p-value: 3.307e-07"),
       fill = c('blue', 'purple'), cex = 0.6)
title("Log of MbyL spring 2021")

### Log of wild and 3 zoos #####################
plot(SMI_zoos$log.SVL, SMI_zoos$log.mass, pch = 16, cex = 1.3,
     col = (c('blue', 'purple', 'blue', 'blue', 'yellow', 'red')[as.numeric(SMI_zoos$pop)]),
     xlab = "Log transformed snout-vent length", ylab = "Log transformed mass (g)")
legend("bottomright", title = "Population", c("Wild", "GVZoo", "TZoo", "VanAqua"), 
       fill = c('blue', 'purple', 'yellow', 'red'), cex = 0.7)
abline(lm(log.mass ~ log.SVL, data = SMI_zoos), col = "black")
lm(log.mass ~ log.SVL, data = SMI_zoos)
summary(lm(log.mass ~ log.SVL, data = SMI_zoos))
abline(lm(log.mass ~ log.SVL, data = SMI_zoos.w), col = "blue")
summary(lm(log.mass ~ log.SVL, data = SMI_zoos.w))
abline(lm(log.mass ~ log.SVL, data = SMI_zoos.GVZ), col = "purple")
summary(lm(log.mass ~ log.SVL, data = SMI_zoos.GVZ))
abline(lm(log.mass ~ log.SVL, data = SMI_zoos.TZ), col = "yellow")
summary(lm(log.mass ~ log.SVL, data = SMI_zoos.TZ))
abline(lm(log.mass ~ log.SVL, data = SMI_zoos.VA), col = "red")
summary(lm(log.mass ~ log.SVL, data = SMI_zoos.VA))
legend("topleft", title = "Regression lines", c("n=45, Adjusted R-squared: 0.5984, p-value: 2.823e-10",
                                                "n=41, Adjusted R-squared: 0.2388, p-value: 0.0007024",
                                                "n=9, Adjusted R-squared: 0.4231, p-value: 0.03438",
                                                "n=15, Adjusted R-squared: 0.7345, p-value: 2.73e-05"),
       fill = c('blue', 'purple', 'yellow', 'red'), cex = 0.6)
title("Log of MbyL spring 2021")

#############################################################################################
### Calculate SMI for 2021 data using Lo and bsma(s) defined previously - unique bsma per pop
#############################################################################################
## use subset data and combine back together at end 
####################################################################

SMI_zoos.w$SMI <- ScaledMassIndex.w(SMI_zoos.w$SVL, SMI_zoos.w$mass)
view(SMI_zoos.w)
SMI_zoos.GVZ$SMI <- ScaledMassIndex.gvz(SMI_zoos.GVZ$SVL, SMI_zoos.GVZ$mass)
view(SMI_zoos.GVZ)
SMI_zoos.TZ$SMI <- ScaledMassIndex.tz(SMI_zoos.TZ$SVL, SMI_zoos.TZ$mass)
view(SMI_zoos.TZ)
SMI_zoos.VA$SMI <- ScaledMassIndex.VA(SMI_zoos.VA$SVL, SMI_zoos.VA$mass)
view(SMI_zoos.VA)
###### rbind all these subsets with their unique SMI values
full_21 <- rbind(SMI_zoos.w, SMI_zoos.GVZ, SMI_zoos.TZ, SMI_zoos.VA)
view(full_21)


######################################################################################################
# plot SMI ####################################################################
ggplot(data = full_21, aes(x = pop, y = SMI, fill=factor(pop, labels = c("Chaplin", "GVZoo", "Maria Slough",
                                                                         "Morris Valley", "TZoo", "VanAqua"))))+
  geom_boxplot(show.legend = FALSE)+
  labs(x = "Population", y = "Scaled Mass Index (g)", title = "SMI in spring 2021")+
  theme_classic()+
  scale_fill_manual(values = c("blue", "purple", "green", "orange", "yellow", "red"))+
  theme(legend.title = element_blank())+
  scale_x_discrete(labels=c("CH" = "Chaplin", "GVZ" = "GVZoo", "MS" = "Maria Slough",
                            "MV" = "Morris Valley", "TZ" = "TZoo", "VA" = "VanAqua"))+
  theme(text = element_text(family = "Arial"))+
  theme(text = element_text(size = 11))


####################### add SMI column to each subset data frame using respective bsma 
### VA.bsma calculated further down from VA_age dataset. R-squared on the regression is 0.908
prepo.gvz$SMI <- ScaledMassIndex.gvz(prepo.gvz$svl, prepo.gvz$mass)
prepo.va$SMI <- ScaledMassIndex.VA(prepo.va$svl, prepo.va$mass)
view(prepo.gvz) #n=126
view(prepo.va) #n=135
###### combine these two datasets, now with their proper respective SMI's
combine_prepo <- rbind(prepo.gvz, prepo.va)

#add in wild SMI - need to make wild dataset that includes season
wild.move <- wild_SMI[, c(1:6, 9)]
wild.move$season <- "postbrum" 
wild.move <- wild.move %>% 
  relocate(season, .after = EB) %>% 
  relocate(birth_yr, .after = pop)

###want to add TZ data for spring 2021 as well. Grab TZ data from SMI_zoos.TZ
pp_TZ <- SMI_zoos.TZ[, c(1:2, 4, 7:9, 12)]
#add column to match other datasets 
pp_TZ$season <- "post21"
pp_TZ <- pp_TZ %>% 
  rename(svl = SVL) %>%
  relocate(season, .after = EB)
view(pp_TZ)

#combine dataframes - all have 8 columns
full_pp <- rbind(combine_prepo, wild.move, pp_TZ)
view(full_pp)
#add some other columns to make more cohesive for grouping
full_pp <- full_pp %>% 
  mutate(when = case_when(grepl("pre", season) ~ "fall", grepl("post", season) ~ "spring")) %>% 
  relocate(when, .after = season)
#column for wild vs all zoos
full_pp <- full_pp %>% 
  mutate(population = case_when(pop == "GVZ" ~ "GVZ", pop == "TZ" ~ "TZ", pop == "VA" ~ "VA", pop == "CH" ~ "wild",
                                pop == "ST" ~ "wild", (grepl("M", pop) ~ "wild"))) %>% 
  relocate(population, .after = pop)
view(full_pp)
full_pp$population <- as.factor(full_pp$population)
full_pp$season <- factor(full_pp$season, labels = c("pre20", "post21", "postbrum", "pre21"), ordered = TRUE)
str(full_pp)
dim(full_pp) #772 rows, 10 columns

###### Plot all together now! ##############################################
###########################################################################
ggplot(data = full_pp, aes(x = population, y = SMI, fill=population))+
  geom_boxplot(show.legend = FALSE)+
  facet_grid(~season)+
  labs(x = "Population", y = "Scaled Mass Index (g)", title = "seasonal SMI from 2020-2021")+
  theme_classic()+
  theme(legend.title = element_blank())+
  theme(text = element_text(family = "Arial"))+
  theme(text = element_text(size = 12))
#####TZ should be in the post-21 facet - think GVZoo should be there too... are the facets incorrectly shifted?

ggplot(data = full_pp, aes(x = population, y = SMI, fill=population))+
  geom_boxplot(show.legend = FALSE)+
  facet_grid(~when)+
  labs(x = "Population", y = "Scaled Mass Index (g)", title = "Seasonal SMI from 2020-2021")+
  theme_classic()+
  theme(panel.background = element_rect(fill = "#F6F0ED"), 
        plot.background = element_rect(fill = "#F6F0ED"), 
        legend.background = element_rect(fill = "#F6F0ED"),
        legend.box.background = element_rect(fill = "#F6F0ED"))+
  scale_fill_manual(values = c("#FF9933", "#663300", "#CC0000", "#CC9999"))+
  theme(legend.title = element_blank())+
  theme(text = element_text(family = "Arial"))+
  theme(text = element_text(size = 12))

####facet by status
ggplot(data = full_pp, aes(x = population, y = SMI, fill=population))+
  geom_boxplot(show.legend = FALSE)+
  facet_grid(when ~ EB)+
  labs(x = "Population", y = "Scaled Mass Index (g)", title = "SMI by final status", 
       subtitle = "0 = egg bound, 1 = other")+
  theme_classic()+
  
  theme(legend.title = element_blank())+
  theme(text = element_text(family = "Arial"))+
  theme(text = element_text(size = 12))

lighten_swatch(0.2)
ggplot(data = full_pp, aes(x = population, y = SMI, fill=EB))+
  geom_boxplot(show.legend = TRUE)+
  facet_grid(~when)+
  labs(x = "Population", y = "Scaled Mass Index (g)", title = "SMI over seasons")+
  theme_classic()+
  theme(panel.background = element_rect(fill = "#F6F0ED"), 
        plot.background = element_rect(fill = "#F6F0ED"), 
        legend.background = element_rect(fill = "#F6F0ED"),
        legend.box.background = element_rect(fill = "#F6F0ED"))+
  theme(legend.title = element_blank())+
  scale_fill_discrete(name = "Status", labels = c("0" = "egg bound", "1" = "other"))+
  scale_fill_manual(values = c("#DC7466", "#9C8C78"))+
  theme(text = element_text(family = "Arial"))+
  theme(text = element_text(size = 12))


ggboxplot(full_pp, x = "EB", y = "SMI", facet.by = "population")+
  labs(x = "Status", y = "Scaled Mass Index (g)", title = "SMI in egg bound vs OK")+
  scale_x_discrete(labels=c("0" = "EB", "1" = "OK"))+
  theme(text = element_text(family = "Arial"))
### use combine_prepo to compare EB vs OK in GVZ vs VA over fall '20 and spring '21
str(combine_prepo) #261 obs from just GVZ and VA
combine_prepo$season <- as.factor(combine_prepo$season)
combine_prepo <- combine_prepo %>% 
  mutate(when = case_when(grepl("pre", season) ~ "fall", grepl("post", season) ~ "spring")) %>% 
  relocate(when, .after = season)
view(combine_prepo)
## Plot again with EB vs OK
lighten_swatch(0.1)
ggplot(data = combine_prepo, aes(x = EB, y = SMI, fill=EB))+
  geom_boxplot(alpha = 0.8, show.legend = FALSE)+
  facet_grid(when~pop)+
  labs(x = "Status", y = "Scaled Mass Index (g)")+
  theme_classic()+
  theme(panel.background = element_rect(fill = "#F6F0ED"), 
        plot.background = element_rect(fill = "#F6F0ED"), 
        legend.background = element_rect(fill = "#F6F0ED"),
        legend.box.background = element_rect(fill = "#F6F0ED"))+
  theme(legend.title = element_blank())+
  scale_x_discrete(labels = c("0" = "egg bound", "1" = "other"))+
  theme(text = element_text(family = "Arial"))+
  theme(text = element_text(size = 12))

#####mean SMI per population
mean(wild_SMI$SMI, na.rm = TRUE) #43.5887
#subset wild populations
wildCH_SMI <- subset(wild_SMI, pop == "CH")
str(wildCH_SMI) #n=7
wildMS_SMI <- subset(wild_SMI, pop == "MS")
wildMT_SMI <- subset(wild_SMI, pop == "MT")
wildMV_SMI <- subset(wild_SMI, pop == "MV")
wildST_SMI <- subset(wild_SMI, pop == "ST")
#means for wild pops 
mean(wildCH_SMI$SMI, na.rm = TRUE) #40.19464
mean(wildMS_SMI$SMI, na.rm = TRUE) #42.23196
mean(wildMT_SMI$SMI, na.rm = TRUE) #51.71032
mean(wildMV_SMI$SMI, na.rm = TRUE) #44.23784
mean(wildST_SMI$SMI, na.rm = TRUE) #46.19538
#subset zoo pops
prepo.GVZ <- subset(prepo, pop == "GVZ")
prepo.VA <- subset(prepo, pop == "VA")
#means for zoo pops per season
mean(prepo.GVZ$SMIpre20, na.rm = TRUE) #53.72223
mean(prepo.VA$SMIpre20, na.rm = TRUE) #53.45626
mean(prepo.GVZ$SMIpo21, na.rm = TRUE) #41.82268
mean(prepo.VA$SMIpo21, na.rm = TRUE) #84.62755
mean(prepo.GVZ$SMIpre21, na.rm = TRUE) #53.54254
mean(prepo.VA$SMIpre21, na.rm = TRUE) #57.34223

#according to Stevens 2013 (Intermediate Statistics: A Modern Approach), normality is not required (for ANOVA) for cases of large data sets (n>30)
#Kruskal-Wallis is an alternative option, compares medians of pops instead of means- does not require normality or homoscedasticity

#do a Levene or Bartlett test first before ANOVA to check for homoscedasticity (equality of variance) leveneTest()

####assume normality for mass and length data because most samples sizes are >30 and half the data are normal, half are not
#check homogeneity of variances (as assumption of ANOVA)

#visual check of variance via boxplot
boxplot(mass_pre20 ~ pop, prepo)
boxplot(svl_pre20 ~ pop, prepo)
boxplot(mass_post21 ~ pop, prepo)
boxplot(svl_post21 ~ pop, prepo)
boxplot(avg_mass ~ pop, prepo)
boxplot(svl_pre21 ~ pop, prepo)

#####above boxplots etc may not be necessary. Really just comparin bsma of GVZ and VA first to see if significantly different


#########################################################################################
# Calculate SMI for full 2021 snapshot of zoos and wild##################################
# use SMI equations from above with bsma for each individual pop (need one for TZ still)#
#########################################################################################
# Regressions first

str(SMI_zoos)
#subset SMI_zoos data frame by populations: CH, GVZ, MS, MV, TZ, VA
SMI_zoos.CH <- subset(SMI_zoos, pop == "CH")
str(SMI_zoos.CH) #n=7
SMI_zoos.GVZ <- subset(SMI_zoos, pop == "GVZ")
str(SMI_zoos.GVZ) #n=41
SMI_zoos.MS <- subset(SMI_zoos, pop == "MS")
str(SMI_zoos.MS) #n=25
SMI_zoos.MV <- subset(SMI_zoos, pop == "MV")
str(SMI_zoos.MV) #n=13
SMI_zoos.TZ <- subset(SMI_zoos, pop == "TZ")
str(SMI_zoos.TZ) #n=9
SMI_zoos.VA <- subset(SMI_zoos, pop == "VA")
str(SMI_zoos.VA) #n=15
#subset by locale (zoos vs wild)
SMI_zoos.z <- subset(SMI_zoos, locale == "Zoo")
str(SMI_zoos.z) #n=65
SMI_zoos.w <- subset(SMI_zoos, locale == "Wild")
str(SMI_zoos.w) #n=45


### Mass (dependant) vs length (independent)
############################ 
plot(SMI_zoos$SVL, SMI_zoos$mass, pch = 16, cex = 1.3,
     col = (c('blue', 'purple', 'green', 'orange', 'yellow', 'red')[as.numeric(SMI_zoos$pop)]),
     xlab = "snout-vent length (mm)", ylab = "mass (g)")
legend("bottomright", title = "Population", c("Chaplin", "GVZoo", "Maria Slough", "Morris Valley", "TZoo", "VanAqua"), 
       fill = c('blue', 'purple', 'green', 'orange', 'yellow', 'red'), cex = 0.6)
abline(lm(mass ~ SVL, data = SMI_zoos), col = "black")
lm(mass ~ SVL, data = SMI_zoos)
summary(lm(mass ~ SVL, data = SMI_zoos))
abline(lm(mass ~ SVL, data = SMI_zoos.CH), col = "blue")
summary(lm(mass ~ SVL, data = SMI_zoos.CH))
abline(lm(mass ~ SVL, data = SMI_zoos.GVZ), col = "purple")
summary(lm(mass ~ SVL, data = SMI_zoos.GVZ))
abline(lm(mass ~ SVL, data = SMI_zoos.MS), col = "green")
summary(lm(mass ~ SVL, data = SMI_zoos.MS))
abline(lm(mass ~ SVL, data = SMI_zoos.MV), col = "orange")
summary(lm(mass ~ SVL, data = SMI_zoos.MV))
abline(lm(mass ~ SVL, data = SMI_zoos.TZ), col = "yellow")
summary(lm(mass ~ SVL, data = SMI_zoos.TZ))
abline(lm(mass ~ SVL, data = SMI_zoos.VA), col = "red")
summary(lm(mass ~ SVL, data = SMI_zoos.VA))
legend("topleft", title = "Regression lines", c("n=7, Adjusted R-squared: 0.5825, p-value: 0.02806",
                                                "n=41, Adjusted R-squared: 0.252, p-value: 0.000489",
                                                "n=25, Adjusted R-squared: 0.6651, p-value: 4.14e-07",
                                                "n=13, Adjusted R-squared: 0.6776, p-value: 0.0003331",
                                                "n=9, Adjusted R-squared: 0.4312, p-value: 0.03256",
                                                "n=15, Adjusted R-squared: 0.7145, p-value: 4.422e-05"),
       fill = c('blue', 'purple', 'green', 'orange', 'yellow', 'red'), cex = 0.6)
title("2021 spring Mass by SVL")

#Log-transformation
SMI_zoos$log.SVL = log(SMI_zoos$SVL)
SMI_zoos$log.mass = log(SMI_zoos$mass)
SMI_zoos.CH$log.SVL = log(SMI_zoos.CH$SVL)
SMI_zoos.CH$log.mass = log(SMI_zoos.CH$mass)
SMI_zoos.GVZ$log.SVL = log(SMI_zoos.GVZ$SVL)
SMI_zoos.GVZ$log.mass = log(SMI_zoos.GVZ$mass)
SMI_zoos.MS$log.SVL = log(SMI_zoos.MS$SVL)
SMI_zoos.MS$log.mass = log(SMI_zoos.MS$mass)
SMI_zoos.MV$log.SVL = log(SMI_zoos.MV$SVL)
SMI_zoos.MV$log.mass = log(SMI_zoos.MV$mass)
SMI_zoos.TZ$log.SVL = log(SMI_zoos.TZ$SVL)
SMI_zoos.TZ$log.mass = log(SMI_zoos.TZ$mass)
SMI_zoos.VA$log.SVL = log(SMI_zoos.VA$SVL)
SMI_zoos.VA$log.mass = log(SMI_zoos.VA$mass)

# Log Mass vs Log Length
############################ 
plot(SMI_zoos$log.SVL, SMI_zoos$log.mass, pch = 16, cex = 1.3,
     col = (c('blue', 'purple', 'green', 'orange', 'yellow', 'red')[as.numeric(SMI_zoos$pop)]),
     xlab = "Log transformed snout-vent length", ylab = "Log transformed mass (g)")
legend("bottomright", title = "Population", c("Chaplin", "GVZoo", "Maria Slough", "Morris Valley", "TZoo", "VanAqua"), 
       fill = c('blue', 'purple', 'green', 'orange', 'yellow', 'red'), cex = 0.6)
abline(lm(log.mass ~ log.SVL, data = SMI_zoos), col = "black")
lm(log.mass ~ log.SVL, data = SMI_zoos)
summary(lm(log.mass ~ log.SVL, data = SMI_zoos))
abline(lm(log.mass ~ log.SVL, data = SMI_zoos.CH), col = "blue")
summary(lm(log.mass ~ log.SVL, data = SMI_zoos.CH))
abline(lm(log.mass ~ log.SVL, data = SMI_zoos.GVZ), col = "purple")
summary(lm(log.mass ~ log.SVL, data = SMI_zoos.GVZ))
abline(lm(log.mass ~ log.SVL, data = SMI_zoos.MS), col = "green")
summary(lm(log.mass ~ log.SVL, data = SMI_zoos.MS))
abline(lm(log.mass ~ log.SVL, data = SMI_zoos.MV), col = "orange")
summary(lm(log.mass ~ log.SVL, data = SMI_zoos.MV))
abline(lm(log.mass ~ log.SVL, data = SMI_zoos.TZ), col = "yellow")
summary(lm(log.mass ~ log.SVL, data = SMI_zoos.TZ))
abline(lm(log.mass ~ log.SVL, data = SMI_zoos.VA), col = "red")
summary(lm(log.mass ~ log.SVL, data = SMI_zoos.VA))
legend("topleft", title = "Regression lines", c("n=7, Adjusted R-squared: 0.5935, p-value: 0.02613",
                                                "n=41, Adjusted R-squared: 0.2388, p-value: 0.0007024",
                                                "n=25, Adjusted R-squared: 0.6846, p-value: 2.048e-07",
                                                "n=13, Adjusted R-squared: 0.6446, p-value: 0.0005799",
                                                "n=9, Adjusted R-squared: 0.4231, p-value: 0.03438",
                                                "n=15, Adjusted R-squared: 0.7345, p-value: 2.73e-05"),
       fill = c('blue', 'purple', 'green', 'orange', 'yellow', 'red'), cex = 0.5)
title("Log of MbyL spring 2021")

#################the un-transformed regression is the best R-squared for TZoo (tho still not very good) 
## use it for calculating bsma
sma(SMI_zoos.TZ$log.mass ~ SMI_zoos.TZ$log.SVL)
bsma.tz <- 2.379120

ScaledMassIndex.tz <-
  function(x, y) {
    y * ((Lo / x) ^ bsma.tz)
  }


#############################################################################################
### Calculate SMI for 2021 data using Lo and bsma(s) defined previously - unique bsma per pop
#############################################################################################
## use subset data and combine back together at end 
####################################################################

SMI_zoos.w$SMI <- ScaledMassIndex.w(SMI_zoos.w$SVL, SMI_zoos.w$mass)
view(SMI_zoos.w)
SMI_zoos.GVZ$SMI <- ScaledMassIndex.gvz(SMI_zoos.GVZ$SVL, SMI_zoos.GVZ$mass)
view(SMI_zoos.GVZ)
SMI_zoos.TZ$SMI <- ScaledMassIndex.tz(SMI_zoos.TZ$SVL, SMI_zoos.TZ$mass)
view(SMI_zoos.TZ)
SMI_zoos.VA$SMI <- ScaledMassIndex.VA(SMI_zoos.VA$SVL, SMI_zoos.VA$mass)
view(SMI_zoos.VA)
###### rbind all these subsets with their unique SMI values
full_21 <- rbind(SMI_zoos.w, SMI_zoos.GVZ, SMI_zoos.TZ, SMI_zoos.VA)
view(full_21)


######################################################################################################
# plot SMI ####################################################################
ggplot(data = full_21, aes(x = pop, y = SMI, fill=factor(pop, labels = c("Chaplin", "GVZoo", "Maria Slough",
                                                                          "Morris Valley", "TZoo", "VanAqua"))))+
  geom_boxplot(show.legend = FALSE)+
  labs(x = "Population", y = "Scaled Mass Index (g)", title = "SMI in spring 2021")+
  theme_classic()+
  scale_fill_manual(values = c("blue", "purple", "green", "orange", "yellow", "red"))+
  theme(legend.title = element_blank())+
  scale_x_discrete(labels=c("CH" = "Chaplin", "GVZ" = "GVZoo", "MS" = "Maria Slough",
                            "MV" = "Morris Valley", "TZ" = "TZoo", "VA" = "VanAqua"))+
  theme(text = element_text(family = "Arial"))+
  theme(text = element_text(size = 11))

#wild vs zoos 
#create new column to group three wild populations together but separate from zoos
full_21 <- full_21 %>% 
  mutate(population = case_when(pop == "CH" ~ "wild",
                                pop == "MS" ~ "wild",
                                pop == "MV" ~ "wild",
                                pop == "GVZ" ~ "GVZ",
                                pop == "VA" ~ "VA",
                                pop == "TZ" ~ "TZ")) %>% 
  relocate(population, .after = 3)
full_21$population <- as.factor(full_21$population) #4 levels, GVZ TZ VA wild
view(full_21)

ggplot(data = full_21, aes(x = population, y = SMI, fill=factor(population, labels = c("GVZoo", 
                                                                                 "TZoo", "VanAqua", "Wild"))))+
  geom_boxplot(show.legend = FALSE)+
  labs(x = "Population", y = "Scaled Mass Index (g)", title = "SMI in spring 2021")+
  theme_classic()+
  scale_fill_manual(values = c("purple", "yellow", "red", "blue"))+
  theme(legend.title = element_blank())+
  scale_x_discrete(labels=c("wild" = "Wild", "GVZ" = "GVZoo", "TZ" = "TZoo", "VA" = "VanAqua"))+
  theme(text = element_text(family = "Arial"))+
  theme(text = element_text(size = 13))

ggboxplot(full_21, x = "EB", y = "SMI", facet.by = "population")+
  labs(x = "Status", y = "Scaled Mass Index (g)", title = "2021 SMI in egg bound vs OK OSF")+
  scale_x_discrete(labels=c("0" = "egg bound", "1" = "other females"))+
  theme(text = element_text(family = "Arial"))

###########################################################
## VA SMI data organized by age
###########################################################
VA_byage_wide <- read.csv("VA_SMI_byage.csv", header = TRUE)
#transform from wide to long 
VA_age <- melt(setDT(VA_byage_wide), id=1:8, measure=patterns("^mass", "^sul"),
     value.name=c("mass", "sul"), variable.name="ageof", na.rm = TRUE)

VA_age <- VA_age %>% 
  mutate(age = case_when(ageof == "1" ~ "0", ageof == "2" ~ "1", ageof == "3" ~ "2", ageof == "4" ~ "3",
                        ageof == "5" ~ "4", ageof == "6" ~ "5", ageof == "7" ~ "6", ageof == "8" ~ "7",
                        ageof == "9" ~ "8", ageof == "10" ~ "9", ageof == "11" ~ "10")) %>%
           relocate(age, .after = EB) #need to make way cleaner way of doing this. set eqn instead of each value
VA_age$ageof <- NULL #drop ageof column now that we have proper age
view(VA_age)
VA_age$frog_id <- as.factor(VA_age$frog_id) #factor 144 levels (individual frogs)
VA_age$EB <- as.factor(VA_age$EB) #as factor. Two levels: 0=EB 1=not (OK)
VA_age$pop <- as.factor(VA_age$pop) #factor with 1 level (all from VA)
VA_age$source <- as.factor(VA_age$source) #factor with 13 levels
VA_age$birth_yr <- as.factor(VA_age$birth_yr) #13 levels
VA_age$age <- as.factor(VA_age$age) #11 levels
str(VA_age)
dim(VA_age) #444 rows, 11 columns
summary(VA_age)

#convert sul values into SVL using previous regression equation
#SVL = 0.889x + 5.1677, R2 = 0.4573
SULconvert <- #set function for SUL to SVL conversion
  function(x) {
    0.889 * x + 5.1677
  }

VA_age$svl <- SULconvert(VA_age$sul) #new column of converted svl values

#visualize distribution
hist(VA_age$mass, main = "VA mass")
hist(VA_age$svl, main = "VA svl")
ggdensity(VA_age$mass, main = "VA mass", xlab = "mass (g)")
ggdensity(VA_age$svl, main = "VA svl", xlab = "snout-vent length (mm)")
ggqqplot(VA_age$mass, na.rm = TRUE)
ggqqplot(VA_age$svl, na.rm = TRUE)
#calculate normality by shapiro (H0 = normal)
shapiro.test(VA_age$mass) #significant = not normal
shapiro.test(VA_age$svl) #significant = not normal
######neither are normal but have large sample size (n=444 total observations)
######samples are not independent because we have multiple measurements per individual frog

#bootstrap confidence intervals
VA.m_boot <- boot(VA_age$mass, my_mean, 1000) #bootstrap the mean for age
VA.m_boot 
plot(VA.m_boot)
boot.ci(boot_out <- VA.m_boot, #confidence intervals for mass
        type = c("norm", "basic", "perc", "bca"))
VA.l_boot <- boot(VA_age$svl, my_mean, 1000) #bootstrap the mean for age
VA.l_boot 
plot(VA.l_boot)
boot.ci(boot_out <- VA.l_boot, #confidence intervals for mass
        type = c("norm", "basic", "perc", "bca"))

#####subset data for adults (>2 years old)
#SMI should standardize for age but want to look at normality of data as is
VA_adults <- VA_age %>% 
  filter(age != 0 & age != 1) #exclude frogs at 0 and 1 yrs
view(VA_adults)
str(VA_adults) #n=263, still have all 144 individual frogs though

VA_adults2 <- VA_adults %>% #exclude 0-2 years old. technically "maturity" is reached after 2yrs
  filter(age != 2)
view(VA_adults2)
str(VA_adults2) #n=168 obs, still 144 frogs

#view their distribution
hist(VA_adults$mass)
hist(VA_adults$svl)
hist(VA_adults2$mass)
hist(VA_adults2$svl) #these all look considerably more normal

### Mass (dependant) vs length (independent)
############################ VA full dataset
#subset by status (EB or OK)
VA_EB <- subset(VA_age, EB == "0")
str(VA_EB) #n=158
VA_OK <- subset(VA_age, EB == "1")
str(VA_OK) #n=276

plot(VA_age$svl, VA_age$mass, pch = 16, cex = 1.3,
     col = (c('red', 'blue')[as.numeric(VA_age$EB)]),
     xlab = "snout-vent length (mm)", ylab = "mass (g)")
legend("bottomright", title = "status", c("Egg bound", "Other"), fill = c('red', 'blue'), cex = 0.8)
abline(lm(mass ~ svl, data = VA_age), col = "black")
lm(mass ~ svl, data = VA_age)
summary(lm(mass ~ svl, data = VA_age))
abline(lm(mass ~ svl, data = VA_EB), col = "red")
summary(lm(mass ~ svl, data = VA_EB))
abline(lm(mass ~ svl, data = VA_OK), col = "blue")
summary(lm(mass ~ svl, data = VA_OK))
legend("topleft", title = "Regression lines", c("n=434, Adjusted R-squared: 0.7607, p-value: 2.2e-16", 
                                                "n=158, Adjusted R-squared: 0.7774, p-value: 2.2e-16",
                                                "n=276, Adjusted R-squared: 0.7577, p-value: 2.2e-16"),
       fill = c('black', 'red', 'blue'), cex = 0.65)
title("MbyL for full VA dataset")

#Log-transformation
VA_age$log.svl = log(VA_age$svl)
VA_age$log.mass = log(VA_age$mass)
VA_EB$log.svl = log(VA_EB$svl)
VA_OK$log.svl = log(VA_OK$svl)
VA_EB$log.mass = log(VA_EB$mass)
VA_OK$log.mass = log(VA_OK$mass)

### Log Mass vs Log Length for 
############################ Pre-brumation (i.e. Nov) 2020
plot(VA_age$log.svl, VA_age$log.mass, pch = 16, cex = 1.3,
     col = (c('red', 'blue')[as.numeric(VA_age$EB)]),
     xlab = "Log transformed snout-vent length", ylab = "Log transformed mass")
legend("topleft", title = "Status", c("Egg bound", "Other"), fill = c('red', 'blue'), cex = 0.8)
abline(lm(log.mass ~ log.svl, data = VA_age), col = "black")
lm(log.mass ~ log.svl, data = VA_age)
summary(lm(log.mass ~ log.svl, data = VA_age))
abline(lm(log.mass ~ log.svl, data = VA_EB), col = "red")
summary(lm(log.mass ~ log.svl, data = VA_EB))
abline(lm(log.mass ~ log.svl, data = VA_OK), col = "blue")
summary(lm(log.mass ~ log.svl, data = VA_OK))
legend("bottomright", title = "Regression lines", c("n=434, Adjusted R-squared: 0.908, p-value: 2.2e-16",
                                                    "n=158, Adjusted R-squared: 0.9285, p-value: 2.2e-16",
                                                    "n=276, Adjusted R-squared: 0.898, p-value: 2.2e-16"),
       fill = c('black', 'red', 'blue'), cex = 0.7)
title("Log of MbyL for VA dataset")

##############################adults vs young
#already have VA_adults2 for >2 year old frogs
str(VA_adults2) #n=168
VA_0 <- subset(VA_age, age == "0")
str(VA_0) #n=75
VA_1 <- subset(VA_age, age == "1")
str(VA_1) #n=106
VA_2 <- subset(VA_age, age == "2")
str(VA_2) #n=95

VA_age <- VA_age %>% 
  mutate(maturity = case_when(age > 2 ~ "adult",
                              age == 0  ~ "0",
                              age == 1 ~ "1",
                              age == 2 ~ "2")) %>% 
  relocate(maturity, .after = age)
VA_age$maturity <- as.factor(VA_age$maturity)
str(VA_age) #4 levels: 0, 1, 2, adult (>2 yrs old)

plot(VA_age$svl, VA_age$mass, pch = 16, cex = 1.3,
     col = (c('orange', 'green', 'purple', 'blue')[as.numeric(VA_age$maturity)]),
     xlab = "snout-vent length (mm)", ylab = "mass (g)")
legend("bottomright", title = "Age", c("0yo", "1yo", "2yo", "adult (>2yo)"), 
       fill = c('orange', 'green', 'purple', 'blue'), cex = 0.7)
abline(lm(mass ~ svl, data = VA_age), col = "black")
lm(mass ~ svl, data = VA_age)
summary(lm(mass ~ svl, data = VA_age))
abline(lm(mass ~ svl, data = VA_0), col = "orange")
summary(lm(mass ~ svl, data = VA_0))
abline(lm(mass ~ svl, data = VA_1), col = "green")
summary(lm(mass ~ svl, data = VA_1))
abline(lm(mass ~ svl, data = VA_2), col = "purple")
summary(lm(mass ~ svl, data = VA_2))
abline(lm(mass ~ svl, data = VA_adults2), col = "blue")
summary(lm(mass ~ svl, data = VA_adults2))
legend("topleft", title = "Regression lines", c("n=444, Adjusted R-squared: 0.7607, p-value: 2.2e-16", 
                                                "n=168, Adjusted R-squared: 0.7582, p-value: 2.2e-16",
                                                "n=75, Adjusted R-squared: 0.7657, p-value: 2.2e-16",
                                                "n=106, Adjusted R-squared: 0.7274, p-value: 2.2e-16",
                                                "n=95, Adjusted R-squared: 0.3358, p-value: 2.2e-16"),
       fill = c('black', 'orange', 'green', 'purple', 'blue'), cex = 0.6)
title("MbyL by age at VA")

#######Log transformation
VA_0$log.svl = log(VA_0$svl)
VA_0$log.mass = log(VA_0$mass)
VA_1$log.svl = log(VA_1$svl)
VA_1$log.mass = log(VA_1$mass)
VA_2$log.svl = log(VA_2$svl)
VA_2$log.mass = log(VA_2$mass)
VA_adults2$log.svl = log(VA_adults2$svl)
VA_adults2$log.mass = log(VA_adults2$mass)
#######Plot Log M by L
plot(VA_age$log.svl, VA_age$log.mass, pch = 16, cex = 1.3,
     col = (c('orange', 'green', 'purple', 'blue')[as.numeric(VA_age$maturity)]),
     xlab = "Log transformed snout-vent length", ylab = "log transformed mass")
legend("topleft", title = "Age", c("0yo", "1yo", "2yo", "adult (>2yo)"), 
       fill = c('orange', 'green', 'purple', 'blue'), cex = 0.7)
abline(lm(log.mass ~ log.svl, data = VA_age), col = "black")
lm(log.mass ~ log.svl, data = VA_age)
summary(lm(log.mass ~ log.svl, data = VA_age))
abline(lm(log.mass ~ log.svl, data = VA_0), col = "orange")
summary(lm(log.mass ~ log.svl, data = VA_0))
abline(lm(log.mass ~ log.svl, data = VA_1), col = "green")
summary(lm(log.mass ~ log.svl, data = VA_1))
abline(lm(log.mass ~ log.svl, data = VA_2), col = "purple")
summary(lm(log.mass ~ log.svl, data = VA_2))
abline(lm(log.mass ~ log.svl, data = VA_adults2), col = "blue")
summary(lm(log.mass ~ log.svl, data = VA_adults2))
legend("bottomright", title = "Regression lines", c("n=444, Adjusted R-squared: 0.908, p-value: 2.2e-16", 
                                                "n=168, Adjusted R-squared: 0.7822, p-value: 2.2e-16",
                                                "n=75, Adjusted R-squared: 0.8466, p-value: 2.2e-16",
                                                "n=106, Adjusted R-squared: 0.8133, p-value: 2.2e-16",
                                                "n=95, Adjusted R-squared: 0.4034, p-value: 2.2e-16"),
       fill = c('black', 'orange', 'green', 'purple', 'blue'), cex = 0.6)
title("Log of MbyL by age at VA")

###########################################
#####Calculate SMI for VA dataset #########
###########################################

VA_age$SMI.w <- ScaledMassIndex.w(VA_age$svl, VA_age$mass) #this is using wild bsma
head(VA_age)

#calculate SMI for VA adult subset
VA_adults$SMI <- ScaledMassIndex.VA(VA_adults$svl, VA_adults$mass)
VA_adults2$SMI <- ScaledMassIndex.VA(VA_adults2$svl, VA_adults2$mass)

#plot SMI
######################################################
#GRAPHING#############################################
plot(VA_age$maturity, VA_age$SMI.w, xlab = "Age", ylab = "Scaled Mass Index (g)", 
     main = "Scaled Mass Index for VanAqua OSF",
     col = (c("orange", "green", "purple", "blue")))

plot(as.factor(VA_age$age), VA_age$SMI.w, xlab = "Age", ylab = "Scaled Mass Index (g)", 
     main = "Scaled Mass Index for VanAqua OSF",
     col = (c("orange")))

#nicer plots
ggplot(data = VA_age, aes(x = maturity, y = SMI.w, fill=factor(maturity, labels = c("0yo", "1yo", "2yo",
                                                                          "adult (>2yo)"))))+
  geom_boxplot(show.legend = FALSE)+
  labs(x = "Age (years)", y = "Scaled Mass Index (g)", title = "SMI at VanAqua (2012-2020)")+
  theme_classic()+
  scale_fill_manual(values = c("orange", "green", "purple", "blue"))+
  theme(legend.title = element_blank())+
  scale_x_discrete(labels=c("0" = "0yo", "1" = "1yo", "2" = "2yo", "adult" = "adult (>2yo)"))+
  theme(text = element_text(family = "Arial"))+
  theme(text = element_text(size = 12))

ggplot(data = VA_age, aes(x = as.factor(age), y = SMI.w, fill=factor(age)))+
  geom_boxplot(show.legend = FALSE)+
  labs(x = "Age (years)", y = "Scaled Mass Index (g)", title = "SMI at VanAqua (2012-2020)")+
  theme_classic()+
  theme(legend.title = element_blank())+
  theme(text = element_text(family = "Arial"))+
  theme(text = element_text(size = 12))

ggboxplot(VA_age %>% filter(!is.na(maturity)), x = "EB", y = "SMI.w", facet.by = "maturity")+
  labs(x = "Status", y = "Scaled Mass Index (g)", title = "SMI by status at VanAqua")+
  scale_x_discrete(labels=c("0" = "egg bound", "1" = "OK"))

#graph EB vs OK over time
VA_age.clean <- VA_age %>% #filter out the NA values for status
  filter(!is.na(EB))

# status.names <- c('0' = "Egg bound",
#                   '1' = "other")
#can't get the above line to work with below boxplot to change facet names - come back to
ggboxplot(VA_age.clean, x = "age", y = "SMI.w", facet.by = "EB")+
  labs(x = "Age (yrs)", y = "Scaled Mass Index (g)", title = "SMI over age at VanAqua",
       subtitle = "0 = egg bound, 1 = other")

####same facets by status but using VA own bsma (bsma.VA)
ggboxplot(VA_age.clean, x = "age", y = "SMI", facet.by = "EB")+
  labs(x = "Age (yrs)", y = "Scaled Mass Index (g)", title = "SMI over age at VanAqua",
       subtitle = "0 = egg bound, 1 = other") #####here now the "1" (i.e. ok) looks standardized but see diff in EB

ggboxplot(VA_age.clean %>% filter(!is.na(maturity)), x = "EB", y = "SMI", facet.by = "maturity")+
  labs(x = "Status", y = "Scaled Mass Index (g)", title = "SMI by status at VanAqua")+
  scale_x_discrete(labels=c("0" = "egg bound", "1" = "OK"))

############# Shouldn't really be seeing this big of a difference in SMI across ages
############# Supposed to be 'scaled' by the bsma 
#########################
# Try calculating bsma and Lo specific to this VanAqua population - instead of from wild pops
####################################################################################################

#calculate bsma again but this time automated using smatr() - Standardized Major Axis
sma(VA_age$log.mass ~ VA_age$log.svl) #use log #the slope of the estimate is the bsma 
VA.bsma <- 3.949900
#check if different if just using VA adults
sma(VA_adults2$log.mass ~ VA_adults2$log.svl)
VA.a.bsma <- 3.202558
#wild bsma was 3.0312
#bsma from VA adults is closer to this and closer to slope of 3 in general (3 supposed to be best?)
########################
#calculate Lo
Lo.VA <- mean(VA_age$svl, na.rm = TRUE)
Lo.VA #55.09706
Lo.VA.a <- mean(VA_adults2$svl, na.rm = TRUE)
Lo.VA.a #64.98735
#wild Lo was 71.3709 - but the Lo should be arbitrary

########Create new SMI functions for new bsma using old Lo
ScaledMassIndex.VA <- function(x, y) {
  y * ((Lo / x) ^ VA.bsma)
}

ScaledMassIndex.va <- function(x, y) {
  y * ((Lo / x) ^ VA.a.bsma)
}
########################## Delete the above section if I find better position for it ? ################

#apply functions to VA data and create new SMI columns respectively
VA_age$SMI <- ScaledMassIndex.VA(VA_age$svl, VA_age$mass)
VA_age$SMI.va <- ScaledMassIndex.va(VA_age$svl, VA_age$mass)
view(VA_age) #can already see differences in SMI values calculated with each different bsma
#let's plot to see if the SMI are more standardized across ages now
#######first with bsma based on full VA dataset (includes juvenile frogs)
ggplot(data = VA_age, aes(x = as.factor(age), y = SMI, fill=factor(age)))+
  geom_boxplot(show.legend = FALSE)+
  labs(x = "Age (years)", y = "Scaled Mass Index (g)", title = "SMI at VanAqua using own bsma")+
  theme_classic()+
  theme(legend.title = element_blank())+
  theme(text = element_text(family = "Arial"))+
  theme(text = element_text(size = 12))
######now with bsma based only on adults
ggplot(data = VA_age, aes(x = as.factor(age), y = SMI.va, fill=factor(age)))+
  geom_boxplot(show.legend = FALSE)+
  labs(x = "Age (years)", y = "Scaled Mass Index (g)", title = "SMI at VanAqua using adult bsma")+
  theme_classic()+
  theme(legend.title = element_blank())+
  theme(text = element_text(family = "Arial"))+
  theme(text = element_text(size = 12))
########################################################
##### SMI's calculated with bsma from full VA dataset definitely seem the most standardized
###### might need different bsma for each population? Or does that defeat the mathmatical purpose?


###do a subset by year of death - just show smi for each frog at death
#or highlight that against all the others that year?

########################################################
## create new dataframe combining VA and wild full datasets
########################################################

adults <- VA_adults2[, c('frog_id', 'pop', 'EB', 'birth_yr', 'mass', 'svl', 'log.mass', 'log.svl', 'SMI')]
view(adults)
#add EB column to wild data frame first
wild_SMI$EB <- 1
wild_SMI <- wild_SMI %>% 
  relocate(EB, .after = pop) %>% 
  rename(mass = avg_mass,
         svl = avg_SVL,
         log.mass = log.avg_mass,
         log.svl = log.avg_SVL)
view(wild_SMI)

#combine dataframes
adult <- rbind(adults, wild_SMI)
view(adult)
str(adult) #670 rows, 9 columns - check: combined VA_adults2 (n=168) + wild_SMI (n=502) = GOOD
#recalculate SMI and try SMI using VA.bsma also
adult$SMI.w <- ScaledMassIndex.w(adult$svl, adult$mass)
adult$SMI.VA <- ScaledMassIndex.VA(adult$svl, adult$mass)
adult$SMI.va <- ScaledMassIndex.va(adult$svl, adult$mass)
view(adult)

########### Plot the SMI
ggplot(data = adult, aes(x = pop, y = SMI, fill=factor(pop, labels = c("VA", "CH", "MS",
                                                                                  "MT", "MV", "ST"))))+
  geom_boxplot(show.legend = FALSE)+
  labs(x = "Population", y = "Scaled Mass Index (g)", title = "SMI in adult VA vs wild")+
  theme_classic()+
  scale_fill_manual(values = c("green", "orange", "yellow", "purple", "blue", "brown"))+
  theme(legend.title = element_blank())+
  scale_x_discrete(labels=c("VA" = "VanAqua", "CH" = "Chaplin", "MS" = "Maria Slough", "MT" = "Mountain",
                            "MV" = "Morris Valley", "ST" = "Semmihault"))+
  theme(text = element_text(family = "Arial"))+
  theme(text = element_text(size = 12))

#create new column to group wild populations together
adult <- adult %>% 
  mutate(population = case_when(pop == "CH" ~ "wild",
                                pop == "MS" ~ "wild",
                                pop == "MV" ~ "wild",
                                pop == "MT" ~ "wild",
                                pop == "ST" ~ "wild",
                                pop == "VA" ~ "VA",)) %>% 
  relocate(population, .after = 2)
adult$population <- as.factor(adult$population)
str(adult)

#plot again but comparing full overall wild with VA
ggplot(data = adult, aes(x = population, y = SMI, fill=factor(population, labels = c("VA", "wild"))))+
  geom_boxplot(show.legend = FALSE)+
  labs(x = "Population", y = "Scaled Mass Index (g)", title = "SMI in adult VA vs overall wild")+
  theme_classic()+
  scale_fill_manual(values = c("green", "orange"))+
  theme(legend.title = element_blank())+
  scale_x_discrete(labels=c("VA" = "Vancovuer Aquarum", "wild" = "Combined Wild"))+
  theme(text = element_text(family = "Arial"))+
  theme(text = element_text(size = 12))
############ NOTE: VanAqua SMI is based on fall measurements while wild is from breeding season (spring)
###########This is also using own bsma for each pop - now try with alternative bsma
ggplot(data = adult, aes(x = population, y = SMI.w, fill=factor(population, labels = c("VA", "wild"))))+
  geom_boxplot(show.legend = FALSE)+
  labs(x = "Population", y = "Scaled Mass Index (g)", title = "SMI in adult VA vs overall wild", subtitle = "using wild bsma")+
  theme_classic()+
  scale_fill_manual(values = c("green", "orange"))+
  theme(legend.title = element_blank())+
  scale_x_discrete(labels=c("VA" = "Vancovuer Aquarum", "wild" = "Combined Wild"))+
  theme(text = element_text(family = "Arial"))+
  theme(text = element_text(size = 12))

ggplot(data = adult, aes(x = population, y = SMI.va, fill=factor(population, labels = c("VA", "wild"))))+
  geom_boxplot(show.legend = FALSE)+
  labs(x = "Population", y = "Scaled Mass Index (g)", title = "SMI in adult VA vs overall wild",
       subtitle = "using VA adult's bsma")+
  theme_classic()+
  scale_fill_manual(values = c("green", "orange"))+
  theme(legend.title = element_blank())+
  scale_x_discrete(labels=c("VA" = "Vancovuer Aquarum", "wild" = "Combined Wild"))+
  theme(text = element_text(family = "Arial"))+
  theme(text = element_text(size = 12))
##### facet with status
ggboxplot(adult, x = "EB", y = "SMI", facet.by = "population")+ #I like this graph better than the one below
  labs(x = "Status", y = "Scaled Mass Index (g)", title = "SMI by status")+
  scale_x_discrete(labels=c("0" = "egg bound", "1" = "OK"))

ggboxplot(adult, x = "population", y = "SMI", facet.by = "EB")+
  labs(x = "Population", y = "Scaled Mass Index (g)", title = "SMI by status")+
  scale_x_discrete(labels=c("VA" = "VanAqua", "wild" = "Wild"))


###############################################################
####### look at (fall) mass changes with age in TZ and VA
###############################################################
mass_age.wide <- read.csv("fall_mass_byage.csv", header = TRUE)
head(mass_age.wide)
view(mass_age.wide)
#need to transform into long form
mass_age <- melt(setDT(mass_age.wide), id=1:9, measure=patterns("^mass"),
              value.name=c("mass"), variable.name="ageof", na.rm = TRUE)
mass_age <- mass_age %>% 
  mutate(age_mass = case_when(ageof == 'mass0' ~ '0',
                              ageof == 'mass1' ~ '1',
                              ageof ==  'mass2' ~ '2',
                              ageof == 'mass3' ~ '3',
                              ageof ==  'mass4' ~ '4',
                              ageof ==  'mass5' ~ '5',
                              ageof ==  'mass6' ~ '6',
                              ageof == 'mass7' ~ '7',
                              ageof ==  'mass8' ~ '8',
                              ageof ==  'mass9' ~ '9',
                              ageof ==  'mass10' ~ '10',
                              ageof == 'mass11' ~ '11',
                              ageof == 'mass12' ~ '12')) %>% 
  relocate(age_mass, .after = EB)
mass_age$ageof <- NULL  #can now drop the ageof column since we have proper age       
view(mass_age)        
dim(mass_age) #566 rows, 11 columns
 
mass_age$ID <- as.factor(mass_age$ID) #convert a bunch of chr to factor
mass_age$pop <- as.factor(mass_age$pop)
mass_age$source <- as.factor(mass_age$source)
mass_age$birth_site <- as.factor(mass_age$birth_site) #12 levels
mass_age$birth_type <- as.factor(mass_age$birth_type) #2 levels 
mass_age$EB <- as.factor(mass_age$EB) #as factor. Two levels: 0=EB 1=not (OK)
mass_age$age_mass <- as.factor(mass_age$age_mass)
mass_age$age_mass <- factor(mass_adult$age_mass, 
                            levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
                            ordered = TRUE) #double check the level order
str(mass_age)
summary(mass_age)

#visualize normality
hist(mass_age$mass) #bimodal thing again from 0-1 year olds
hist(mass_age$age_yrs)
ggqqplot(mass_age$mass, na.rm = TRUE)
shapiro.test(mass_age$mass) #significant = not-normal
# subset out adults (>2 years old at mass)
mass_adult <- mass_age %>% 
  filter(age_mass != 0 & age_mass != 1 & age_mass != 2)
view(mass_adult)
dim(mass_adult) #273 rows, 11 columns (down from 566)
#order the levels of age_mass correctly
mass_adult$age_mass <- factor(mass_adult$age_mass, 
                              levels = c("3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))

#check if normality is any different for adults
hist(mass_adult$mass) #looks good
shapiro.test(mass_adult$mass) # NORMAL! p-value = 0.06646

#plot mass by age
ggplot(data = mass_age, aes(x = age_mass, y = mass, fill=factor(pop)))+ #can swap out mass_age for mass_adult to compare
  geom_boxplot()+
  labs(x = "Age", y = "Mass (g)", title = "mass of adult OSF with age")+
  theme_classic()+
  scale_fill_discrete(name = "Population",
                      breaks = c("TZ", "VA"),
                      labels = c("TZ", "VA"))+
   scale_fill_manual(name = "Population", 
                     values = c("blue", "green"))+
  theme(legend.position = "top")+
  theme(text = element_text(family = "Arial"))+
  theme(text = element_text(size = 12))

####### facet grid to show TZ vs VA with also status? #########
#filter out NA for status
mass.clean <- mass_age %>% #filter out the NA values for status
  filter(!is.na(EB))

ggplot(data = mass.clean, aes(x = age_mass, y = mass, fill=EB))+
  geom_boxplot()+
  facet_grid(~pop)+
  labs(x = "Age (yrs)", y = "Mass (g)", title = "mass changes with age and status")+
  theme_classic()+
  scale_fill_discrete(name = "Population",
                      breaks = c("TZ", "VA"),
                      labels = c("TZ", "VA"))+
  scale_fill_manual(name = "Status", 
                    values = c("red", "purple"))+
  theme(legend.position = "top")+
  theme(text = element_text(family = "Arial"))+
  theme(text = element_text(size = 12))
### I would like to look at above graph but with EB/OK and VA/TZ plotted on some axis - diff colours
# add column to split into groups

## subset the data
mass.VA <- subset(mass.clean, pop == "VA") #  n=556
mass.VA.EB <- subset(mass.VA, EB == "0")
mass.VA.OK <- subset(mass.VA, EB == "1")
str(mass.VA.EB) # n=167
str(mass.VA.OK) # n=303
#do same with TZ
mass.TZ <- subset(mass.clean, pop == "TZ")
mass.TZ.EB <- subset(mass.TZ, EB == "0")
mass.TZ.OK <- subset(mass.TZ, EB == "1")
str(mass.TZ.EB) # n=12
str(mass.TZ.OK) # n=74
## add columns to the subsets for EB + pop
mass.VA <- mass.VA %>% 
  mutate(status = case_when(EB == "0" ~ "EB.va", EB == "1" ~ "OK.va")) %>% 
  relocate(status, .after = EB)
mass.TZ <- mass.TZ %>% 
  mutate(status = case_when(EB == "0" ~ "EB.tz", EB == "1" ~ "OK.tz")) %>% 
  relocate(status, .after = EB)
#combine two datasets with new columns
EB_comp <- rbind(mass.TZ, mass.VA)
view(EB_comp) #566 obs again
#try graph again but now group by status
#####################################################
library(devtools)
devtools::install_github('Mikata-Project/ggthemr')
library(ggthemr)
ggthemr('dust')

######### PLOT for CHS
ggplot(data = EB_comp, aes(x = age_mass, y = mass, fill=status))+
  geom_boxplot()+
  labs(x = "Age (yrs)", y = "Mass (g)", title = "Mass Changes with Age", subtitle = "Vancouver Aquarium and Toronto Zoo")+
  theme_classic()+
  theme(panel.background = element_rect(fill = "#F6F0ED"), 
        plot.background = element_rect(fill = "#F6F0ED"), 
        legend.background = element_rect(fill = "#F6F0ED"),
        legend.box.background = element_rect(fill = "#F6F0ED"))+
  scale_fill_manual(name = "Pop Status", 
                    labels = c("EB.va" = "EB at VA", "OK.va" = "OK at VA", "EB.tz" = "EB at TZ", "OK.tz" = "OK at TZ"),
                    values = c("#663300", "#CC0000", "#CC9933","#FF9999"))+
  theme(legend.position = "right")+
  theme(text = element_text(family = "Arial"))+
  theme(text = element_text(size = 12))

## subset plot so I can introduce pieces one at a time ##
EB_VA <- subset(EB_comp, pop == "VA")
EB_TZ <- subset(EB_comp, pop == "TZ")
ggplot(data = EB_TZ, aes(x = age_mass, y = mass, fill=status))+
  geom_boxplot()+
  labs(x = "Age (yrs)", y = "Mass (g)", title = "Mass Changes with Age", subtitle = "Vancouver Aquarium and Toronto Zoo")+
  theme_classic()+
  theme(panel.background = element_rect(fill = "#F6F0ED"), 
        plot.background = element_rect(fill = "#F6F0ED"), 
        legend.background = element_rect(fill = "#F6F0ED"),
        legend.box.background = element_rect(fill = "#F6F0ED"))+
  scale_fill_manual(name = "Pop Status", 
                    labels = c("EB.tz" = "EB at TZ", "OK.tz" = "OK at TZ"),
                    values = c("#663300", "#CC9933"))+
  theme(legend.position = "right")+
  ylim(0, 150)+
  theme(text = element_text(family = "Arial"))+
  theme(text = element_text(size = 12))
####can't figure out how to make scales the same. bummer. next time. ################

#### trying to figure out how to make this a scatter plot and add curved lines? 
## or even just leave as box plots but show all four facets
plot(mass.clean$age_mass, mass.clean$mass, pch = 16, cex = 1.3,
     col = (c('red', 'blue')[as.numeric(mass.clean$pop)]),
     xlab = "Age (yrs)", ylab = "Mass (g)")
# legend("bottomright", title = "status", c("Egg bound", "Other"), fill = c('red', 'blue'), cex = 0.8)
# abline(lm(mass ~ svl, data = VA_age), col = "black")
# lm(mass ~ svl, data = VA_age)
# summary(lm(mass ~ svl, data = VA_age))
# abline(lm(mass ~ svl, data = VA_EB), col = "red")
# summary(lm(mass ~ svl, data = VA_EB))
# abline(lm(mass ~ svl, data = VA_OK), col = "blue")
# summary(lm(mass ~ svl, data = VA_OK))
# legend("topleft", title = "Regression lines", c("n=434, Adjusted R-squared: 0.7607, p-value: 2.2e-16", 
#                                                 "n=158, Adjusted R-squared: 0.7774, p-value: 2.2e-16",
#                                                 "n=276, Adjusted R-squared: 0.7577, p-value: 2.2e-16"),
#        fill = c('black', 'red', 'blue'), cex = 0.65)
# title("MbyL for full VA dataset")
##########################################

ggboxplot(mass_age, x = "pop", y = "mass", facet.by = "EB")+
  labs(x = "Population", y = "Mass (g)", title = "Fall mass by status")+
  scale_x_discrete(labels=c("VA" = "VanAqua", "TZ" = "TZoo"))


ggboxplot(mass.clean, x = "age_mass", y = "mass", facet.by = "EB")+
  labs(x = "Age (years)", y = "Mass (g)", title = "Fall mass by status - combined TZ + VA", 
       subtitle = "0 = egg bound, 1 = other")

ggboxplot(mass.clean, x = "EB", y = "mass", facet.by = "age_mass")+
  labs(x = "Status", y = "Mass (g)", title = "TZ + VA fall mass by status")+
  scale_x_discrete(labels=c("0" = "EB", "1" = "OK"))

#Regression of M by Age
#subset by zoo
ma_VA <- subset(mass_age, pop == "VA") #n=480
ma_TZ <- subset(mass_age, pop == "TZ") #n=86

plot(as.numeric(mass_age$age_mass), mass_age$mass, pch = 16, cex = 1.3,
     col = (c('blue', 'green')[as.numeric(mass_age$pop)]),
     xlab = "Age (yrs)", ylab = "Mass (g)")
legend("topleft", title = "Population", c("Toronto Zoo", "Vancouver Aquarium"), 
       fill = c('blue', 'green'), cex = 0.7)
abline(lm(mass ~ age_mass, data = mass_age), col = "black")
lm(mass ~ age_mass, data = mass_age)
summary(lm(mass ~ age_mass, data = mass_age))
abline(lm(mass ~ age_mass, data = ma_TZ), col = "blue")
summary(lm(mass ~ age_mass, data = ma_TZ))
abline(lm(mass ~ age_mass, data = ma_VA), col = "green")
summary(lm(mass ~ age_mass, data = ma_VA))
legend("bottomright", title = "Regression lines", c("n=566, Adjusted R-squared: 0.5867, p-value: 2.2e-16", 
                                                "n=86, Adjusted R-squared: 0.2408, p-value: 0.000471",
                                                "n=480, Adjusted R-squared: 0.6118, p-value: 2.2e-16"),
       fill = c('black', 'blue', 'green'), cex = 0.6)
title("Mass by age")

#try with adult only data
maa_VA <- subset(mass_adult, pop == "VA") #n=195
maa_TZ <- subset(mass_adult, pop == "TZ") #n=78

plot(as.numeric(mass_adult$age_mass), mass_adult$mass, pch = 16, cex = 1.3,
     col = (c('blue', 'green')[as.numeric(mass_adult$pop)]),
     xlab = "Age (yrs)", ylab = "Mass (g)")
legend("topleft", title = "Population", c("Toronto Zoo", "Vancouver Aquarium"), 
       fill = c('blue', 'green'), cex = 0.6)
abline(lm(mass ~ age_mass, data = mass_adult), col = "black")
lm(mass ~ age_mass, data = mass_adult)
summary(lm(mass ~ age_mass, data = mass_adult))
abline(lm(mass ~ age_mass, data = maa_TZ), col = "blue")
summary(lm(mass ~ age_mass, data = maa_TZ))
abline(lm(mass ~ age_mass, data = maa_VA), col = "green")
summary(lm(mass ~ age_mass, data = maa_VA))
legend("bottomright", title = "Regression lines", c("n=273, Adjusted R-squared: 0.126, p-value: 9.905e-07", 
                                                    "n=78, Adjusted R-squared: 0.1301, p-value: 0.02194",
                                                    "n=195, Adjusted R-squared: 0.2036, p-value: 4.825e-08"),
       fill = c('black', 'blue', 'green'), cex = 0.6)
title("Mass by age - adults only")
####can we plot a more curving line to this rather than straight line of best fit? may not be linear relationship

#######Log transformation
mass_age$log.mass = log(mass_age$mass)
ma_TZ$log.mass = log(ma_TZ$mass)
ma_VA$log.mass = log(ma_VA$mass)
mass_age$log.age = log(as.numeric(mass_age$age_mass))
ma_TZ$log.age = log(as.numeric(ma_TZ$age_mass))
ma_VA$log.age = log(as.numeric(ma_VA$age_mass))

plot(mass_age$log.age, mass_age$log.mass, pch = 16, cex = 1.3,
     col = (c('blue', 'green')[as.numeric(mass_age$pop)]),
     xlab = "Log transformed age", ylab = "Log transformed mass")
legend("topleft", title = "Population", c("Toronto Zoo", "Vancouver Aquarium"), 
       fill = c('blue', 'green'), cex = 0.6)
abline(lm(log.mass ~ log.age, data = mass_age), col = "black")
lm(log.mass ~ log.age, data = mass_age)
summary(lm(log.mass ~ log.age, data = mass_age))
abline(lm(log.mass ~ log.age, data = ma_TZ), col = "blue")
summary(lm(log.mass ~ log.age, data = ma_TZ))
abline(lm(log.mass ~ log.age, data = ma_VA), col = "green")
summary(lm(log.mass ~ log.age, data = ma_VA))
legend("bottomright", title = "Regression lines", c("n=566, Adjusted R-squared: 0.659, p-value: 2.2e-16", 
                                                    "n=86, Adjusted R-squared: 0.2095, p-value: 5.587e-06",
                                                    "n=480, Adjusted R-squared: 0.6845, p-value: 2.2e-16"),
       fill = c('black', 'blue', 'green'), cex = 0.8)
title("Log of mass by age")

################################################################################################
####### Let's do some ANOVAs and/or Kruskal-Wallis tests to see if differences are significant
################################################################################################
####### Compare SMI among populations (using full_21 dataset and looking btw 'populations')

#Hypotheses
#Null: There is no difference in the means of factor A, B, C, D
#Alternative: There is a difference in the means of factor A, B, C, D

#Assumptions:
#Randomly sampled
#Independently sampled - this is true because single year snapshot so only one measurement per frog
#Normality - my variables going in (mass & svl) were not normal but need to check for calculated SMI
####ANOVA is "robust" against non-normality when large sample sizes are at play. 
####Test normality with shapiro-wilks, q-q plots and histogram
#Equal variance - test with bartlett

#Design:
#Balanced = sample sizes of all groups are the same
#Unbalanced = sample sizes of all groups are not the same
##If unbalanced, need to use alternative SS calculation (type IV)

######### Case 1: SMI ~ population #######
######## snapshot of 2021 spring SMI #####
# Continuous dependent variable: SMI
# Categorical independent variables: GVZ, TZ, VA, Wild
#Unbalanced design, unequal sample sizes

##################################################################
# Test assumptions
hist(full_21$SMI) #looking for bell shape
# Histogram looks skewed, long right tail

qqnorm(full_21$SMI) #looking for straight line
#looks quite sloped

#Shapiro Wilks test: null hypothesis = variance of data is normally distributed
#check by population
shapiro.test(subset(full_21, population == "GVZ")$SMI) # p-value: 2.918e-05
shapiro.test(subset(full_21, population == "TZ")$SMI) # p-value: 0.6497
shapiro.test(subset(full_21, population == "VA")$SMI) # p-value: 0.0002709
shapiro.test(subset(full_21, population == "wild")$SMI) # p-value: 0.5215
# TZ and wild are normal (p > 0.05) but GVZ and VA are not.
# TZ n=9, wild n=45, GVZ n=41, VA n=15
# TZ may have come out as normal just because of very small sample size 

#try transformation
full_21$log.SMI <- log(full_21$SMI)
plot(full_21$log.SMI)
hist(full_21$log.SMI) #looks slightly better
qqnorm(full_21$log.SMI) #better but still not great
shapiro.test(subset(full_21, population == "GVZ")$log.SMI) # p-value: 0.001568
shapiro.test(subset(full_21, population == "TZ")$log.SMI) # p-value: 0.906
shapiro.test(subset(full_21, population == "VA")$log.SMI) # p-value: 0.01761
shapiro.test(subset(full_21, population == "wild")$log.SMI) # p-value: 0.2524
###  GVZoo and VA are still not normal - but much closer than they were. 
# GVZoo has highest sample size of other zoos
# Will still use log transformed data - with higher sample size GVZoo can be considered normal ???

#####################################################################################################

# Test assumption of homogeneity of variance on transformed data
# Null hypothesis of bartlett: variance is equal

plot(log.SMI~population, data = full_21)
bartlett.test(log.SMI~population, data = full_21)
# p-value: 2.227e-05 - reject the null hypothesis
# the variance is not homogenous so we cannot proceed to ANOVA
# Alternative tests are Welch-ANOVA or Kruskal-Wallis
# Kruskal-Wallis does not require normality or homoscedasticity of variances
# Let's use our untransformed data in Kruskal-Wallis test because KW does not require normality

#####################################################################################################
# Kruskal-Wallis
kruskal.test(SMI~population, data = full_21)
# chi-squared = 65.485, p-value = 3.95e-14
# p < 0.05 so we reject the null hypotheses of all mean SMI being equal between populations
# but we still do not know WHICH population means differ - just know at least one is different

# need to do post-hoc test to determine which populations differ
# Use Dunn Test (though could also use pairwise.wilcox.test())
library(FSA) #see citation('FSA') if used in publication
dunnTest(SMI~population, data = full_21, method = "holm")
# if last column (adjusted p-value) is < 0.05 then the indicated means differ significantly
# GVZ - TZ: significant
# GVZ - VA: significant 
# TZ - VA: NOT significant = means of VA and TZ do not differ significantly
# GVZ - wild: p.adj = 3.378e-04 #### significant
# TZ - wild: significant (barely though - much closer than others)
# VA - wild: significant

###### we see that all means differ significantly EXCEPT VA from TZ - but ALL zoos differ significantly from wild

# Plot statistical results to show significant differences
library(ggstatsplot)

ggbetweenstats(
  data = full_21,
  x = population,
  y = SMI,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE
)

######### Case 1: SMI ~ age #######
#####SMI of VanAqua frogs by age from 2011-2022#####
# Continuous dependent variable: SMI
# Categorical independent variables: GVZ, TZ, VA, Wild
#Unbalanced design, unequal sample sizes