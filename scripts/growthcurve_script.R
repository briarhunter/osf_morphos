getwd()
library(assertr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(reshape2)
library(data.table)
# dataset also used in SMI_script but load here separately to keep clean
mass.wide <- read.csv("processed data/fall_mass_byage.csv", header = TRUE)
#transform into long form
growth <- melt(setDT(mass.wide), id=1:9, measure=patterns("^mass"),
                 value.name=c("mass"), variable.name="ageof", na.rm = TRUE)
growth <- growth %>% 
  mutate(age = case_when(ageof == 'mass0' ~ '0',
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
  relocate(age, .after = EB)
growth$ageof <- NULL  #can now drop the ageof column since we have proper age   
growth$age_yrs <- NULL # don't need this extra age column - does not represent age of frogs when weighed
view(growth)
dim(growth) #566 rows, 10 columns
growth <- growth %>% #filter out the NA values for EB so we only have known frogs
  filter(!is.na(EB))

# subset out the data so we can add column for combined status (could be better way but don't know)
g.VA <- subset(growth, pop == "VA")
g.TZ <- subset(growth, pop == "TZ")
g.VA <- g.VA %>% 
  mutate(status = case_when(EB == "0" ~ "EB.va", EB == "1" ~ "OK.va")) %>% 
  relocate(status, .after = EB)
g.TZ <- g.TZ %>% 
  mutate(status = case_when(EB == "0" ~ "EB.tz", EB == "1" ~ "OK.tz")) %>% 
  relocate(status, .after = EB)
gc <- rbind(g.TZ, g.VA)
view(gc) # growth curve data set, n = 556 (NAs were removed)
gc$status <- as.factor(gc$status)
gc$pop <- as.factor(gc$pop)
gc$age <- as.numeric(gc$age)
gc$ID <- as.factor(gc$ID)
gc$EB <- as.factor(gc$EB)

##########################################################################################################
############ Modeling growth curve for Vancouver Aquarium and Toronto Zoo Oregon Spotted Frogs ###########
# Goal: to compare mass of egg bound frogs to others in their pop at each age, and also growth over time #
##########################################################################################################
# Design #
######## Longitudinal, repeated measures - mass measurements are nested within subjects over time (age)
########################################################################################
# Response variable (continuous dependent variable): mass (g)
# Explanatory (independent) variables: 
######### Age (years) - categorical or continuous, repeated measure 
######### Status (EB or OK) - dichotomous categorical (grouping factor)
######### Pop (VA or TZ) - dichotomous categorical (grouping factor)
#########################################################################################################
## Status and Pop are crossed - they occur at each level of each factor (EB.tz, EB.va, OK.tz, OK.va)
## Frog_id (subject) is nested in both Status and Pop as each frog is only at one level of each (VA or TZ, EB or OK)
## Frog_id is supposed to be crossed with Age, but some measurements are missing (should have each frog at each age, but not all present)
#########################################################################################################
# Unbalanced design, unequal sample sizes (may need to group or remove some due to n < 3)
#########################################################################################################


## DATA EXPLORATION ##

## 1. Outliers in the response and explanatory variables
######### Cleveland dotplots - dotchart(df$variable, main = "Title", group = df$group)
######### (isolated points at far ends and either sides suggest potential outliers. if observations stand out for all variables = bad)
######### might suggest a transformation is required
## 2. Collinearity of explanatory variables 
######### 3 tools: pairwise scatterplots, correlation coefficients, and variance inflation factors (VIF)
######### how to interpret pairwise plots?
######### calculate VIF using car package - values below 3 mean no collinearity 
## 3. Relationships between the response variable and explanatory variables
######### graph response variable and multiple explanatory to look for interactions: coplot and xyplot are good or plot.design from design package

################################################
##### 1. Looking for outliers ##################
################################################
op <- par(mfrow = c(2, 2), mar = c(3, 3, 3, 1))
dotchart(gc$mass, main = "Mass", group = gc$pop)
dotchart(gc$age, main = "Age", group = gc$pop)
dotchart(gc$mass, main = "Mass by status", group = gc$EB)
dotchart(gc$age, main = "Age by status", group = gc$EB)
### doesn't seem to be any clear outliers
### mass often requires transformation however - keep going for now but keep this in mind

################################################
##### 2. Look for collinearity #################
################################################

# Pairwise plots for collinearity 
cor(gc[, c(10:11)]) # correlation coefficients
# can't do correlation coef for EB or pop because they are not numeric 
library(GGally)
ggpairs(gc[, c(2, 8, 10:11)]) 
# look at matrix of variables plotted against each other - doesn't work super well because of nominal variables
## Correlation coef between mass and age is 0.683 - not high enough to be concerned about? we expect these variables to be related

M1 <- lm(mass ~ age + pop + EB, data = gc) 
summary(M1)
library(car)
vif(M1) # Variance Inflation Factors 
# no VIF values above 2 meaning there are no strong correlations = GOOD stuff

## Conclusion: Keep all variables in the analysis 

################################################
##### 3. Look for interactions ##################
################################################

#Basic linear regression of mass ~ age
lm <- (lm(mass ~ age, data = gc))
par(mfrow =c(2,2))
plot(lm) # indicates heterogeneity ?
hist(residuals(lm)) # looks normal
acf(rstandard(lm)) #look for autocorrelation: looks ok. drops off 
######### above from Jenna - keep??? ############

## My thoughts process
# what interactions make sense? the effect of a frog being egg bound or not on mass might depend on which pop
# they are from (VA or TZ) and vice versa (impact of pop depends on being EB or not)
# and impact of age on mass might depend on which pop and status a frog is 
# age*pop*EB = age + pop + EB + age:pop + age:EB + pop:EB + age:pop:EB
# age + pop*EB + age:pop + age:EB = age + pop + EB + pop:EB + age+pop + age:EB

######## Basic linear model ######## (start with full model- all potential variables and interactions)
gc$age2 <- gc$age^2 - mean(gc$age^2) #add a quadratic - subtracting the mean 'centres the quadratic to reduce collinearity" ?
LM <- lm(mass ~ age2 + age + pop * EB + age:pop + age:EB, data = gc)
par(mfrow = c(2,2))
plot(LM) # the distribution of residuals looks fairly normal (should be horizontal line at 0)
# Residuals vs leverage looks wonky ? 
# Cook's is less than 1 so ok, one point way out right in Residuals vs Leverage

acf(rstandard(LM)) # check for autocorrelation: looks good! 

# Diagnostics: Cook's distance > 1 is bad; 

################################################################
### Question: should I be including full three-way interaction of age:pop:EB ? 
# If I do include this, model fit comparison leaves me with best LMM being: 
# lm1 <- lme(mass ~ age2 + age * pop * EB, data = gc, random = ~1 | fage, method = "ML")
# but many of those terms are not actually significant, but better AIC than when they are removed... 
################################################################

######## Generalized Linear Model ########
GLM <- gls(mass ~ age2 + age + pop * EB + age:pop + age:EB, data = gc, method = "ML") # null model; no random effects
summary(GLM) # significant = age2, age, age:pop, age:EB

######## Optimal random structure ######## (fit LMM with fixed effects and different random structures)
gc$fage <- factor(gc$age, 
                  levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
                  ordered = TRUE)

lm1 <- lme(mass ~ age2 + age + pop * EB + age:pop + age:EB, data = gc,
           random = ~1 | fage, method = "ML")
lm2 <- lme(mass ~ age2 + age + pop * EB + age:pop + age:EB, data = gc,
           random = ~1 | pop, method = "ML") 
lm3 <- lme(mass ~ age2 + age + pop * EB + age:pop + age:EB, data = gc,
            random = ~1 | EB, method = "ML")
lm4 <- lme(mass ~ age2 + age + pop * EB + age:pop + age:EB, data = gc,
           random = ~1 | pop/EB, method = "ML") # EB is nested within pop
anova(GLM, lm1, lm2, lm3, lm4) # compare models
# GLM actually has best (lowest) AIC... do we need random effects to account for age ?
# Of the LMMs, lm1 has slightly smaller AIC (and BIC) - best LMM model fit
# this also makes the most biological sense

### no random slope because strength of relationship between mass and age should be ~same for all frogs
### each pop (and EB?) could have different baseline (intercept) but there is no slope for discrete variables

plot(ranef(lm1, level = 1)) # distribution of random intercepts
# a few (3) are biased towards negative values

# QQ plots to compare residual distribution to GLM and lm1
par(mfrow = c(1,2))
lims <- c(-3.5,3.5)
qqnorm(resid(GLM, type = "normalized"),
       xlim = lims, ylim = lims,main = "GLM")
abline(0,1, col = "red", lty = 2)
qqnorm(resid(lm1, type = "normalized"),
       xlim = lims, ylim = lims, main = "lm1")
abline(0,1, col = "red", lty = 2)
# look basically the same

######## Optimal fixed structure ########
summary(lm1) # see pop, EB, pop:EB are not significant
# compare models by removing least significant term each round, first = popVA:EB1
lm1.2 <- lme(mass ~ age2 + age + pop + EB + age:pop + age:EB, data = gc,
           random = ~1 | fage, method = "ML")
summary(lm1.2) # least sig: popVA
lm1.3 <- lme(mass ~ age2 + age + EB + age:pop + age:EB, data = gc,
           random = ~1 | fage, method = "ML")
summary(lm1.3) #least sig: EB1 - but this is still significant (p<0.05)
lm1.4 <- lme(mass ~ age2 + age + age:pop + age:EB, data = gc,
           random = ~1 | fage, method = "ML")
# compare models using AIC 
anova(GLM, lm1, lm1.2, lm1.3, lm1.4)
### model lm1.3 has the lowest AIC (4827.051)

#update first GLM to reflect new fixed effect structure and compare
GLM1 <- gls(mass ~ age2 + age * EB + age:pop, data = gc, method = "ML")
anova(GLM1, lm1.3) # the updated GLM1 actually has a lower AIC than our best LMM model 
## does this indicate we don't need random effects? Do we need them to account for non-independence by age?

######## Fit optimal model with REML ########
finalmodel <- lme(mass ~ age2 + age * EB + age:pop, data = gc,
                  random = ~1 | fage, method = "REML") # model form updated to be more concise 

dev.off() # Reset previous graphical pars
# New GLM
GLM1.2 <- gls(mass ~ age2 + age * EB + age:pop, data = gc, method = "REML") #update final GLM to REML
# Plot side by side, beta with respective SEs
plot(coef(GLM1.2), xlab = "Fixed Effects", ylab = expression(beta), axes = F,
     pch = 16, col = "black", ylim = c(-3.2,9))
stdErrors <- coef(summary(GLM1.2))[,2]
segments(x0 = 1:6, x1 = 1:6, y0 = coef(GLM1.2) - stdErrors, y1 = coef(GLM1.2) + stdErrors,
         col = "black")
axis(2)
abline(h = 0, col = "grey", lty = 2)
axis(1, at = 1:6,
     labels = c("Intercept", "Age2", "Age", "EB(1)","Age:Pop(VA)", "Age:EB(1)"), cex.axis = .7)
# LMM
points(1:6 + .1, fixef(finalmodel), pch = 16, col = "red")
stdErrorsLMM <- coef(summary(finalmodel))[,2]
segments(x0 = 1:6 + .1, x1 = 1:6 + .1, y0 = fixef(finalmodel) - stdErrorsLMM, y1 = fixef(finalmodel) + stdErrorsLMM, col = "red")
# Legend
legend("topright", legend = c("GLM","LMM"), text.col = c("black","red"), bty = "n")
# residuals for Age and Intercept do not make it on the graph 
# looks basically the same between GLM and LMM, with slightly smaller SE (bars) on Age:EB for LMM
plot(finalmodel)
summary(finalmodel)

###### Conclusions ######
# growth follows a quadratic form over time (as frog ages) - meaning mass levels out at older ages 
# how to interpret age and age2 ? 
# Non egg bound frogs (EB: 1) have a greater mass than egg bound frogs (+6.29, p = 0.018) - does it make sense to keep this?
# As they age, egg bound frogs (EB: 0) have greater mass than other frogs (+2.67, p < 0.01)
# As they age, Vancouver Aquarium frogs have a greater mass than Toronto Zoo frogs (+2.10, p < 0.01)



#################################################################################################
#### Old, more complicated method of modeling based on Zuur et al. 2009 #########################
#################################################################################################

## Look for relationships between response and explanatory variables (before plotting interactions in the linear regression)

# Start with no interactions - we have already modeled this: 
summary(M1) # have EB1 and pop1 because EB0 and other pop are used as baseline comparisons (EB1 is comparison to EB0)
drop1(M1, test="F") #drops one variable at a time; <none> indicates no variables were dropped - the full model
#################### EB variable is not significant, and the model with EB removed has the lowest AIC (best model)
###### So we could (should?) remove the EB variable
anova(M1) #again shows that EB is not significant, though the F value still drops quite a bit from the pop model

# Model Selection - use a backwards selection based on the AIC (Akaike information criteria)
step(M1) #the lower the AIC the better the model (as judged by AIC)
# the model without EB has the best AIC (3402.69) - summary and anova showed us previously that both of these were sig
# but let's double check with a new fitted model
M2 <- lm(mass ~ age + pop, data = gc)
M3 <- lm(mass ~ age * pop, data = gc) # and check if there is an interaction
summary(M2) #R-squared is still low: 0.4806
summary(M3) #R-squared is better: 0.5337 - all terms (and interaction) are significant
anova(M2)
anova(M3)
# compare with or without interaction (though interactions seems significant)
library(AICcmodavg)
model.set <- list(M1, M2, M3)
model.names <- c("full model", "two way", "interaction")
aictab(model.set, modnames = model.names)
## this again indicates the interaction model is best - lowest AICc
## this fit is still only R2: 0.5337

## Model Validation
op <- par(mfrow = c(2, 2))
plot(M3) #residuals plots indicate there is heterogeneity - lines are not horizontal
E <-rstandard(M3)
hist(E) # normality
qqnorm(E) # normality
# check for independence and homogeneity: residuals vs explanatory variables
plot(y = E, x = gc$age, xlab = "Age", ylab = "Residuals") # potential heterogeneity as more spread in old ages
abline(0,0)
plot(E ~ gc$pop, xlab = "Pop", ylab = "Residuals")
abline(0,0)
par(op)


#### Stopped before A.3.3 Model Interpretation, pg 543 ####
########################################################################################
#### Go look at CH 5-7 and 12 for mixed effect models for repeated measures designs ####
########################################################################################

# all the above checks were telling me to drop EB as a variable - but what if there is interaction btw EB and pop ?

library(lattice)
xyplot(gc$mass ~ gc$age | gc$status, col = 1)

# Decaying polynomial - aka quadratic model instead of linear growth - add an age^2 term
# specify structure of variance-covariance model: unstructured for growth models () 
# allows distinct estimates

# test out different correlation structures to see what the variable relationship is 
# linear vs quadratic (rational quadratic = RATIO), exponential, (EXPO), Gaussian (normal), etc

# Starting model - linear regression
# GrowthRateij = α + β1 × Ageij + β2 × Age2ij + β3 × Statusij + β4 × Popij + εij
###### Age included as a quadratic, and status and pop are included as dummy variables (0 and 1)
# EB is already as dummy variable: 0 (EB) or 1 (OK)
coplot(mass ~ age | pop * EB,
       data = gc, xlab = c("Age (yrs)"))
xyplot(mass ~ age | status, data = gc,
         panel=function(x, y){
           panel.xyplot(x, y, col = 1, cex = 0.5, pch = 1)
           panel.grid(h = -1, v = 2)
           panel.abline(v = 0, lty = 2)
           if (length(x) > 5) panel.loess(x, y, span = 0.9,
                                          col = 1, lwd = 2)
         })
# the linear regression:
gc$age2 <- gc$age^2 - mean(gc$age^2) #add the quadratic - subtracting the mean 'centres the quadratic to reduce collinearity" ?
M.lm <- lm(mass ~ age + age2 + pop + EB, data = gc) # start with as many explanatory variables as possible
drop1(M.lm, test = "F")
# all but EB are significant. model without EB has the lowest AIC

######## Plot residuals vs fitted #######
#### well-behaved plot should bounce randomly and form roughly horizontal line around 0 
#### no data points should stand out from basic random pattern of other residuals
plot(gc.lm, which = c (1)) 
#Interpret: we see a horizontal line at 0 but we see distinct vertical patterns - suggests heterogeneity
# distinct vertical patterns (instead of random bounce) suggest fit is not linear (we suspect quadratic)

library(nlme) 
# graphical validation for linear regression model, fitted with gls
M.gls <- gls(mass ~ age + age2 * pop * EB, data = gc) # AIC (by summary) = 4838.13
M1.gls <- gls(mass ~ age + age2 * pop * EB, data = gc) # add interactions
summary(M1.gls) # AIC = 4828.857; age2:popVA interaction significant, 3 way interaction (age2:popVA:EB) sig
# when interactions are added (M1 vs M) pop is no longer significant but interactions with it are

E <- resid(M1.gls)
op <- par(mfrow = c(2,2))
boxplot(E ~ gc$pop, main = "Population")
abline(0,0)
boxplot(E ~ gc$EB, main = "Status: 0 = egg bound")
abline(0,0)
boxplot(E ~ gc$EB * gc$pop, main = "Status & Population")
abline(0,0)
boxplot(E ~ gc$age, main = "Age")
abline(0,0)
par(op)
#Interpretation: more variation in residual spread for VA but equal for both status ??? learn more
# Should include Age as explanatory variable - random effect (allows for correlation in values btw years)

## Step 3: choose variance structure
## suggested varIdent due to variation in residuals, using variance covariate Population (also EB or no?)
gc$fage <- factor(gc$age, 
                      levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
                      ordered = TRUE)
M1.lme <- lme(mass ~ age + age2 * pop * EB, data = gc,
              weights = varIdent(form =~ 1 | pop), #variance structure
              random =~ 1 | fage, method = "REML") # age as a factor modeled as random effect

########## Step 4. find optimal structures #########
# with or without variance structure:
M2.lme <- lme(mass ~ age + age2 * pop * EB, data = gc,
              random = ~1 | fage, method = "REML")
summary(M1.lme) # AIC = 4825.316 (with varIdent); same significance terms as M1.gls
summary(M2.lme) # AIC = 4827.402 (without varIdent); BIC slightly lower
### suggests variance structure doesn't make much difference (?), so simpler model is to exclude

# optimal random structure
### random intercept because baseline mass for each frog differs, 
### no random slope because strength of relationship between mass and age should be ~same for all frogs
### each pop (and EB?) could have different baseline (intercept) but there is no slope for discrete variables

# B1 <- gls(mass ~ age + age2 + pop + EB, data = gc,
#           method = "REML") ### Trying to plot without random structure but not working 
#### want to compare to having random intercept 

M3.lme <- lme(mass ~ age + age2 * pop * EB, data = gc,
          random = ~1 | ID, method = "REML")
summary(M3.lme) # baseline mass varies by frog... 
AIC(M1.lme, M2.lme, M3.lme) # M3 has lowest AIC - but we still want to include random effects of fage ...

# B2 <- lme(mass ~ age + age2 + pop + EB, data = gc,
#           random = ~1 + fage | ID, method = "REML")
## can't do above because "fewer observations than random effects in all level 1 groups ##

###### want to account for variance between age groups, but should intercept vary by frog or age?
###### should slope variance be by age ?

########## Step 5: optimal fixed structure ############
# should we have included interaction of explanatory variables in fixed effects? 
M4 <- lme(mass ~ age * pop + age2 + EB, data = gc,
              random = ~1 | fage, method = "REML")
summary(M4) #AIC: 4829.058; significant = age, age2, age:popVA (interaction is significant)

M5 <- lme(mass ~ age + age2 * pop * EB, data = gc,
          random = ~1 | fage, method = "REML")
summary(M5) ### supposed to start with full model (most interactions) but should quadratic come first?
### how is it nested? 

