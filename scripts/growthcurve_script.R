getwd()
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
growth$age <- factor(growth$age, 
                            levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
                            ordered = TRUE)
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
view(gc) # growth curve data set, n = 566

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

### Looking for interactions......  multiple explanatory variables and few subjects... don't have enough observations per level - simplify
# 1. Start with a model with no interactions. Apply the model, model selection, and
# model validation. If the validation shows that there are patterns in the residuals, investigate why. Adding interactions may be an option to improve the
# model.
# 2. Use biological knowledge to decide which, if any, interactions are sensible to
# add.
# 3. Apply a good data exploration to see which interactions may be important.


################################################
##### 1. Looking for outliers ##################
################################################
library(AED)
op <- par(mfrow = c(2, 2), mar = c(3, 3, 3, 1))
dotchart(gc$mass, main = "Mass", group = gc$pop)
dotchart(gc$age, main = "Age", group = gc$pop)
dotchart(gc$mass, main = "Mass by status", group = gc$EB)
dotchart(gc$age, main = "Age by status", group = gc$EB)
### doesn't seem to be any clear outliers
### mass often requires transformation however - keep going for now but keep this in mind

# 2. Pairwise plots for collinearity 
cor(gc[, c(11:12)]) # correlation coefficients
# can't do correlation coef for EB or pop because they are not numeric 
library(GGally)
ggpairs(gc[, c(2, 9, 11:12)]) 
# look at matrix of variables plotted against each other - doesn't work super well because of nominal variables
## Correlation coef between mass and age is 0.683 - not high enough to be concerned about? we expect these variables to be related

M1 <- lm(mass ~ age + pop + EB, data = gc) # couldn't find way to calculate VIF values without modeling variables first...
summary(M1)
library(car)
vif(model) # Variance Inflation Factors 
# no VIF values above 2 meaning there are no strong correlations = GOOD stuff

## Conclusion: Keep all variables in the analysis 
################################################
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
##### should this really be a LINEAR model (lm)? Looks more like a quadratic ? #####

## Model Validation
op <- par(mfrow = c(2, 2))
plot(M3)
E <-rstandard(M3)
hist(E) # normality
qqnorm(E) # normality
# check for independence and homogeneity: residuals vs explanatory variables
plot(y = E, x = gc$age, xlab = "Age", ylab = "Residuals") # potential heterogeneity as more spread in old ages
abline(0,0)
plot(E ~ gc$pop, xlab = "Pop", ylab = "Residuals")
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
gc$f.pop <- factor(gc$pop, levels = c(0, 1),
                      Labels = c("VA", "TZ"))