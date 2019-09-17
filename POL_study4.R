# set the working directory
getwd()

setwd("/gabriellechristinemartin/Desktop/R_Pol_data_scripts/Paper Study 4")

# clear the environment
rm(list=ls())

library(tidyverse)
library(haven)

#1. IMPORTING, VIEWING, AND SELECTING THE DATA ----

#create data.frame called pols1 from the spss (.sav) file
pols1 <- data.frame(read_sav("/Users/gabriellechristinemartin/Desktop/R_Pol_data_scripts/Paper Study 4/POL_Expt4_original.sav"))

#returns class: data.frame
class(pols1)

#provides the structure of the data: number observations, number variables, variable names, variable type/classes 
#plus a preview of the data
glimpse(pols1)

library(naniar)

#Use replace_with_na_all() when you want to replace ALL values that meet a condition across an entire dataset. 
#The syntax here is a little different, and follows the rules for rlang’s expression of simple functions. 
#This means that the function starts with ~, and when referencing a variable, you use .x.
pols1 <- pols1 %>%
  replace_with_na_all(condition = ~.x == "")

#pulls out all rows for only the columns in pols1 that we need to examine the primary analyses and report demographics
#sets all vars to numeric except for the factor vars
#recodes the two string condition vars to factors
#recodes the string indicator of whether the participant got to the end of the study: complete
#recodes string sex to factor gender

pols1_abbrev <- pols1 %>%
  dplyr::select(condpolarize, condbroken, attsimilar, attdifferent, attoverlap,
         brokensystem, systemfailing, electflawed, electpoorfunct,
         desirepersonal, desireothers, desirebenefit, 
         thirdsupport1, thirdsupport2, thirdsupport3, thirdsupport4, thirdsupport5, thirdsupport6,
         thirdsupport7, thirdsupport8, fixsystem1, fixsystem2, fixsystem3, fixsystem4, fixsystem5,
         fixsystem6, fixsystem7, fixsystem8, fixsystem9, fixsystem10,
         X._session_status, polorient, partyid, age, sex, education, ethnicityom, raceomb) %>%
      mutate(partyid = as.factor(partyid)) %>%
      mutate(education = as.factor(education)) %>%
      mutate(ethnicityom = as.factor(ethnicityom)) %>%
      mutate(raceomb = as.factor(raceomb)) %>%
  mutate(polar_cond = as.factor(case_when( 
      condpolarize == "manipinstmedpol" ~ 2,
      condpolarize ==  "manipinsthighpol" ~ 3,
      condpolarize ==  "manipinstlowpol" ~ 1))) %>%
  mutate(broken_cond = as.factor(case_when( 
    condbroken == "manipinstbroken" ~ 2,
    condbroken ==  "manipinstnotbroken" ~ 1))) %>%
  mutate(complete = as.factor(case_when(
        X._session_status == "C" ~ 1,
        X._session_status == "null" ~ 0))) %>%
      mutate(gender = as.factor(case_when(
        sex == "m" ~ 1,
        sex == "f" ~ 2))) 

glimpse(pols1_abbrev)

#checking to make sure the ns in each factor level are the same after re-coding into new var

table(pols1_abbrev$condpolarize)
table(pols1_abbrev$polar_cond)
class(pols1_abbrev$polar_cond)

table(pols1_abbrev$condbroken)
table(pols1_abbrev$broken_cond)
class(pols1_abbrev$broken_cond)

table(pols1_abbrev$X._session_status)
table(pols1_abbrev$complete)
class(pols1_abbrev$complete)

table(pols1_abbrev$sex)
table(pols1_abbrev$gender)
class(pols1_abbrev$gender)

# set factor levels 

levels(pols1_abbrev$complete) <- c("P exited study early", "P reached study end")
summary(pols1_abbrev$complete)

levels(pols1_abbrev$polar_cond) <- c("Low Polarization", "Medium Polarization","High Polarization")
summary(pols1_abbrev$polar_cond)

levels(pols1_abbrev$broken_cond) <- c("Not Broken", "Broken")
summary(pols1_abbrev$broken_cond)

levels(pols1_abbrev$partyid) <- c("Democrat", "Republican", "Independent", "Green", "Libertarian", "Other")
summary(pols1_abbrev$partyid)

levels(pols1_abbrev$gender) <- c("Male", "Female")
summary(pols1_abbrev$gender)

levels(pols1_abbrev$ethnicityom) <- c("hispanic or latino", "not hispanic or latino", "unknown")
summary(pols1_abbrev$ethnicityom)

levels(pols1_abbrev$raceomb) <- c("american indian/alaskan native", "east asian", "south asian", "native hawiian or other pacific islander",
                                  "black or african american", "white", "more than one race- Black/White", "more than one race- other",
                                  "other or unknown")
summary(pols1_abbrev$raceomb)

levels(pols1_abbrev$education) <- c("elementary school", "junior high", "some high school", "high school graduate", "some college",
                                    "associates degree", "bachelors degree", "some graduate school", "masters degree",
                                    "JD", "MD", "PhD", "other advanced degree", "MBA")
summary(pols1_abbrev$education)
  

# this is what we report in the manuscript: complete_DV & age > 18 (ignoring whether Ps got to the end)

# dropped 826 incomplete DV (all three DV responses not present), n = 1345
#droped Ps under 18 (n = 23): results in 1322 responses
# final n = 1322; same as manu 

pols1_abbrev2 <- pols1_abbrev %>%
  dplyr::select(polar_cond, broken_cond, attsimilar, attdifferent, attoverlap,
         brokensystem, systemfailing, electflawed, electpoorfunct,
         desirepersonal, desireothers, desirebenefit, 
         thirdsupport1, thirdsupport2, thirdsupport3, thirdsupport4, thirdsupport5, thirdsupport6,
         thirdsupport7, thirdsupport8, fixsystem1, fixsystem2, fixsystem3, fixsystem4, fixsystem5,
         fixsystem6, fixsystem7, fixsystem8, fixsystem9, fixsystem10,
         complete, polorient, partyid, age, gender, education, ethnicityom, raceomb) %>%
  mutate(complete_DV = (as.factor(ifelse(
    desirepersonal > -99 & desirebenefit > -99 & desireothers > -99, 1, NA)))) %>%
  filter(complete_DV == 1) %>%
  filter(age >= 18)
  
glimpse(pols1_abbrev2)

summary(pols1_abbrev2$age)
sd(pols1_abbrev2$age)
summary(pols1_abbrev2$gender)
summary(pols1_abbrev2$partyid)
summary(pols1_abbrev2$polorient)
sd(pols1_abbrev2$polorient, na.rm = TRUE)
summary(pols1_abbrev2$raceomb)
table(pols1_abbrev2$polar_cond)

pols1_abbrev3 <- pols1_abbrev2 %>%
  mutate(polorient17 = (ifelse(
    polorient == 8, NA, polorient))) 

table(pols1_abbrev3$polorient17)
summary(pols1_abbrev3$polorient17)
sd(pols1_abbrev3$polorient17, na.rm = TRUE)



#2. EXAMINING ITEM DISTRIBUTIONS AND SUMMARY STATS ----

# ugly but quick and dirty histograms for each item in primary analyses
# most items look mostly normally-distributed; 

# desire DV items
hist(pols1_abbrev2$desirepersonal)
hist(pols1_abbrev2$desirebenefit)
hist(pols1_abbrev2$desireothers)

summary(pols1_abbrev2$desirepersonal)
sd(pols1_abbrev2$desirepersonal)

summary(pols1_abbrev2$desirebenefit)
sd(pols1_abbrev2$desirebenefit)

summary(pols1_abbrev2$desireothers)
sd(pols1_abbrev2$desireothers)

# polarization manip check items
hist(pols1_abbrev2$attsimilar)
hist(pols1_abbrev2$attdifferent)
hist(pols1_abbrev2$attoverlap)

summary(pols1_abbrev2$attsimilar)
sd(pols1_abbrev2$attsimilar, na.rm = TRUE)

summary(pols1_abbrev2$attdifferent)
sd(pols1_abbrev2$attdifferent, na.rm = TRUE)

summary(pols1_abbrev2$attoverlap)
sd(pols1_abbrev2$attoverlap, na.rm = TRUE)


# brokenness manipulation check items
# distributions look a little low on the high end and a little high on the low end

hist(pols1_abbrev2$brokensystem)
hist(pols1_abbrev2$systemfailing)
hist(pols1_abbrev2$electflawed)
hist(pols1_abbrev2$electpoorfunct)

summary(pols1_abbrev2$brokensystem)
sd(pols1_abbrev2$brokensystem, na.rm = TRUE)

summary(pols1_abbrev2$systemfailing)
sd(pols1_abbrev2$systemfailing, na.rm = TRUE)

summary(pols1_abbrev2$electflawed)
sd(pols1_abbrev2$electflawed, na.rm = TRUE)

summary(pols1_abbrev2$electpoorfunct)
sd(pols1_abbrev2$electpoorfunct, na.rm = TRUE)

#3. CHECKING FOR SCALE RELIABILITY AND CREATING SCALES ----

#get the reliability for the three polarization manipulation check items: alpha = .89

library(psych)

#reverse-code attsimilar and attoverlap before testing alpha
# then, combine with attdifferent by taking the average

pols1_abbrev2 <- pols1_abbrev2 %>%
  mutate(attsimilar_r = 
           6 - attsimilar) %>%
  mutate(attoverlap_r = 
           6 - attoverlap) %>%
  mutate(pol_check_scale =
           ((attsimilar_r + attoverlap_r + attdifferent)/3)) 

table(pols1_abbrev2$attsimilar)
table(pols1_abbrev2$attsimilar_r)

table(pols1_abbrev2$attoverlap)
table(pols1_abbrev2$attoverlap_r)

polar_scale <- select(pols1_abbrev2, attsimilar_r, attdifferent, attoverlap_r)

psych::alpha(polar_scale)

pols1_abbrev2 %>%
  group_by(polar_cond, broken_cond) %>%
  summarize(
    mean(pol_check_scale, na.rm = TRUE),
    sd(pol_check_scale, na.rm = TRUE),
    n())

#get the reliability for the four brokenness manipulation check items: alpha = .92
# no need to reverse-code

broken_scale <- select(pols1_abbrev2, brokensystem, systemfailing, electflawed, electpoorfunct)

psych::alpha(broken_scale)

pols1_abbrev2 <- pols1_abbrev2 %>%
  mutate(broken_check_scale =
           ((systemfailing + electpoorfunct + brokensystem + electflawed)/4)) 

pols1_abbrev2 %>%
  group_by(polar_cond, broken_cond) %>%
  summarize(
    mean(broken_check_scale, na.rm = TRUE),
    sd(broken_check_scale, na.rm = TRUE),
    n())

#get the reliability for the three primary DV items: alpha = .81

desire_scale <- select(pols1_abbrev2, desirepersonal, desireothers, desirebenefit)

psych::alpha(desire_scale)

pols1_abbrev2 <- pols1_abbrev2 %>%
  mutate(desire_scale =
           (desirepersonal + desireothers + desirebenefit)/3) 
  
# checking desire_scale means by condition
#mostly matches paper, condition ns look good

pols1_abbrev2 %>%
  group_by(polar_cond, broken_cond) %>%
  summarize(
    mean(desire_scale, na.rm = TRUE),
    sd(desire_scale, na.rm = TRUE),
    n())


#get the reliability for the 8 secondary support for third parties items: alpha = .88

support_scale <- select(pols1_abbrev2, thirdsupport1, thirdsupport2, thirdsupport3, 
                             thirdsupport4, thirdsupport5, thirdsupport6, thirdsupport7,
                             thirdsupport8)

psych::alpha(support_scale)

pols1_abbrev2 <- pols1_abbrev2 %>%
  mutate(thirdsupport_scale =
           (thirdsupport1 + thirdsupport2 + thirdsupport3 +
            thirdsupport4 + thirdsupport5 + thirdsupport6 + thirdsupport7 +
            thirdsupport8)/8)

# checking thirdsupport_scale means by condition
#mostly matches paper, condition ns look good

pols1_abbrev2 %>%
  group_by(polar_cond, broken_cond) %>%
  summarize(
    mean(thirdsupport_scale, na.rm = TRUE),
    sd(thirdsupport_scale, na.rm = TRUE),
    n())


#get the reliability for the 10 general fixes items: alpha = .87

fixes_scale <- select(pols1_abbrev2, fixsystem1, fixsystem2, fixsystem3, 
                        fixsystem4, fixsystem5, fixsystem6, fixsystem7,
                      fixsystem8, fixsystem9, fixsystem10)

psych::alpha(fixes_scale)

pols1_abbrev2 <- pols1_abbrev2 %>%
  mutate(genfixes_scale =
           (fixsystem1 + fixsystem2 + fixsystem3 + 
            fixsystem4 + fixsystem5 + fixsystem6 + fixsystem7 +
            fixsystem8 + fixsystem9 + fixsystem10)/10)


# checking genfixes_scale means by condition
#mostly matches paper, condition ns look good

pols1_abbrev2 %>%
  group_by(polar_cond, broken_cond) %>%
  summarize(
    mean(genfixes_scale, na.rm = TRUE),
    sd(genfixes_scale, na.rm = TRUE),
    n())

#4. ANALYSES OF THE TWO MANIPULATION CHECK SCALES (AS IN THE MANUSCRIPT) ----

# Polarization manipulation effectiveness: check for a positive linear effect of condition on
#     the polarization manipulation check scale: pol_check_scale
# Brokenness manipulation effectiveness: check for broken > not broken condition
#     on the brokenness manipulation check scale: broken_check_scale


# car package appears to be unavailable although listed on CRAN
# can also get levene's test in lawstat package
library(car) # levene.test has been deprecated and is now defunct
library(lawstat)


# test whether the homogeniety of variance across levels assumption holds
# this code does not work if there is missing data!
# solution: first filter for missing values on the vectors passed to levene.test

table(pols1_abbrev2$polar_cond)

polarcond_polarcheck <- pols1_abbrev2 %>%
  select(pol_check_scale, polar_cond) %>%
  filter(polar_cond != "NA" & pol_check_scale > -99)

with(polarcond_polarcheck, lawstat::levene.test(pol_check_scale, as.factor(polar_cond),location="mean"))

# Welch's test
wmodel_omnibus_polarcheck <- oneway.test(pol_check_scale ~ as.factor(polar_cond), data = polarcond_polarcheck)
wmodel_omnibus_polarcheck

# we know we have unequal variances across levels 
# thus, need to report games-howell adjustment

library(userfriendlyscience)
library(MBESS)
gh.manip_check_scale <- oneway(polarcond_polarcheck$polar_cond, y = polarcond_polarcheck$pol_check_scale, posthoc = 'games-howell')
gh.manip_check_scale


# effect sizes for the adjusted pairwise comparisons

library(lsr)

# Error: grouping factor must have exactly 2 levels
# Solution: create new dfs by creating relevant comparisons

pols1_abbrev2 <- pols1_abbrev2 %>%
  mutate(LvM = factor(case_when(
    polar_cond == "Low Polarization" ~ "Low Polarization",
    polar_cond == "Medium Polarization" ~ "Medium Polarization"))) %>%
  mutate(HvM = factor(case_when(
    polar_cond == "High Polarization" ~ "High Polarization",
    polar_cond == "Medium Polarization" ~ "Medium Polarization"))) %>%
  mutate(LvH = factor(case_when(
    polar_cond == "Low Polarization" ~ "Low Polarization",
    polar_cond == "High Polarization" ~ "High Polarization")))

# If the original t-test did not make a homogeneity of variance assumption, as per the Welch test, 
# the normalising term should mirror the Welch test (i.e., use method = "unequal"). 

cohensD(x = attdifferent ~ LvM, data = pols1_abbrev2, method = "unequal")
cohensD(x = attdifferent ~ HvM, data = pols1_abbrev2, method = "unequal")
cohensD(x = attdifferent ~ LvH, data = pols1_abbrev2, method = "unequal")


# check for homogeneity of variance across the broken and not-broken conditions

brokencond_brokencheck <- pols1_abbrev2 %>%
  select(broken_check_scale, broken_cond) %>%
  filter(broken_cond != "NA" & broken_check_scale > -99)

with(brokencond_brokencheck, lawstat::levene.test(broken_check_scale, as.factor(broken_cond),location="mean"))


# independent 2-group t-test for the effect of brokenness condition on the brokenness manipulation check scale
# the default assumes unequal variance and applies the Welsh df modification.
# we know there are equal variances across groups here, so turn off that default
# looks good!
t.test(pols1_abbrev2$broken_check_scale~pols1_abbrev2$broken_cond, var.equal = TRUE) # where y is numeric and x is a binary factor

# effect size
cohensD(x = broken_check_scale ~ broken_cond, data = pols1_abbrev2)



#5. ANALYSES OF THE THREE DVS IN ANOVA WITH CONTRASTS (AS IN THE MANUSCRIPT) ----

# Question 1: Do the level of polarization (low, medium, high) between two fictional candidates AND 
#             the level of brokenness in an electoral system  (broken v not broken) 
#             exert unique effects on desire for inclusion of a third party or idependent candidate?

# Question 2: Does the effect in Q1 extend to the two new DVs: 
#             general support for third parties and candidates and alternative fixes for electoral system brokenness

summary(pols1_abbrev2$polar_cond)
summary(pols1_abbrev2$broken_cond)


# 5.1 Create the terms for the 5 contrasts:

pols1_abbrev2 <- pols1_abbrev2 %>%
  mutate(brokenpol_cond = 
           ifelse((broken_cond == "Not Broken" & polar_cond == "Low Polarization"), 1,
           ifelse((broken_cond == "Not Broken" & polar_cond == "Medium Polarization"), 2,
            ifelse((broken_cond == "Not Broken" & polar_cond == "High Polarization"), 3,
            ifelse((broken_cond == "Broken" & polar_cond == "Low Polarization"), 4,
            ifelse((broken_cond == "Broken" & polar_cond == "Medium Polarization"), 5,
            ifelse((broken_cond == "Broken" & polar_cond == "High Polarization"), 6, NA)))))))

# condition ns look good!
table(pols1_abbrev2$brokenpol_cond)

class(pols1_abbrev2$brokenpol_cond)

#5.2 Create the n(conditions) - 1 = 5 contrast codes from brokenpol_cond:
#         Code 1: the main effect of brokenness condition: not-broken versus broken
#         Code 2: the first main effect of polarization condition: low AND high versus medium polarization
#         Code 3: the second main effect of polarization condition: low versus high (excluding medium polarization)
#         Code 4: the interaction between brokenness condition (not-broken versus broken) and low AND high versus medium polarization
#         Code 5: the interaction between brokenness condition (not-broken versus broken) and low versus high (excluding medium polarization)

library(dplyr)

glimpse(pols1_abbrev2)

#creating the contrast codes to implement in the ANOVA 
contrast_bvnb <- c(-1, -1, -1, 1, 1, 1) # Code 1: not broken v broken
contrast_lhvm <- c(1, -2, 1, 1, -2, 1) # Code 2: low and high versus medium
contrast_lvh <- c(1, 0, -1, 1, 0, -1) # Code 3: low v high, excluding medium
contrast_blhvm <- c(-1, 2, -1, 1, -2, 1) # Code 4: not broken v broken x low and high versus medium
contrast_blvh <- c(-1, 0, 1, 1, 0, -1) # Code 5: not broken v broken x low versus high, excluding medium

# combine the above 5 lines into a matrix
mat <- cbind(contrast_bvnb, contrast_lhvm, contrast_lvh,contrast_blhvm, contrast_blvh)

# condition var must be a factor to assign the contrasts to it
# adding a new factor var so that I have the condition var coded two ways: one as numeric and one as factor
pols1_abbrev2 <- pols1_abbrev2 %>%
  mutate(brokenpol_cond_fact = as.factor(brokenpol_cond))

class(pols1_abbrev2$brokenpol_cond_fact)

levels(pols1_abbrev2$brokenpol_cond_fact) <- c("Not Broken, Low Polarization", "Not Broken, Medium Polarization","Not Broken, High Polarization",
                                               "Broken, Low Polarization", "Broken, Medium Polarization","Broken, High Polarization")
summary(pols1_abbrev2$brokenpol_cond_fact)


# tell R that the matrix gives the contrasts you want
contrasts(pols1_abbrev2$brokenpol_cond_fact) <- mat

# 5.3 Model testing

#1. Desire DV

# check for homogeneity of variance across the 6 conditions for the primary DV: desire_scale

polarbroken_desire <- pols1_abbrev2 %>%
  dplyr::select(desire_scale, brokenpol_cond_fact) %>%
  filter(brokenpol_cond_fact != "NA" & desire_scale > -99)

with(polarbroken_desire, lawstat::levene.test(desire_scale, as.factor(brokenpol_cond_fact),location="mean"))

# desire DV omnibus test
amodel_omnibus_desire <- aov(desire_scale ~ brokenpol_cond_fact, data = pols1_abbrev2)
summary(amodel_omnibus_desire)

# Welch's test for non-homogeneity
wmodel_omnibus_polarcheck <- oneway.test(desire_scale ~ as.factor(brokenpol_cond_fact), data = polarbroken_desire)
wmodel_omnibus_polarcheck


# the below provides the estimates for the two contrasts and the omnibus model
# Make sure to use summary.aov here or 'split' might not work
# make sure the contrasts are listed in the same order they appear in the matrix!
# these look good!
summary.aov(amodel_omnibus_desire, split=list(brokenpol_cond_fact=list("Code 1: contrast_bvnb" = 1,"Code 2: contrast_lhvm" = 2, 
                                                              "Code 3: contrast_lvh" = 3, "Code 4: contrast_blhvm" = 4,
                                                              "Code 5: contrast_blvh" = 5))) 

# contrasts adjusted for non-homogeneity with White's
adjcont_desire <- aov(desire_scale ~ code1+code2+code3+code4+code5, data = pols1_abbrev2)
Anova(adjcont_desire, type = "III", white.adjust=TRUE)


# adjusted contrasts to account for heteroscedasticity 
# load libraries before analysis

library(SimComp)
library(multcomp)
library(mvtnorm)

#creating the contrast codes 
NBL <- c(-1, 1, -1, -1, 1) #not broken, low pol
NBM <- c(-1, -2, 0, 2, 0) #not broken, med pol
NBH <- c(-1, 1, 1, -1, -1) #not broken, high pol
BL <- c(1, 1, -1, 1, -1) #broken, low pol
BM <- c(1, -2, 0, -2, 0) #broken, medium pol
BH <- c(1, 1, 1, 1, 1) #broken, high pol

# combine the above 5 lines into a matrix
mat2 <- cbind(NBL, NBM, NBH, BL, BM, BH)

# filter out missing data prior to analysis

SimTestDiff(data = polarbroken_desire, grp="brokenpol_cond_fact", resp="desire_scale", ContrastMat = mat2,
            covar.equal = FALSE)

# filter out missing data prior to analysis

brokenpolar_fixes<- pols1_abbrev2 %>%
  select(genfixes_scale, brokenpol_cond_fact) %>%
  filter(brokenpol_cond_fact != "NA" & genfixes_scale > -99)

SimTestDiff(data = brokenpolar_fixes, grp="brokenpol_cond_fact", resp="genfixes_scale", ContrastMat = mat,
            covar.equal = FALSE)



#2. Support for third parties DV

# check for homogeneity of variance across the 6 conditions for the secondary DV: Support for third parties

polarbroken_support <- pols1_abbrev2 %>%
  select(thirdsupport_scale, brokenpol_cond_fact) %>%
  filter(brokenpol_cond_fact != "NA" & thirdsupport_scale > -99)

with(polarbroken_support, lawstat::levene.test(thirdsupport_scale, as.factor(brokenpol_cond_fact),location="mean"))

# secondary DV, support for third parties omnibus test
# equal variances across conditions so no need for GH adjustment
amodel_omnibus_thirdsupport <- aov(thirdsupport_scale ~ brokenpol_cond_fact, data = pols1_abbrev2)
summary(amodel_omnibus_thirdsupport)

summary.aov(amodel_omnibus_thirdsupport, split=list(brokenpol_cond_fact=list("Code 1: contrast_bvnb" = 1,"Code 2: contrast_lhvm" = 2, 
                                                                       "Code 3: contrast_lvh" = 3, "Code 4: contrast_blhvm" = 4,
                                                                       "Code 5: contrast_blvh" = 5)))
# rcontrast = sqrt(F or t^2)/sqrt(F or t^2 + df_within)

# rcontrast: brokenness 
sqrt(23.846)/sqrt(23.846 + 1178)

#rcontrast: high and low versus medium polarization
sqrt(2.98)/sqrt(2.98 + 1178)

#rcontrast: high versus low excluding medium polarization
sqrt(.013)/sqrt(.013 + 1178)
                                                                       
                      
                                                                    
                                                                                                                                               
#3. Support for alternative fixes DV

# check for homogeneity of variance across the 6 conditions for the secondary DV: Support for alternative fixes

polarbroken_fixes <- pols1_abbrev2 %>%
  select(genfixes_scale, brokenpol_cond_fact) %>%
  filter(brokenpol_cond_fact != "NA" & genfixes_scale > -99)

with(polarbroken_fixes, lawstat::levene.test(genfixes_scale, as.factor(brokenpol_cond_fact),location="mean"))

# Welch's test; unequal variances
wmodel_omnibus_polarcheck <- oneway.test(genfixes_scale ~ as.factor(brokenpol_cond_fact), data = polarbroken_fixes)
wmodel_omnibus_polarcheck

# secondary DV, support for alternative fixes omnibus test
amodel_omnibus_genfixes <- aov(genfixes_scale ~ brokenpol_cond_fact, data = pols1_abbrev2)

summary.aov(amodel_omnibus_genfixes, split=list(brokenpol_cond_fact=list("Code 1: contrast_bvnb" = 1,"Code 2: contrast_lhvm" = 2, 
                                                                             "Code 3: contrast_lvh" = 3, "Code 4: contrast_blhvm" = 4,
                                                                             "Code 5: contrast_blvh" = 5))) 
# we know we have unequal variances across levels 
# thus, need to report games-howell adjustment

gh.fixes_scale <- oneway(polarbroken_fixes$brokenpol_cond_fact, y = polarbroken_fixes$genfixes_scale, posthoc = 'games-howell')
gh.fixes_scale



#6. ANALYSES IN REGRESSION ----

#predict the three DVs from the 5 contrast codes

pols1_abbrev2 <- pols1_abbrev2 %>%
  mutate(code1 = dplyr::recode(
    brokenpol_cond, '1'=-1, '2'=-1, '3'=-1, '4'=1, '5'=1, '6'=1)) %>%
  mutate(code2 = dplyr::recode(
    brokenpol_cond, '1'=1, '2'=-2, '3'=1, '4'=1, '5'=-2, '6'=1)) %>%
  mutate(code3 = dplyr::recode(
    brokenpol_cond, '1'=1, '2'=0, '3'=-1, '4'=1, '5'=0, '6'=-1)) %>%
  mutate(code4 = dplyr::recode(
    brokenpol_cond, '1'=-1, '2'=2, '3'=-1, '4'=1, '5'=-2, '6'=1)) %>%
  mutate(code5 = dplyr::recode(
    brokenpol_cond, '1'=-1, '2'=0, '3'=1, '4'=1, '5'=0, '6'=-1))

glimpse(pols1_abbrev2)


#1. model predicting the polarization manipulation check_scale 
rmodel_linear_polcheck <- lm(pol_check_scale ~ as.numeric(polar_cond), data = pols1_abbrev2)
summary(rmodel_linear_polcheck)

#2. model predicting the brokenness manipulation check_scale
rmodel_linear_brokencheck <- lm(broken_check_scale ~ as.numeric(broken_cond), data = pols1_abbrev2)
summary(rmodel_linear_brokencheck)

#3. model with the 5 contrast codes predicting desire
rmodel_desire <- lm(desire_scale ~ code1+code2+code3+code4+code5, data = pols1_abbrev2)
summary(rmodel_desire)

#4. model with the 5 contrast codes predicting general support for third parties
rmodel_support <- lm(thirdsupport_scale ~ code1+code2+code3+code4+code5, data = pols1_abbrev2)
summary(rmodel_support)

#5. model with the 5 contrast codes predicting alternative fixes
rmodel_fixes <- lm(genfixes_scale ~ code1+code2+code3+code4+code5, data = pols1_abbrev2)
summary(rmodel_fixes)







