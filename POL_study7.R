# set the working directory
getwd()

setwd("/gabriellechristinemartin/Desktop/R_Pol_data_scripts/POL Expt 7")

# clear the environment
rm(list=ls())

library(tidyverse)
library(haven)

#1. IMPORTING, VIEWING, AND SELECTING THE DATA ----

#create data.frame called pols1 from the spss (.sav) file
pols1 <- data.frame(read_sav("/Users/gabriellechristinemartin/Desktop/R_Pol_data_scripts/POL Expt 7/POL_Expt7_original.sav"))

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
#recodes the string condition var (X._task_id) to factor
#recodes the string indicator (X._session_status) of whether the participant got to the end of the study: complete
#recodes string sex to factor gender
# make sure to include the mediator items: systembroken, healthysystem, electpointless, electwellfunct 

pols1_abbrev <- pols1 %>%
  select(X._task_id, desirepersonal, desireothers, desirebenefit, attdifferent, attsimilar, attoverlap, 
         brokensystem, systempoorfunct, systemflawed, systemfailing,
         trust1, trust2, trust3, trust4, trust5, exptdemand1, exptdemand2, exptdemand3, exptdemand4, exptdemand5,
         X._session_status, polorient, partyid, age, sex, education, ethnicityomb, raceomb) %>%
      mutate(partyid = as.factor(partyid)) %>%
      mutate(education = as.factor(education)) %>%
      mutate(ethnicityomb = as.factor(ethnicityomb)) %>%
      mutate(raceomb = as.factor(raceomb)) %>%
  mutate(polar_cond_m1 = as.factor(case_when( 
        X._task_id == "manipinstmed1pol" ~ 2,
        X._task_id ==  "manipinsthighpol" ~ 3,
        X._task_id ==  "manipinstlowpol" ~ 1))) %>%
  mutate(polar_cond_m2 = as.factor(case_when( 
    X._task_id == "manipinstmed2pol" ~ 2,
    X._task_id ==  "manipinsthighpol" ~ 3,
    X._task_id ==  "manipinstlowpol" ~ 1))) %>%
      mutate(complete = as.factor(case_when(
        X._session_status == "C" ~ 1,
        X._session_status == "null" ~ 0))) %>%
      mutate(gender = as.factor(case_when(
        sex == "m" ~ 1,
        sex == "f" ~ 2)))

glimpse(pols1_abbrev)

#checking to make sure the ns in each factor level are the same after re-coding into new var

table(pols1_abbrev$X._task_id)
table(pols1_abbrev$polar_cond_m1)
class(pols1_abbrev$polar_cond_m1)
table(pols1_abbrev$polar_cond_m2)
class(pols1_abbrev$polar_cond_m2)

table(pols1_abbrev$X._session_status)
table(pols1_abbrev$complete) #403 completed the session; 319 did not
class(pols1_abbrev$complete)

table(pols1_abbrev$sex)
table(pols1_abbrev$gender)
class(pols1_abbrev$gender)


# set factor levels 

levels(pols1_abbrev$complete) <- c("P exited study early", "P reached study end")
summary(pols1_abbrev$complete)

levels(pols1_abbrev$polar_cond_m1) <- c("Low Polarization", "Medium1 Polarization","High Polarization")
summary(pols1_abbrev$polar_cond_m1)

levels(pols1_abbrev$polar_cond_m2) <- c("Low Polarization", "Medium2 Polarization","High Polarization")
summary(pols1_abbrev$polar_cond_m2)

levels(pols1_abbrev$partyid) <- c("Democrat", "Republican", "Independent", "Green", "Libertarian", "Other")
summary(pols1_abbrev$partyid)

levels(pols1_abbrev$gender) <- c("Male", "Female")
summary(pols1_abbrev$gender)

levels(pols1_abbrev$ethnicityomb) <- c("hispanic or latino", "not hispanic or latino", "unknown")
summary(pols1_abbrev$ethnicityomb)

levels(pols1_abbrev$raceomb) <- c("american indian/alaskan native", "east asian", "south asian", "native hawiian or other pacific islander",
                                  "black or african american", "white", "more than one race- Black/White", "more than one race- other",
                                  "other or unknown")
summary(pols1_abbrev$raceomb)

levels(pols1_abbrev$education) <- c("elementary school", "junior high", "some high school", "high school graduate", "some college",
                                    "associates degree", "bachelors degree", "some graduate school", "masters degree",
                                    "JD", "MD", "PhD", "other advanced degree", "MBA")
summary(pols1_abbrev$education)


# starting n = 722  
#drops the now unnecessary string vars
# what we should report is complete_DV & age > 18 (ignoring whether Ps got to the end)
#n, complete_DV = 430 (dropped n = 292)
#n, complete = 403
#droped Ps under 18 (n = 0): results in 430 responses

pols1_abbrev2 <- pols1_abbrev %>%
  select(desirepersonal, desireothers, desirebenefit, attdifferent, attsimilar, attoverlap, 
         brokensystem, systempoorfunct, systemflawed, systemfailing,
         trust1, trust2, trust3, trust4, trust5, exptdemand1, exptdemand2, exptdemand3, exptdemand4, exptdemand5,
         polorient, partyid, age, gender, education, ethnicityomb, raceomb, polar_cond_m1, polar_cond_m2, complete) %>%
  mutate(complete_DV = (as.factor(ifelse(
    desirepersonal > -99 & desirebenefit > -99 & desireothers > -99, 1, NA)))) %>%
  filter(complete_DV == 1) %>%
  filter(age >= 18)

glimpse(pols1_abbrev2)

summary(pols1_abbrev2$age)
sd(pols1_abbrev2$age, na.rm = TRUE)
summary(pols1_abbrev2$gender)
summary(pols1_abbrev2$partyid)
summary(pols1_abbrev2$raceomb)
table(pols1_abbrev2$polar_cond)

pols1_abbrev3 <- pols1_abbrev2 %>%
  mutate(polorient17 = (ifelse(
    polorient == 8, NA, polorient))) 

table(pols1_abbrev2$polorient)
table(pols1_abbrev3$polorient17)
mean(pols1_abbrev3$polorient17)
sd(pols1_abbrev3$polorient17, na.rm = TRUE)

#2. EXAMINING ITEM DISTRIBUTIONS AND SUMMARY STATS ----

# ugly but quick and dirty histograms for each item in primary analyses
# most items look mostly normally-distributed; 
# counts a little high on the high end for desirebenefit and desirepersonal

# desire DV items
hist(pols1_abbrev2$desirepersonal)
hist(pols1_abbrev2$desirebenefit)
hist(pols1_abbrev2$desireothers)

#note the differences in SD across items
# NAs look roughly the same across items
summary(pols1_abbrev2$desirepersonal)
sd(pols1_abbrev2$desirepersonal, na.rm = TRUE)

summary(pols1_abbrev2$desirebenefit)
sd(pols1_abbrev2$desirebenefit, na.rm = TRUE)

summary(pols1_abbrev2$desireothers)
sd(pols1_abbrev2$desireothers, na.rm = TRUE)

# polarization manip check item
# counts a little low on the high end of the scale for attdifferent

hist(pols1_abbrev2$attdifferent)
hist(pols1_abbrev2$attsimilar)
hist(pols1_abbrev2$attoverlap)

summary(pols1_abbrev2$attdifferent)
sd(pols1_abbrev2$attdifferent, na.rm = TRUE)

summary(pols1_abbrev2$attsimilar)
sd(pols1_abbrev2$attsimilar, na.rm = TRUE)

summary(pols1_abbrev2$attoverlap)
sd(pols1_abbrev2$attoverlap, na.rm = TRUE)


# mediator items: system brokenness
# distributions look pretty awful for systembroken and electpointless

hist(pols1_abbrev2$brokensystem)
hist(pols1_abbrev2$systemflawed)
hist(pols1_abbrev2$systemfailing)
hist(pols1_abbrev2$systempoorfunct)

summary(pols1_abbrev2$brokensystem)
sd(pols1_abbrev2$brokensystem, na.rm = TRUE)

summary(pols1_abbrev2$systemflawed)
sd(pols1_abbrev2$systemflawed, na.rm = TRUE)

summary(pols1_abbrev2$systemfailing)
sd(pols1_abbrev2$systemfailing, na.rm = TRUE)

summary(pols1_abbrev2$systempoorfunct)
sd(pols1_abbrev2$systempoorfunct, na.rm = TRUE)

#3. CHECKING FOR SCALE RELIABILITY AND CREATING SCALES ----

#get the reliability for the three manipulation check items: alpha = .83

pols1_abbrev2 <- pols1_abbrev2 %>%
  mutate(attsimilar_r = 
           6 - attsimilar) %>%
  mutate(attoverlap_r = 
           6 - attoverlap) %>%
  mutate(polar_check =
            (attsimilar_r + attoverlap_r + attdifferent)/3)

table(pols1_abbrev2$attsimilar)
table(pols1_abbrev2$attsimilar_r)

table(pols1_abbrev2$attoverlap)
table(pols1_abbrev2$attoverlap_r)

polar_scale <- select(pols1_abbrev2, attdifferent, attsimilar_r, attoverlap_r)

psych::alpha(polar_scale)

# checking polarization check scale means by condition

pols1_abbrev2 %>%
  group_by(polar_cond_m1) %>%
  summarize(
    mean(polar_check, na.rm = TRUE),
    sd(polar_check, na.rm = TRUE),
    n())

pols1_abbrev2 %>%
  group_by(polar_cond_m2) %>%
  summarize(
    mean(polar_check, na.rm = TRUE),
    sd(polar_check, na.rm = TRUE),
    n())


#get the reliability for the three primary DV items: alpha = .84

desire_scale <- select(pols1_abbrev2, desirepersonal, desireothers, desirebenefit)

library(psych)

psych::alpha(desire_scale)

# create the desire scale by averaging the three items
pols1_abbrev2 <- pols1_abbrev2 %>%
  mutate(desire_scale =
           (desirepersonal + desireothers + desirebenefit)/3) 
  
# checking desire_scale means by condition

pols1_abbrev2 %>%
  group_by(polar_cond_m1) %>%
  summarize(
    mean(desire_scale, na.rm = TRUE),
    sd(desire_scale, na.rm = TRUE),
    n())

pols1_abbrev2 %>%
  group_by(polar_cond_m2) %>%
  summarize(
    mean(desire_scale, na.rm = TRUE),
    sd(desire_scale, na.rm = TRUE),
    n())


#get the reliability for the nine mediator items: alpha = .72

# four brokenness items and 5 distrust items

pols1_abbrev2 <- pols1_abbrev2 %>%
  mutate(brokenness_scale =
           ((brokensystem + systempoorfunct + systemflawed + systemfailing +
               trust1 + trust2 + trust3 + trust4 + trust5)/9)) 

summary(pols1_abbrev2$brokenness_scale)
sd(pols1_abbrev2$brokenness_scale, na.rm = TRUE)

glimpse(pols1_abbrev2)

brokensc <- select(pols1_abbrev2, brokensystem, systempoorfunct, systemflawed, systemfailing,
                     trust1, trust2, trust3, trust4, trust5)

psych::alpha(brokensc)

# checking brokenness_scale means by condition

pols1_abbrev2 %>%
  group_by(polar_cond_m1) %>%
  summarize(
    mean(brokenness_scale, na.rm = TRUE),
    sd(brokenness_scale, na.rm = TRUE),
    n())

pols1_abbrev2 %>%
  group_by(polar_cond_m2) %>%
  summarize(
    mean(brokenness_scale, na.rm = TRUE),
    sd(brokenness_scale, na.rm = TRUE),
    n())


# Reliability for the 5 experimenter demand items: alpha = .87

demand_scale <- select(pols1_abbrev2, exptdemand1, exptdemand2, exptdemand3, 
                       exptdemand4, exptdemand5)

psych::alpha(demand_scale)

# Factor analysis of experimenter demand items

# step 1: create the matrix with only the items in the scale
demand_mat <- cor(demand_scale, use = "na.or.complete")
demand_mat

# step 2: factor analysis of the matrix
# item 3 does not load

fa_demand <- fa(r = demand_mat, nfactors = 1, rotate = "oblimin", fm = "pa")
fa_demand

fa_demand2 <- fa(r = demand_mat, nfactors = 2, rotate = "oblimin", fm = "pa")
fa_demand2

#3.8c. Means, sd, and ns for each condition

pols1_abbrev2 <- pols1_abbrev2 %>%
  mutate(exptdemand_scale =
           (exptdemand1 + exptdemand2 +
              exptdemand4 + exptdemand5)/4)


pols1_abbrev2 %>%
  group_by(polar_cond_m1) %>%
  summarize(
    mean(exptdemand_scale, na.rm = TRUE),
    sd(exptdemand_scale, na.rm = TRUE),
    n())

pols1_abbrev2 %>%
  group_by(polar_cond_m2) %>%
  summarize(
    mean(exptdemand_scale, na.rm = TRUE),
    sd(exptdemand_scale, na.rm = TRUE),
    n())


pols1_abbrev2 %>%
  group_by(polar_cond_m1) %>%
  summarize(
    mean(exptdemand3, na.rm = TRUE),
    sd(exptdemand3, na.rm = TRUE),
    n())

pols1_abbrev2 %>%
  group_by(polar_cond_m2) %>%
  summarize(
    mean(exptdemand3, na.rm = TRUE),
    sd(exptdemand3, na.rm = TRUE),
    n())


#4. ANALYSES IN ANOVA: POLARIZATION MANIPULATION CHECK ----

# Polarization manipulation effectiveness: check for a positive linear effect of condition (from low to high)
#     on the manipulation check item: attdifferent

#checking the condition variable class and levels before analysis
class(pols1_abbrev2$polar_cond_m1)
table(pols1_abbrev2$polar_cond_m1)

class(pols1_abbrev2$polar_cond_m2)
table(pols1_abbrev2$polar_cond_m2)

#get summary info for the manipulation check scale, primary DV, and brokenness by condition
describeBy(pols1_abbrev2$polar_check, pols1_abbrev2$polar_cond_m1, mat=TRUE)
describeBy(pols1_abbrev2$desire_scale, pols1_abbrev2$polar_cond_m1, mat=TRUE)
describeBy(pols1_abbrev2$brokenness_scale, pols1_abbrev2$polar_cond_m1, mat=TRUE)

describeBy(pols1_abbrev2$polar_check, pols1_abbrev2$polar_cond_m2, mat=TRUE)
describeBy(pols1_abbrev2$desire_scale, pols1_abbrev2$polar_cond_m2, mat=TRUE)
describeBy(pols1_abbrev2$brokenness_scale, pols1_abbrev2$polar_cond_m2, mat=TRUE)


#models predicting polarization check

# car package appears to be unavailable although listed on CRAN
# can also get levene's test in lawstat package
library(lawstat)

# test whether the homogeniety of variance across levels assumption holds
# this code does not work if there is missing data!
# solution: first filter for missing values on the vectors passed to levene.test

table(pols1_abbrev2$polar_cond)

polarcond_polarcheck_m1 <- pols1_abbrev2 %>%
  select(polar_check, polar_cond_m1) %>%
  filter(polar_cond_m1 != "NA" & polar_check > -99)

polarcond_polarcheck_m2 <- pols1_abbrev2 %>%
  select(polar_check, polar_cond_m2) %>%
  filter(polar_cond_m2 != "NA" & polar_check > -99)

# polarcond_polarcheck_m1, n = 350
with(polarcond_polarcheck_m1, lawstat::levene.test(polar_check, as.factor(polar_cond_m1),location="mean"))

# polarcond_polarcheck_m2, n = 338
with(polarcond_polarcheck_m2, lawstat::levene.test(polar_check, as.factor(polar_cond_m2),location="mean"))


# we know we have unequal variances across levels 
# thus, need to report Welch's adjusted ANOVA and games-howell adjustment

# Welch's test
wmodel_omnibus_polarcheck_m1 <- oneway.test(polar_check ~ as.factor(polar_cond_m1), data = polarcond_polarcheck_m1)
wmodel_omnibus_polarcheck_m1

wmodel_omnibus_polarcheck_m2 <- oneway.test(polar_check ~ as.factor(polar_cond_m2), data = polarcond_polarcheck_m2)
wmodel_omnibus_polarcheck_m2

# adjusted pairwise comparisons
library(userfriendlyscience)
library(MBESS)

gh.manip_check_scale_m1 <- oneway(polarcond_polarcheck_m1$polar_cond_m1, y = polarcond_polarcheck_m1$polar_check, posthoc = 'games-howell')
gh.manip_check_scale_m1

gh.manip_check_scale_m2 <- oneway(polarcond_polarcheck_m2$polar_cond_m2, y = polarcond_polarcheck_m2$polar_check, posthoc = 'games-howell')
gh.manip_check_scale_m2


# effect sizes for the adjusted pairwise comparisons

library(lsr)

# Error: grouping factor must have exactly 2 levels
# Solution: create new dfs by filtering for the relevant conditions for comparison

pols1_abbrev2 <- pols1_abbrev2 %>%
  mutate(LvM1 = factor(case_when(
    polar_cond_m1 == "Low Polarization" ~ "Low Polarization",
    polar_cond_m1 == "Medium1 Polarization" ~ "Medium1 Polarization"))) %>%
  mutate(LvM2 = factor(case_when(
    polar_cond_m2 == "Low Polarization" ~ "Low Polarization",
    polar_cond_m2 == "Medium2 Polarization" ~ "Medium2 Polarization"))) %>%
  mutate(HvM1 = factor(case_when(
    polar_cond_m1 == "High Polarization" ~ "High Polarization",
    polar_cond_m1 == "Medium1 Polarization" ~ "Medium1 Polarization"))) %>%
  mutate(HvM2 = factor(case_when(
    polar_cond_m2 == "High Polarization" ~ "High Polarization",
    polar_cond_m2 == "Medium2 Polarization" ~ "Medium2 Polarization"))) %>%
  mutate(LvH = factor(case_when(
    polar_cond_m1 == "Low Polarization" ~ "Low Polarization",
    polar_cond_m1 == "High Polarization" ~ "High Polarization")))

# If the original t-test did not make a homogeneity of variance assumption, as per the Welch test, 
# the normalising term should mirror the Welch test (i.e., use method = "unequal"). 

cohensD(x = polar_check ~ LvM1, data = pols1_abbrev2, method = "unequal")
cohensD(x = polar_check ~ LvM2, data = pols1_abbrev2, method = "unequal")

cohensD(x = polar_check ~ HvM1, data = pols1_abbrev2, method = "unequal")
cohensD(x = polar_check ~ HvM2, data = pols1_abbrev2, method = "unequal")

cohensD(x = polar_check ~ LvH, data = pols1_abbrev2, method = "unequal")


#5. ANALYSES IN ANOVA: DV = DESIRE SCALE (AS IN THE MANUSCRIPT) ----

# Question 1: Does the level of polarization between two fictional candidates affect 
#             desire for inclusion of a third party or idependent candidate?
# Q1a. Do people in the low AND high polarization conditions report greater desire compared to
#     people in the medium condition?
# Q1b. Do people in the low polarization and high polarization conditions (excluding medium condition)  
#     report equal levels of desire?


# test whether the homogeniety of variance across levels assumption holds
# this code does not work if there is missing data!
# solution: first filter for missing values on the vectors passed to levene.test

polarcond_desire_m1 <- pols1_abbrev2 %>%
  select(desire_scale, polar_cond_m1) %>%
  filter(polar_cond_m1 != "NA" & desire_scale > -99)

polarcond_desire_m2 <- pols1_abbrev2 %>%
  select(desire_scale, polar_cond_m2) %>%
  filter(polar_cond_m2 != "NA" & desire_scale > -99)

# polarcond_desire_m1, n = 362
with(polarcond_desire_m1, lawstat::levene.test(desire_scale, as.factor(polar_cond_m1),location="mean"))

# polarcond_desire_m1, n = 351
# m2 condition has heterogeneous variances
with(polarcond_desire_m2, lawstat::levene.test(desire_scale, as.factor(polar_cond_m2),location="mean"))


# run an omnibus ANOVA for the primary DV = desire scale by condition

amodel_omnibus_desire_m1 <- aov(desire_scale ~ polar_cond_m1, data = pols1_abbrev2)
summary(amodel_omnibus_desire_m1)

#creating the contrast codes to implement in the ANOVA 
contrast_hvl <- c(-1, 0, 1) #high v low, excluding medium
contrast_hlvm <- c(1, -2, 1) #high and low versus medium

# combine the above 2 lines into a matrix
mat <- cbind(contrast_hvl, contrast_hlvm)

# tell R that the matrix gives the contrasts you want
contrasts(pols1_abbrev2$polar_cond_m1) <- mat
contrasts(pols1_abbrev2$polar_cond_m2) <- mat



# the below provides the estimates for the two contrasts and the omnibus model
# Make sure to use summary.aov here or 'split' might not work
# make sure the contrasts are listed in the same order they appear in the matrix!
# these look pretty good!
summary.aov(amodel_omnibus_desire_m1, split=list(polar_cond_m1=list("High vs. Low"=2, 
                                           "High & Low vs Medium" = 1))) 


# effect sizes for the contrasts: contrast correlation (Rosnow, Rosenthal, & Rubin, 2000)

# the below provides the partial correlation between scores on the outcome variable
# and the lambdas associated with the groups, after eliminating all between-group non-contrast variation

# rcontrast = sqrt(F or t^2)/sqrt(F or t^2 + df_within)

# High and low versus medium contrast

sqrt(.018)/sqrt(.018 + 359)

# High versus low contrast

sqrt(2.738)/sqrt(2.738 + 359)


# dealing with the heterogeneity of variance for mediumm2 condition

# Welch's test
wmodel_omnibus_desire_m2 <- oneway.test(desire_scale ~ as.factor(polar_cond_m2), data = polarcond_desire_m2)
wmodel_omnibus_desire_m2

# analyses for adjusted contrasts
# export to csv to work on separately: for some reason the code does not work in this script but work separately

pols1_abbrev2 %>%
  dplyr::select(polar_cond_m1, polar_cond_m2, desire_scale) %>%
  write.csv("/Users/gabriellechristinemartin/Desktop/R_Pol_data_scripts/Supplement_S7toS10_adjustedcontrasts/S7_adjustedcontrasts/S7_simcomp.csv", row.names = TRUE)

# adjusted contrasts to account for heteroscedasticity 
# load libraries before analysis

library(SimComp)
library(multcomp)
library(mvtnorm)

#creating the contrast codes 
Low <- c(-1, 1) #high v low, excluding medium
Med2 <- c(0, -2) #high and low versus medium
High <- c(1, 1) #high v low, excluding medium

# combine the above 5 lines into a matrix
mat2 <- cbind(Low, Med2, High)

# filter out missing data prior to analysis

polarcond_desire_m2 <- pols1_abbrev2 %>%
  select(desire_scale, polar_cond_m2) %>%
  filter(polar_cond_m2 != "NA" & desire_scale > -99)

SimTestDiff(data = polarcond_desire_m2, grp="polar_cond_m2", resp="desire_scale", ContrastMat = mat2,
            covar.equal = FALSE)


# assessing the linear effect (rather than contrats) with Tukey pairwise comparisons for Medium1 and GH for Medium2

tk.desire_scale_m1 <- oneway(polarcond_desire_m1$polar_cond_m1, y = polarcond_desire_m1$desire_scale, posthoc = 'tukey')
tk.desire_scale_m1

gh.desire_scale_m2 <- oneway(polarcond_desire_m2$polar_cond_m2, y = polarcond_desire_m2$desire_scale, posthoc = 'games-howell')
gh.desire_scale_m2


# 7. ANALYSES IN ANOVA: MEDIATOR = BROKEN SCALE (AS IN THE MANUSCRIPT) ----

# Question 2: Does the level of polarization between two fictional candidates affect 
#             perceived brokenness in the electoral system?
# Q2a. Do people in the low AND high polarization conditions report greater perceived brokenness compared to
#     people in the medium condition?
# Q2b. Do people in the low polarization and high polarization conditions (excluding medium condition)  
#     report equal levels of perceived brokenness?

# test whether the homogeniety of variance across levels assumption holds
# this code does not work if there is missing data!
# solution: first filter for missing values on the vectors passed to levene.test

polarcond_broken_m1 <- pols1_abbrev2 %>%
  select(brokenness_scale, polar_cond_m1) %>%
  filter(polar_cond_m1 != "NA" & brokenness_scale > -99)

polarcond_broken_m2 <- pols1_abbrev2 %>%
  select(brokenness_scale, polar_cond_m2) %>%
  filter(polar_cond_m2 != "NA" & brokenness_scale > -99)

# polarcond_broken, n = 348
with(polarcond_broken_m1, lawstat::levene.test(brokenness_scale, as.factor(polar_cond_m1),location="mean"))

# polarcond_broken, n = 340
with(polarcond_broken_m2, lawstat::levene.test(brokenness_scale, as.factor(polar_cond_m2),location="mean"))

# run an omnibus ANOVA for the mediator = brokenness by condition

amodel_omnibus_brokenness_m1 <- aov(brokenness_scale ~ polar_cond_m1, data = pols1_abbrev2)
summary(amodel_omnibus_brokenness_m1)

amodel_omnibus_brokenness_m2 <- aov(brokenness_scale ~ polar_cond_m2, data = pols1_abbrev2)
summary(amodel_omnibus_brokenness_m2)

# using the contrasts defined above

# the below provides the estimates for the two contrasts and the omnibus model
# Make sure to use summary.aov here or 'split' might not work
# make sure the contrasts are listed in the same order they appear in the matrix!
# Fs are a little off but otherwise these look pretty good!
summary.aov(amodel_omnibus_brokenness_m1, split=list(polar_cond_m1=list("High vs. Low"=1, 
                                                              "High & Low vs Medium" = 2))) 

summary.aov(amodel_omnibus_brokenness_m2, split=list(polar_cond_m2=list("High vs. Low"=1, 
                                                                        "High & Low vs Medium" = 2))) 

# effect sizes for the contrasts: contrast correlation (Rosnow, Rosenthal, & Rubin, 2000)

# the below provides the partial correlation between scores on the outcome variable
# and the lambdas associated with the groups, after eliminating all between-group non-contrast variation

# rcontrast = sqrt(F or t^2)/sqrt(F or t^2 + df_within)

# High and low versus medium1 contrast

sqrt(.006)/sqrt(.006 + 345)

# High and low versus medium2 contrast

sqrt(1.571)/sqrt(1.571 + 337)

# High versus low contrast

sqrt(1.489)/sqrt(1.489 + 345)

# assessing the linear effect with Tukey pairwise comparisons for Medium1 and Medium2

tk.brokenness_scale_m1 <- oneway(polarcond_broken_m1$polar_cond_m1, y = polarcond_broken_m1$brokenness_scale, posthoc = 'tukey')
tk.brokenness_scale_m1

tk.brokenness_scale_m2 <- oneway(polarcond_broken_m2$polar_cond_m2, y = polarcond_broken_m2$brokenness_scale, posthoc = 'tukey')
tk.brokenness_scale_m2



# 9. MEDIATION ANALYSIS OF THE INDIRECT EFFECT OF POL COND ON DESIRE THROUGH BROKENNESS ----


# HAVENT RUN ANY OF THESE THINGS YET


# Question 3: Does polarization condition affect desire through brokenness? 
# i.e., Is there an indrect effect of polarization on desire through perceived brokenness, 
# such that in the low and high polarization conditions people perceive greater system brokenness
# and thus report greater desire for inclusion of a third party or independent candidate?

# note: mediation models with multicategorical predictors have to be broken into dummy comparisons
# that compare to a reference group (here, the medium condition)
#   one model comparing low and medium conditions
#   one model comparing high and medium conditions
# Multicategorical predictor models with three levels of the IV produce:
#   two sets of coefficients each for the a-path, total effect (C-path), direct effect (C’-path), and indirect effect, 
#   and a single coefficient for the b-path.

# first, create the two contrasts comparing to low and, separately, high to medium condition

pols1_abbrev2 <- pols1_abbrev2 %>% 
  mutate(contrast_lowvmed1 = as.numeric(recode(
    polar_cond_m1, 'Low Polarization' = 1, 
    'Medium1 Polarization' = 0))) %>%
  mutate(contrast_highvmed1 = as.numeric(recode(
    polar_cond_m1, 'Medium1 Polarization' = 0, 
    'High Polarization' = 1)))

pols1_abbrev2 <- pols1_abbrev2 %>% 
  mutate(contrast_lowvmed2 = as.numeric(recode(
    polar_cond_m2, 'Low Polarization' = 1, 
    'Medium2 Polarization' = 0))) %>%
  mutate(contrast_highvmed2 = as.numeric(recode(
    polar_cond_m2, 'Medium2 Polarization' = 0, 
    'High Polarization' = 1)))


table(pols1_abbrev2$polar_cond_m1)
table(pols1_abbrev2$polar_cond_m2)

table(pols1_abbrev2$contrast_lowvmed1)
table(pols1_abbrev2$contrast_lowvmed2)

table(pols1_abbrev2$contrast_highvmed1)
table(pols1_abbrev2$contrast_highvmed2)


#creating the contrast codes to implement in the mediation model
contrast_lvm <- c(1, -1, 0) #low v medium, excluding high
contrast_hvm <- c(0, -1, 1) #high v medium, excluding low 

# combine the above 2 lines into a matrix
mat <- cbind(contrast_lvm, contrast_hvm)

# tell R that the matrix gives the contrasts you want
contrasts(pols1_abbrev2$polar_cond_m1) <- mat
contrasts(pols1_abbrev2$polar_cond_m2) <- mat


# the below provides the estimates for the two contrasts and the omnibus model
# Make sure to use summary.aov here or 'split' might not work
# make sure the contrasts are listed in the same order they appear in the matrix!
# these look pretty good!
summary.aov(amodel_omnibus_desire, split=list(polar_cond_m1=list("Low vs. Medium"=1, 
                                                              "High vs Medium" = 2))) 

summary.aov(amodel_omnibus_desire, split=list(polar_cond_m2=list("Low vs. Medium"=1, 
                                                              "High vs Medium" = 2))) 


library(knitr)
library(lavaan)

# specifying the model paths

# medium 1 model

mod_lvm1 <- "# a path
         brokenness_scale ~ a * contrast_lowvmed1

         # b path
         desire_scale ~ b * brokenness_scale

         # c prime path 
         desire_scale ~ cp * contrast_lowvmed1

         # indirect and total effects
         ab := a * b
         total := cp + ab"

mod_hvm1 <- "# a path
         brokenness_scale ~ a * contrast_highvmed1

         # b path
         desire_scale ~ b * brokenness_scale

         # c prime path 
         desire_scale ~ cp * contrast_highvmed1

         # indirect and total effects
         ab := a * b
         total := cp + ab"

# medium 2 model

mod_lvm2 <- "# a path
         brokenness_scale ~ a * contrast_lowvmed2

         # b path
         desire_scale ~ b * brokenness_scale

         # c prime path 
         desire_scale ~ cp * contrast_lowvmed2

         # indirect and total effects
         ab := a * b
         total := cp + ab"

mod_hvm2 <- "# a path
         brokenness_scale ~ a * contrast_highvmed2

         # b path
         desire_scale ~ b * brokenness_scale

         # c prime path 
         desire_scale ~ cp * contrast_highvmed2

         # indirect and total effects
         ab := a * b
         total := cp + ab"


# set random seed so results can be reproduced
set.seed(1234)

# You must specify bootstrapping in the sem() function
# used 5000 bootstrapped samples in the paper

# fit models
sem_lvm1 <- sem(mod_lvm1, data = pols1_abbrev2, se = "bootstrap", bootstrap = 5000)

sem_hvm1 <- sem(mod_hvm1, data = pols1_abbrev2, se = "bootstrap", bootstrap = 5000)


sem_lvm2 <- sem(mod_lvm2, data = pols1_abbrev2, se = "bootstrap", bootstrap = 5000)

sem_hvm2 <- sem(mod_hvm2, data = pols1_abbrev2, se = "bootstrap", bootstrap = 5000)


# summarize models
# note: total effect = c path; cp = c prime (c'); ab = indirect effect

summary(sem_lvm1, standardized = TRUE)

summary(sem_hvm1, standardized = TRUE)

summary(sem_lvm2, standardized = TRUE)

summary(sem_hvm2, standardized = TRUE)


# print all model parameters
parameterestimates(sem_lvm1, boot.ci.type = "bca.simple", standardized = TRUE) %>% 
  kable()

parameterestimates(sem_hvm1, boot.ci.type = "bca.simple", standardized = TRUE) %>% 
  kable()


parameterestimates(sem_lvm2, boot.ci.type = "bca.simple", standardized = TRUE) %>% 
  kable()

parameterestimates(sem_hvm2, boot.ci.type = "bca.simple", standardized = TRUE) %>% 
  kable()




#10. ANALYSES IN REGRESSION ----

#recode polar_cond into two new variables to represent the contrast codes
#lowvhigh comparing the low and high polarization conditions
#lowhighvmed comparing the low AND high polarization conditions together to the medium condition

pols1_abbrev2 <- pols1_abbrev2 %>% 
  mutate(contrast_lowvhigh_m1 = as.numeric(recode(
    polar_cond_m1, 'Low Polarization' = -1, 
                'Medium1 Polarization' = 0, 
                'High Polarization' = 1))) %>%
  mutate(contrast_lowhighvmed_m1 = as.numeric(recode(
    polar_cond_m1, 'Low Polarization' = 1, 
                'Medium1 Polarization' = -2, 
                'High Polarization' = 1))) %>%
  mutate(contrast_lowvhigh_m2 = as.numeric(recode(
    polar_cond_m2, 'Low Polarization' = -1, 
    'Medium2 Polarization' = 0, 
    'High Polarization' = 1))) %>%
  mutate(contrast_lowhighvmed_m2 = as.numeric(recode(
    polar_cond_m2, 'Low Polarization' = 1, 
    'Medium2 Polarization' = -2, 
    'High Polarization' = 1)))

glimpse(pols1_abbrev2)


#checking to make sure the ns in each contrast match the condition ns after re-coding

table(pols1_abbrev2$polar_cond_m1)
table(pols1_abbrev2$contrast_lowvhigh_m1)
table(pols1_abbrev2$contrast_lowhighvmed_m1)

table(pols1_abbrev2$polar_cond_m2)
table(pols1_abbrev2$contrast_lowvhigh_m2)
table(pols1_abbrev2$contrast_lowhighvmed_m2)

# models predicting polarization manipulation check 
rmodel_linear_check_m1 <- lm(polar_check ~ as.numeric(polar_cond_m1), data = pols1_abbrev2)
summary(rmodel_linear_check_m1)

rmodel_linear_check_m2 <- lm(polar_check ~ as.numeric(polar_cond_m2), data = pols1_abbrev2)
summary(rmodel_linear_check_m2)

# models predicting DV = desire

#first, check the linear effect on desire. No effect, as expected.
rmodel_linear_desire_m1 <- lm(desire_scale ~ as.numeric(polar_cond_m1), data = pols1_abbrev2)
summary(rmodel_linear_desire_m1)

rmodel_linear_desire_m2 <- lm(desire_scale ~ as.numeric(polar_cond_m2), data = pols1_abbrev2)
summary(rmodel_linear_desire_m2)

# contrast between low and high pol, excluding medium pol
rmodel_lvh_desire_m1 <- lm(desire_scale ~ contrast_lowvhigh_m1, data = pols1_abbrev2)
summary(rmodel_lvh_desire_m1)

rmodel_lvh_desire_m2 <- lm(desire_scale ~ contrast_lowvhigh_m2, data = pols1_abbrev2)
summary(rmodel_lvh_desire_m2)

# contrast between low and high pol together versus medium pol
rmodel_lhvm_desire_m1 <- lm(desire_scale ~ contrast_lowhighvmed_m1, data = pols1_abbrev2)
summary(rmodel_lhvm_desire_m1)

rmodel_lhvm_desire_m2 <- lm(desire_scale ~ contrast_lowhighvmed_m2, data = pols1_abbrev2)
summary(rmodel_lhvm_desire_m2)

# models predicting mediator = brokenness

#first, check the linear effect on desire. No effect, as expected.
rmodel_linear_brokenness_m1 <- lm(brokenness_scale ~ as.numeric(polar_cond_m1), data = pols1_abbrev2)
summary(rmodel_linear_brokenness_m1)

rmodel_linear_brokenness_m2 <- lm(brokenness_scale ~ as.numeric(polar_cond_m2), data = pols1_abbrev2)
summary(rmodel_linear_brokenness_m2)

# contrast between low and high pol, excluding medium pol
rmodel_lvh_brokenness_m1 <- lm(brokenness_scale ~ contrast_lowvhigh_m1, data = pols1_abbrev2)
summary(rmodel_lvh_brokenness_m1)

rmodel_lvh_brokenness_m2 <- lm(brokenness_scale ~ contrast_lowvhigh_m2, data = pols1_abbrev2)
summary(rmodel_lvh_brokenness_m2)

# contrast between low and high pol together versus medium pol
rmodel_lhvm_brokenness_m1 <- lm(brokenness_scale ~ contrast_lowhighvmed_m1, data = pols1_abbrev2)
summary(rmodel_lhvm_brokenness_m1)

rmodel_lhvm_brokenness_m2 <- lm(brokenness_scale ~ contrast_lowhighvmed_m2, data = pols1_abbrev2)
summary(rmodel_lhvm_brokenness_m2)

#11. TESTING THE EFFECTS OF DEMAND AS A COVARIATE IN REGRESSION ----

library(QuantPsyc)

# model with the 2 contrast codes and experimenter demand predicting desire
rmodel_desire_m1 <- lm(desire_scale ~ contrast_lowhighvmed_m1+contrast_lowvhigh_m1+exptdemand_scale, data = pols1_abbrev2)
summary(rmodel_desire_m1)
lm.beta(rmodel_desire_m1)

rmodel_desire_m2 <- lm(desire_scale ~ contrast_lowhighvmed_m2+contrast_lowvhigh_m2+exptdemand_scale, data = pols1_abbrev2)
summary(rmodel_desire_m2)
lm.beta(rmodel_desire_m2)

# model with the 2 contrast codes and experimenter demand predicting brokenness
rmodel_brokenness_m1 <- lm(brokenness_scale ~ contrast_lowhighvmed_m1+contrast_lowvhigh_m1+exptdemand_scale, data = pols1_abbrev2)
summary(rmodel_brokenness_m1)
lm.beta(rmodel_brokenness_m1)

rmodel_brokenness_m2 <- lm(brokenness_scale ~ contrast_lowhighvmed_m2+contrast_lowvhigh_m2+exptdemand_scale, data = pols1_abbrev2)
summary(rmodel_brokenness_m2)
lm.beta(rmodel_brokenness_m2)


#12. TESTING THE MODERATION EFFECTS OF DEMAND VIA INTERACTIONS WITH CONTRAST CODES IN REGRESSION ----

# condition moderated by demand: multiply the 2 contrast codes by experimenter demand 
# and enter them together in a model predicting the outcomes; 5 terms in total. 

#6.3c. model with the 3 main effects (contrast codes predicting desire) and 2 interaction terms

# creating the function to center with 'scale()' before passing to mutate() below
center_scale <- function(x) {
  scale(x, scale = FALSE)
}

#mean-centering the exptdemand_scale and adding it back
pols1_abbrev2 <- pols1_abbrev2%>%
  mutate(demand_C = 
           as.vector(center_scale(exptdemand_scale)))

#shows that the mean of demand_C is zero because it is mean centered
summary(pols1_abbrev2$demand_C) 


# model with the 2 contrast codes and experimenter demand predicting desire

mmodel_desire_m1 <- lm(desire_scale ~ contrast_lowhighvmed_m1+contrast_lowvhigh_m1+demand_C+
                         contrast_lowhighvmed_m1*demand_C+contrast_lowvhigh_m1*demand_C, data = pols1_abbrev2)
summary(mmodel_desire_m1)
lm.beta(mmodel_desire_m1)

mmodel_desire_m2 <- lm(desire_scale ~ contrast_lowhighvmed_m2+contrast_lowvhigh_m2+demand_C+
                         contrast_lowhighvmed_m2*demand_C+contrast_lowvhigh_m2*demand_C, data = pols1_abbrev2)
summary(mmodel_desire_m2)
lm.beta(mmodel_desire_m2)


# model with the 2 contrast codes and experimenter demand predicting brokenness

mmodel_brokenness_m1 <- lm(brokenness_scale ~ contrast_lowhighvmed_m1+contrast_lowvhigh_m1+demand_C+
                         contrast_lowhighvmed_m1*demand_C+contrast_lowvhigh_m1*demand_C, data = pols1_abbrev2)
summary(mmodel_brokenness_m1)
lm.beta(mmodel_brokenness_m1)

mmodel_brokenness_m2 <- lm(brokenness_scale ~ contrast_lowhighvmed_m2+contrast_lowvhigh_m2+demand_C+
                         contrast_lowhighvmed_m2*demand_C+contrast_lowvhigh_m2*demand_C, data = pols1_abbrev2)
summary(mmodel_brokenness_m2)
lm.beta(mmodel_brokenness_m2)



