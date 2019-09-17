# set the working directory
getwd()

setwd("/gabriellechristinemartin/Desktop/R_Pol_data_scripts/Paper Study 3")

# clear the environment
rm(list=ls())

library(tidyverse)
library(haven)

#1. IMPORTING, VIEWING, AND SELECTING THE DATA ----

#create data.frame called pols1 from the spss (.sav) file
pols1 <- data.frame(read_sav("/Users/gabriellechristinemartin/Desktop/R_Pol_data_scripts/Paper Study 3/POL_Expt5_original.sav"))

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
# No polarization manipulaiton this time: dropped attdifferent from collection for this study

pols1_abbrev <- pols1 %>%
  select(X._task_id, desirepersonal, desireothers, desirebenefit, 
         brokensystem, systemfailing, electflawed, electpoorfunct,
         X._session_status, polorient, partyid, age, sex, education, ethnicityomb, raceomb) %>%
      mutate(desirepersonal = as.numeric(desirepersonal)) %>%
      mutate(desireothers = as.numeric(desireothers)) %>%
      mutate(desirebenefit = as.numeric(desirebenefit)) %>%
      mutate(systembroken = as.numeric(brokensystem)) %>%
      mutate(healthysystem = as.numeric(systemfailing)) %>%
      mutate(electpointless = as.numeric(electflawed)) %>%
      mutate(electwellfunct = as.numeric(electpoorfunct)) %>%
      mutate(polorient = as.numeric(polorient)) %>%
      mutate(partyid = as.factor(partyid)) %>%
      mutate(education = as.factor(education)) %>%
      mutate(ethnicityomb = as.factor(ethnicityomb)) %>%
      mutate(raceomb = as.factor(raceomb)) %>%
  mutate(broken_cond = as.factor(case_when( 
        X._task_id == "manipinstbroken" ~ 2,
        X._task_id ==  "manipinstnotbroken" ~ 1))) %>%
      mutate(complete = as.factor(case_when(
        X._session_status == "C" ~ 1,
        X._session_status == "null" ~ 0))) %>%
      mutate(gender = as.factor(case_when(
        sex == "m" ~ 1,
        sex == "f" ~ 2))) %>%
      mutate(polorient = (ifelse(
          polorient == 8, NA, polorient)))

glimpse(pols1_abbrev)

#checking to make sure the ns in each factor level are the same after re-coding into new var

table(pols1_abbrev$X._task_id)
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

levels(pols1_abbrev$broken_cond) <- c("Not Broken Condition", "Broken Condition")
summary(pols1_abbrev$broken_cond)

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

# this is the data we currently report BUT see below

#n, complete = 690 reached end of study; 613 did not
#droped Ps under 18 (n = 21): results in 669 responses
# dropped 39 incomplete DV (all three DV responses not present)
# final n = 630 
pols1_abbrev2 <- pols1_abbrev %>%
  select(desirepersonal, desireothers, desirebenefit, 
         systembroken, healthysystem, electpointless, electwellfunct, complete,
         polorient, partyid, age, gender, education, ethnicityomb, raceomb, broken_cond) %>%
          mutate(complete_DV = (as.factor(ifelse(
                    desirepersonal > -99 & desirebenefit > -99 & desireothers > -99, 1, NA)))) %>%
          filter(complete == 'P reached study end') %>%
          filter(age >= 18) %>%
          filter(complete_DV == 1)

# what we should report is complete_DV & age > 18 (ignoring whether Ps got to the end)

# starting n = 1303
# n after dropping incomplete DV = 744
# n after dropping age < 18 = 718
pols1_abbrev2 <- pols1_abbrev %>%
  select(desirepersonal, desireothers, desirebenefit, 
         systembroken, healthysystem, electpointless, electwellfunct, complete,
         polorient, partyid, age, gender, education, ethnicityomb, raceomb, broken_cond) %>%
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
table(pols1_abbrev2$polorient)
summary(pols1_abbrev2$raceomb)
table(pols1_abbrev2$polar_cond)
  

#2. EXAMINING ITEM DISTRIBUTIONS AND SUMMARY STATS ----

# ugly but quick and dirty histograms for each item in primary analyses
# most items look mostly normally-distributed; 
# counts a little low on the high end for desireothers

# desire DV items
hist(pols1_abbrev2$desirepersonal)
hist(pols1_abbrev2$desirebenefit)
hist(pols1_abbrev2$desireothers)

#note the differences in SD across the desireothers and desirepersonal and desirebenefit items
# NAs look roughly the same across items
summary(pols1_abbrev2$desirepersonal)
sd(pols1_abbrev2$desirepersonal)

summary(pols1_abbrev2$desirebenefit)
sd(pols1_abbrev2$desirebenefit)

summary(pols1_abbrev2$desireothers)
sd(pols1_abbrev2$desireothers)

# brokenness manipulation check items
# distributions look a little low on the high end and a little high on the low end

hist(pols1_abbrev2$systembroken)
hist(pols1_abbrev2$healthysystem)
hist(pols1_abbrev2$electpointless)
hist(pols1_abbrev2$electwellfunct)

summary(pols1_abbrev2$systembroken)
sd(pols1_abbrev2$systembroken, na.rm = TRUE)

summary(pols1_abbrev2$healthysystem)
sd(pols1_abbrev2$healthysystem, na.rm = TRUE)

summary(pols1_abbrev2$electpointless)
sd(pols1_abbrev2$electpointless, na.rm = TRUE)

summary(pols1_abbrev2$electwellfunct)
sd(pols1_abbrev2$electwellfunct, na.rm = TRUE)

#3. CHECKING FOR SCALE RELIABILITY AND CREATING SCALES ----

#get the reliability for the four brokenness manipulation check items: alpha = .92
# we re-wrote the items this study so that we don't need to reverse-code, as in the previous

broken_scale <- select(pols1_abbrev2, systembroken, healthysystem, electpointless, electwellfunct)

library(psych)

psych::alpha(broken_scale)

pols1_abbrev2 <- pols1_abbrev2 %>%
  mutate(brokenness_scale =
           ((healthysystem + electwellfunct + systembroken + electpointless)/4)) 


summary(pols1_abbrev2$brokenness_scale)
sd(pols1_abbrev2$brokenness_scale, na.rm = TRUE)

glimpse(pols1_abbrev2)

# checking brokenness_scale means by condition
# mostly matches paper

pols1_abbrev2 %>%
  group_by(broken_cond) %>%
  summarize(
    mean(brokenness_scale, na.rm = TRUE),
    sd(brokenness_scale, na.rm = TRUE),
    n())


#get the reliability for the three primary DV items: alpha = .80

desire_scale <- select(pols1_abbrev2, desirepersonal, desireothers, desirebenefit)

psych::alpha(desire_scale)

# create the desire scale by averaging the three items
pols1_abbrev2 <- pols1_abbrev2 %>%
  mutate(desire_scale =
           (desirepersonal + desireothers + desirebenefit)/3) 

# checking desire_scale means by condition
# mostly matches paper

pols1_abbrev2 %>%
  group_by(broken_cond) %>%
  summarize(
    mean(desire_scale, na.rm = TRUE),
    sd(desire_scale, na.rm = TRUE),
    n())

#4. ANALYSES WITH T-TESTS (AS IN THE MANUSCRIPT) ----

# Question 1: Does the level of brokenness (broken v not broken) in a fictional electoral system affect 
#             desire for inclusion of a third party or idependent candidate in an election?

# Brokenness manipulation effectiveness: check for broken > not broken condition
#     on the manipulation check scale: brokenness_scale

#checking the condition variable class and levels before analysis
class(pols1_abbrev2$broken_cond)
table(pols1_abbrev2$broken_cond)

#get summary info for the manipulation check scale and primary DV by condition
describeBy(pols1_abbrev2$brokenness_scale, pols1_abbrev2$broken_cond, mat=TRUE)
describeBy(pols1_abbrev2$desire_scale, pols1_abbrev2$broken_cond, mat=TRUE)

#check distribution of brokenness scale and desire scale across condition levels

library(ggpubr)

ggboxplot(pols1_abbrev2, x = "broken_cond", y = "brokenness_scale", 
          color = "broken_cond", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("Not Broken Condition", "Broken Condition"),
          ylab = "Broknness Manip Check Scale", xlab = "Brokenness Condition")

ggboxplot(pols1_abbrev2, x = "broken_cond", y = "desire_scale", 
          color = "broken_cond", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("Not Broken Condition", "Broken Condition"),
          ylab = "Broknness Manip Check Scale", xlab = "Brokenness Condition")


#models predicting the brokenness manipulation check: brokenness_scale

# test whether the homogeniety of variance across levels assumption holds
# this code does not work if there is missing data!
# solution: first filter for missing values on the vectors passed to levene.test

table(pols1_abbrev2$polar_cond)

brokencond_broken <- pols1_abbrev2 %>%
  select(brokenness_scale, broken_cond) %>%
  filter(broken_cond != "NA" & brokenness_scale > -99)

# brokencond_broken, n = 674
with(brokencond_broken, lawstat::levene.test(brokenness_scale, as.factor(broken_cond),location="mean"))

# independent 2-group t-test
# the default assumes unequal variance and applies the Welsh df modification.
# we know there are unequal variances across groups, but I should run the levene's test anyway
t.test(pols1_abbrev2$brokenness_scale~pols1_abbrev2$broken_cond) # where y is numeric and x is a binary factor


# effect sizes for the adjusted pairwise comparisons

library(lsr)

# If the original t-test did not make a homogeneity of variance assumption, as per the Welch test, 
# the normalising term should mirror the Welch test (i.e., use method = "unequal"). 

cohensD(x = brokenness_scale ~ broken_cond, data = pols1_abbrev2, method = "unequal")



#models predicting the desire DV: desire_scale

brokencond_desire <- pols1_abbrev2 %>%
  select(desire_scale, broken_cond) %>%
  filter(broken_cond != "NA" & desire_scale > -99)

# brokencond_desire, n = 716
with(brokencond_desire, lawstat::levene.test(desire_scale, as.factor(broken_cond),location="mean"))

t.test(pols1_abbrev2$desire_scale~pols1_abbrev2$broken_cond, var.equal = TRUE) # where y is numeric and x is a binary factor


# effect sizes for the adjusted pairwise comparisons

# If the original t-test did not make a homogeneity of variance assumption, as per the Welch test, 
# the normalising term should mirror the Welch test (i.e., use method = "unequal"). 

cohensD(x = desire_scale ~ broken_cond, data = pols1_abbrev2)

#5. ANALYSES IN REGRESSION ----

# model predicting brokenness scale (manipulation check)
# looks pretty good
rmodel_broken_check <- lm(brokenness_scale ~ as.numeric(broken_cond), data = pols1_abbrev2)
summary(rmodel_broken_check)

# models predicting DV = desire
rmodel_desire <- lm(desire_scale ~ as.numeric(broken_cond), data = pols1_abbrev2)
summary(rmodel_desire)





