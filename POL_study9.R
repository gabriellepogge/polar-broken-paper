# set the working directory
getwd()

setwd("/gabriellechristinemartin/Desktop/R_Pol_data_scripts/POL Expt 9")

# clear the environment
rm(list=ls())

library(tidyverse)
library(haven)

#1. IMPORTING, VIEWING, AND SELECTING THE DATA ----

#create data.frame called pols1 from the spss (.sav) file
pols1 <- data.frame(read_sav("/Users/gabriellechristinemartin/Desktop/R_Pol_data_scripts/POL Expt 9/POL_Expt9_original.sav"))

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
  select(polarcond, brokencond, attsimilar, attdifferent, attoverlap,
         brokensystem, systemfailing, systemflawed, systempoorfunct,
         desirepersonal, desireothers, desirebenefit, 
         thirdsupport1, thirdsupport2, thirdsupport3, thirdsupport4, thirdsupport5, thirdsupport6,
         thirdsupport7, thirdsupport8, fixsystem1, fixsystem2, fixsystem3, fixsystem4, fixsystem5,
         fixsystem6, fixsystem7, fixsystem8, fixsystem9, fixsystem10,
         trust1, trust2, trust3, trust4, trust5, 
         exptdemand1, exptdemand2, exptdemand3, exptdemand4, exptdemand5,
         X._session_status, polorient, partyid, age, sex, education, ethnicityomb, raceomb) %>%
      mutate(partyid = as.factor(partyid)) %>%
      mutate(education = as.factor(education)) %>%
      mutate(ethnicityomb = as.factor(ethnicityomb)) %>%
      mutate(raceomb = as.factor(raceomb)) %>%
  mutate(polar_cond = as.factor(case_when( 
    polarcond == "manipinstmed2pol" ~ 2,
    polarcond ==  "manipinsthighpol" ~ 3,
    polarcond ==  "manipinstlowpol" ~ 1))) %>%
  mutate(broken_cond = as.factor(case_when( 
    brokencond == "manipinstbroken" ~ 2,
    brokencond ==  "manipinstnotbroken" ~ 1))) %>%
  mutate(complete = as.factor(case_when(
        X._session_status == "C" ~ 1,
        X._session_status == "null" ~ 0))) %>%
  mutate(gender = as.factor(case_when(
        sex == "m" ~ 1,
        sex == "f" ~ 2))) 

glimpse(pols1_abbrev)

#checking to make sure the ns in each factor level are the same after re-coding into new var

table(pols1_abbrev$polarcond)
table(pols1_abbrev$polar_cond)
class(pols1_abbrev$polar_cond)

table(pols1_abbrev$brokencond)
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
  

# this is what we will want to report in the manuscript: complete_DV & age > 18 (ignoring whether Ps got to the end)

# dropped 540 incomplete DV (all three DV responses not present), n = 1233
#droped Ps under 18 (n = 9): results in 1224 responses
# final n = 1224

pols1_abbrev2 <- pols1_abbrev %>%
  select(polar_cond, broken_cond, attsimilar, attdifferent, attoverlap,
         brokensystem, systemfailing, systemflawed, systempoorfunct,
         desirepersonal, desireothers, desirebenefit, 
         thirdsupport1, thirdsupport2, thirdsupport3, thirdsupport4, thirdsupport5, thirdsupport6,
         thirdsupport7, thirdsupport8, fixsystem1, fixsystem2, fixsystem3, fixsystem4, fixsystem5,
         fixsystem6, fixsystem7, fixsystem8, fixsystem9, fixsystem10,
         trust1, trust2, trust3, trust4, trust5, 
         exptdemand1, exptdemand2, exptdemand3, exptdemand4, exptdemand5,
         complete, polorient, partyid, age, gender, education, ethnicityomb, raceomb) %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(complete_DV = (as.factor(ifelse(
    desirepersonal > -99 & desirebenefit > -99 & desireothers > -99, 1, NA)))) %>%
  filter(complete_DV == 1) %>%
  filter(age >= 18)
  
glimpse(pols1_abbrev2)

summary(pols1_abbrev2$age)
sd(pols1_abbrev2$age)
summary(pols1_abbrev2$partyid)
summary(pols1_abbrev2$polorient)
sd(pols1_abbrev2$polorient, na.rm = TRUE)
summary(pols1_abbrev2$raceomb)
table(pols1_abbrev2$polar_cond)

pols1_abbrev3 <- pols1_abbrev2 %>%
  mutate(polorient17 = (ifelse(
    polorient == 8, NA, polorient))) 

table(pols1_abbrev2$polorient)
summary(pols1_abbrev3$polorient17)
sd(pols1_abbrev3$polorient17, na.rm = TRUE)

table(pols1_abbrev2$polar_cond)
table(pols1_abbrev2$broken_cond)

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

# secondary DV: general support for third parties and candidates

hist(pols1_abbrev2$thirdsupport1)
hist(pols1_abbrev2$thirdsupport2)
hist(pols1_abbrev2$thirdsupport3)
hist(pols1_abbrev2$thirdsupport4)
hist(pols1_abbrev2$thirdsupport5)
hist(pols1_abbrev2$thirdsupport6)
hist(pols1_abbrev2$thirdsupport7)
hist(pols1_abbrev2$thirdsupport8)

summary(pols1_abbrev2$thirdsupport1)
sd(pols1_abbrev2$thirdsupport1)

summary(pols1_abbrev2$thirdsupport2)
sd(pols1_abbrev2$thirdsupport2)

summary(pols1_abbrev2$thirdsupport3)
sd(pols1_abbrev2$thirdsupport3)

summary(pols1_abbrev2$thirdsupport4)
sd(pols1_abbrev2$thirdsupport4)

summary(pols1_abbrev2$thirdsupport5)
sd(pols1_abbrev2$thirdsupport5)

summary(pols1_abbrev2$thirdsupport6)
sd(pols1_abbrev2$thirdsupport6)

summary(pols1_abbrev2$thirdsupport7)
sd(pols1_abbrev2$thirdsupport7)

summary(pols1_abbrev2$thirdsupport8)
sd(pols1_abbrev2$thirdsupport8)

# secondary DV: support for alternative fixes

hist(pols1_abbrev2$fixsystem1)
hist(pols1_abbrev2$fixsystem2)
hist(pols1_abbrev2$fixsystem3)
hist(pols1_abbrev2$fixsystem4)
hist(pols1_abbrev2$fixsystem5)
hist(pols1_abbrev2$fixsystem6)
hist(pols1_abbrev2$fixsystem7)
hist(pols1_abbrev2$fixsystem8)
hist(pols1_abbrev2$fixsystem9)
hist(pols1_abbrev2$fixsystem10)

summary(pols1_abbrev2$fixsystem1)
sd(pols1_abbrev2$fixsystem1)

summary(pols1_abbrev2$fixsystem2)
sd(pols1_abbrev2$fixsystem2)

summary(pols1_abbrev2$fixsystem3)
sd(pols1_abbrev2$fixsystem3)

summary(pols1_abbrev2$fixsystem4)
sd(pols1_abbrev2$fixsystem4)

summary(pols1_abbrev2$fixsystem5)
sd(pols1_abbrev2$fixsystem5)

summary(pols1_abbrev2$fixsystem6)
sd(pols1_abbrev2$fixsystem6)

summary(pols1_abbrev2$fixsystem7)
sd(pols1_abbrev2$fixsystem7)

summary(pols1_abbrev2$fixsystem8)
sd(pols1_abbrev2$fixsystem8)

summary(pols1_abbrev2$fixsystem9)
sd(pols1_abbrev2$fixsystem9)

summary(pols1_abbrev2$fixsystem10)
sd(pols1_abbrev2$fixsystem10)


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
hist(pols1_abbrev2$systemflawed)
hist(pols1_abbrev2$systempoorfunct)

summary(pols1_abbrev2$brokensystem)
sd(pols1_abbrev2$brokensystem, na.rm = TRUE)

summary(pols1_abbrev2$systemfailing)
sd(pols1_abbrev2$systemfailing, na.rm = TRUE)

summary(pols1_abbrev2$systemflawed)
sd(pols1_abbrev2$systemflawed, na.rm = TRUE)

summary(pols1_abbrev2$systempoorfunct)
sd(pols1_abbrev2$systempoorfunct, na.rm = TRUE)

# trust items to incorporate into the brokenness manipulation check

hist(pols1_abbrev2$trust1)
hist(pols1_abbrev2$trust2)
hist(pols1_abbrev2$trust3)
hist(pols1_abbrev2$trust4)
hist(pols1_abbrev2$trust5)

summary(pols1_abbrev2$trust1)
sd(pols1_abbrev2$trust1, na.rm = TRUE)

summary(pols1_abbrev2$trust2)
sd(pols1_abbrev2$trust2, na.rm = TRUE)

summary(pols1_abbrev2$trust3)
sd(pols1_abbrev2$trust3, na.rm = TRUE)

summary(pols1_abbrev2$trust4)
sd(pols1_abbrev2$trust4, na.rm = TRUE)

summary(pols1_abbrev2$trust5)
sd(pols1_abbrev2$trust5, na.rm = TRUE)

# experimenter demand items

hist(pols1_abbrev2$exptdemand1)
hist(pols1_abbrev2$exptdemand2)
hist(pols1_abbrev2$exptdemand3)
hist(pols1_abbrev2$exptdemand4)
hist(pols1_abbrev2$exptdemand5)

summary(pols1_abbrev2$exptdemand1)
sd(pols1_abbrev2$exptdemand1, na.rm = TRUE)

summary(pols1_abbrev2$exptdemand2)
sd(pols1_abbrev2$exptdemand2, na.rm = TRUE)

summary(pols1_abbrev2$exptdemand3)
sd(pols1_abbrev2$exptdemand3, na.rm = TRUE)

summary(pols1_abbrev2$exptdemand4)
sd(pols1_abbrev2$exptdemand4, na.rm = TRUE)

summary(pols1_abbrev2$exptdemand5)
sd(pols1_abbrev2$exptdemand5, na.rm = TRUE)


#3. CHECKING FOR SCALE RELIABILITY, EFA (scale structure), & CREATING SCALES ----

#3.1a. Reliability for the three polarization manipulation check items: alpha = .83

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

#3.1b. Factor analysis of polarization manipulation check items

# step 1: create the matrix with only the items in the scale
pol_mat <- cor(polar_scale, use = "na.or.complete")
pol_mat

# step 2: factor analysis of the matrix

# in the psych library: fa() function 
# primary arguments:
#   r: the correlation matrix
#   nfactors: number of factors to be extracted (default = 1)
#   rotate: one of several matrix rotation methods, such as "varimax" or "oblimin"
#   fm: one of several factoring methods, such as "pa" (principal axis) or "ml" (maximum likelihood)

# we dont appear to get a scree plot or eigenvalues for the factors with the code below

fa_pol <- fa(r = pol_mat, nfactors = 1, rotate = "oblimin", fm = "pa")
fa_pol

#3.1c. Means, sd, and ns for each condition
# these two analyses provide the same ns and means
pols1_abbrev2 %>%
  group_by(polar_cond, broken_cond) %>%
  summarize(
    mean(pol_check_scale, na.rm = TRUE),
    sd(pol_check_scale, na.rm = TRUE),
    n())

pols1_abbrev2 %>%
  group_by(brokenpol_cond) %>%
  summarize(
    mean(pol_check_scale, na.rm = TRUE),
    sd(pol_check_scale, na.rm = TRUE),
    n())


#3.2a. Reliability for the four brokenness manipulation check items: alpha = .95
# no need to reverse-code

broken_scale <- select(pols1_abbrev2, brokensystem, systemfailing, systemflawed, systempoorfunct)

psych::alpha(broken_scale)

pols1_abbrev2 <- pols1_abbrev2 %>%
  mutate(broken_check_scale =
           ((systemfailing + systempoorfunct + brokensystem + systemflawed)/4)) 


#3.2c. Means, sd, and ns for each condition

pols1_abbrev2 %>%
  group_by(polar_cond, broken_cond) %>%
  summarize(
    mean(broken_check_scale, na.rm = TRUE),
    sd(broken_check_scale, na.rm = TRUE),
    n())

#3.3a. Reliability for the five dis(trust) items: alpha = .86
# no need to reverse-code

distrust_scale <- select(pols1_abbrev2, trust1, trust2, trust3, trust4, trust5)

psych::alpha(distrust_scale)

pols1_abbrev2 <- pols1_abbrev2 %>%
  mutate(trust_check_scale =
           ((trust1 + trust2 + trust3 + trust4 + trust5)/5)) 

#3.3c. Means, sd, and ns for each condition

pols1_abbrev2 %>%
  group_by(polar_cond, broken_cond) %>%
  summarize(
    mean(trust_check_scale, na.rm = TRUE),
    sd(trust_check_scale, na.rm = TRUE),
    n())


#3.4a. Reliability for the nine brokenness + dis(trust) items: alpha = .92

brdist_scale <- select(pols1_abbrev2, brokensystem, systemfailing, systemflawed, systempoorfunct, 
                       trust1, trust2, trust3, trust4, trust5)

psych::alpha(brdist_scale)

#3.4b. Factor analysis of brokenness manipulation check + dis(trust) items

# step 1: create the matrix with only the items in the scale
brdist_mat <- cor(brdist_scale, use = "na.or.complete")
brdist_mat

# step 2: factor analysis of the matrix

fa_brdist <- fa(r = brdist_mat, nfactors = 1, rotate = "oblimin", fm = "pa")
fa_brdist

#3.4c. Means, sd, and ns for each condition

pols1_abbrev2 <- pols1_abbrev2 %>%
  mutate(brtrust_check_scale =
           ((systemfailing + systempoorfunct + brokensystem + systemflawed + 
               trust1 + trust2 + trust3 + trust4 + trust5)/9)) 

pols1_abbrev2 %>%
  group_by(polar_cond, broken_cond) %>%
  summarize(
    mean(brtrust_check_scale, na.rm = TRUE),
    sd(brtrust_check_scale, na.rm = TRUE),
    n())


#3.5a. Reliability for the three primary DV items: alpha = .78

desire_scale <- select(pols1_abbrev2, desirepersonal, desireothers, desirebenefit)

psych::alpha(desire_scale)

#3.5b. Factor analysis of primary DV = desire items

# step 1: create the matrix with only the items in the scale
desire_mat <- cor(desire_scale, use = "na.or.complete")
desire_mat

# step 2: factor analysis of the matrix

fa_desire <- fa(r = desire_mat, nfactors = 1, rotate = "oblimin", fm = "pa")
fa_desire

#3.5c. Means, sd, and ns for each condition

pols1_abbrev2 <- pols1_abbrev2 %>%
  mutate(desire_scale =
           (desirepersonal + desireothers + desirebenefit)/3) 

pols1_abbrev2 %>%
  group_by(polar_cond, broken_cond) %>%
  summarize(
    mean(desire_scale, na.rm = TRUE),
    sd(desire_scale, na.rm = TRUE),
    n())


#3.6a. Reliability for the 8 secondary support for third parties items: alpha = .90

support_scale <- select(pols1_abbrev2, thirdsupport1, thirdsupport2, thirdsupport3, 
                             thirdsupport4, thirdsupport5, thirdsupport6, thirdsupport7,
                             thirdsupport8)

psych::alpha(support_scale)

#3.6b. Factor analysis of secondary Dv = general support for third parties items

# step 1: create the matrix with only the items in the scale
support_mat <- cor(support_scale, use = "na.or.complete")
support_mat

# step 2: factor analysis of the matrix

fa_support <- fa(r = support_mat, nfactors = 1, rotate = "oblimin", fm = "pa")
fa_support

#3.6c. Means, sd, and ns for each condition

pols1_abbrev2 <- pols1_abbrev2 %>%
  mutate(thirdsupport_scale =
           (thirdsupport1 + thirdsupport2 + thirdsupport3 +
            thirdsupport4 + thirdsupport5 + thirdsupport6 + thirdsupport7 +
            thirdsupport8)/8)

pols1_abbrev2 %>%
  group_by(polar_cond, broken_cond) %>%
  summarize(
    mean(thirdsupport_scale, na.rm = TRUE),
    sd(thirdsupport_scale, na.rm = TRUE),
    n())


#3.7a. Reliability for the 10 general fixes items: alpha = .87

fixes_scale <- select(pols1_abbrev2, fixsystem1, fixsystem2, fixsystem3, 
                        fixsystem4, fixsystem5, fixsystem6, fixsystem7,
                      fixsystem8, fixsystem9, fixsystem10)

psych::alpha(fixes_scale)

#3.7b. Factor analysis of secondary Dv = support for general fixes items

# step 1: create the matrix with only the items in the scale
fixes_mat <- cor(fixes_scale, use = "na.or.complete")
fixes_mat

# step 2: factor analysis of the matrix

fa_fixes <- fa(r = fixes_mat, nfactors = 1, rotate = "oblimin", fm = "pa")
fa_fixes

pols1_abbrev2 <- pols1_abbrev2 %>%
  mutate(genfixes_scale =
           (fixsystem1 + fixsystem2 + fixsystem3 + 
            fixsystem4 + fixsystem5 + fixsystem6 + fixsystem7 +
            fixsystem8 + fixsystem9 + fixsystem10)/10)

#3.7c. Means, sd, and ns for each condition

pols1_abbrev2 %>%
  group_by(polar_cond, broken_cond) %>%
  summarize(
    mean(genfixes_scale, na.rm = TRUE),
    sd(genfixes_scale, na.rm = TRUE),
    n())


#3.8a. Reliability for the 5 experimenter demand items: alpha = .56

demand_scale <- select(pols1_abbrev2, exptdemand1, exptdemand2, exptdemand3, 
                      exptdemand4, exptdemand5)

psych::alpha(demand_scale)

demand_scale3 <- dplyr::select(pols1_abbrev2, exptdemand1, exptdemand4, exptdemand5)
                       
psych::alpha(demand_scale3)


#3.8b. Factor analysis of experimenter demand items

# step 1: create the matrix with only the items in the scale
demand_mat <- cor(demand_scale, use = "na.or.complete")
demand_mat

# step 2: factor analysis of the matrix
# items 2 & 3 have low loadings

fa_demand <- fa(r = demand_mat, nfactors = 1, rotate = "oblimin", fm = "pa")
fa_demand

fa_demand2 <- fa(r = demand_mat, nfactors = 2, rotate = "oblimin", fm = "pa")
fa_demand2

#3.8c. Means, sd, and ns for each condition

pols1_abbrev2 <- pols1_abbrev2 %>%
  mutate(exptdemand_scale =
           (exptdemand1 + exptdemand4 + exptdemand5)/3)


pols1_abbrev2 %>%
  group_by(polar_cond, broken_cond) %>%
  summarize(
    mean(exptdemand_scale, na.rm = TRUE),
    sd(exptdemand_scale, na.rm = TRUE),
    n())

pols1_abbrev2 %>%
  group_by(polar_cond, broken_cond) %>%
  summarize(
    mean(exptdemand3, na.rm = TRUE),
    sd(exptdemand3, na.rm = TRUE),
    n())

pols1_abbrev2 %>%
  group_by(polar_cond, broken_cond) %>%
  summarize(
    mean(exptdemand2, na.rm = TRUE),
    sd(exptdemand2, na.rm = TRUE),
    n())


#4. ANALYSES OF THE TWO MANIPULATION CHECK SCALES (AS IN THE MANUSCRIPT) ----

# Polarization manipulation effectiveness: check for a positive linear effect of condition on
#     the polarization manipulation check scale: pol_check_scale
# Brokenness manipulation effectiveness: check for broken > not broken condition
#     on the brokenness manipulation check scale: broken_check_scale

# 4.1a Omnibus ANOVA for the polarization manipulation check scale by condition
amodel_omnibus_polcheck <- aov(pol_check_scale ~ polar_cond, data = pols1_abbrev2)
summary(amodel_omnibus_polcheck)

# 4.1b Testing homogeneity across levels assumption 
# car package appears to be unavailable although listed on CRAN
# can also get levene's test in lawstat package
library(car)
library(lawstat)

# test whether the homogeniety of variance across levels assumption holds
# this code does not work if there is missing data!
# solution: first filter out missing values on the vectors passed to levene.test

table(pols1_abbrev2$polar_cond)

polarcond_polarcheck <- pols1_abbrev2 %>%
  select(pol_check_scale, polar_cond) %>%
  filter(polar_cond != "NA" & pol_check_scale > -99)

with(polarcond_polarcheck, lawstat::levene.test(pol_check_scale, as.factor(polar_cond),location="mean"))

# we know we have unequal variances across levels 
# thus, need to report the Welch-adjusted ANOVA and games-howell-adjusted parameters for the pairwise comparisons

# 4.1c. Welch's test
wmodel_omnibus_polarcheck <- oneway.test(pol_check_scale ~ as.factor(polar_cond), data = polarcond_polarcheck)
wmodel_omnibus_polarcheck

# 4.1d. Games-howell adjusted pairwise comparisons

library(userfriendlyscience)
library(MBESS)

gh.polarcheck_scale <- oneway(polarcond_polarcheck$polar_cond, y = polarcond_polarcheck$pol_check_scale, posthoc = 'games-howell')
gh.polarcheck_scale

# effect sizes

library(lsr)

# Error: grouping factor must have exactly 2 levels
# potential solution: create new dfs by filtering for the relevant conditions for comparison
cohensD(x = pol_check_scale ~ LvM, data = pols1_abbrev2, method = "unequal")
cohensD(x = pol_check_scale ~ HvM, data = pols1_abbrev2, method = "unequal")
cohensD(x = pol_check_scale ~ LvH, data = pols1_abbrev2, method = "unequal")

# If the original t-test did not make a homogeneity of variance assumption, as per the Welch test, the normalising term should mirror the Welch test (method = "unequal"). 

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


# 4.2a. Homogeneity of variance across the broken and not-broken conditions

brokencond_brokencheck <- pols1_abbrev2 %>%
  select(brtrust_check_scale, broken_cond) %>%
  filter(broken_cond != "NA" & brtrust_check_scale > -99)

with(brokencond_brokencheck, lawstat::levene.test(brtrust_check_scale, as.factor(broken_cond),location="mean"))


# 4.2b. Independent 2-group t-test for the effect of brokenness condition on the brokenness manipulation check scale
#     the default assumes unequal variance and applies the Welsh df modification.
#     we know there are equal variances across groups here, so turn off that default
t.test(pols1_abbrev2$brtrust_check_scale~pols1_abbrev2$broken_cond, var.equal = TRUE) # where y is numeric and x is a binary factor

cohensD(x = brtrust_check_scale ~ broken_cond, data = pols1_abbrev2)



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

# condition ns 
table(pols1_abbrev2$brokenpol_cond)

class(pols1_abbrev2$brokenpol_cond)

#5.2 Create the n(conditions) - 1 = 5 contrast codes from brokenpol_cond:
#         Code 1: the main effect of brokenness condition: not-broken versus broken
#         Code 2: the first main effect of polarization condition: low AND high versus medium polarization
#         Code 3: the second main effect of polarization condition: low versus high (excluding medium polarization)
#         Code 4: the interaction between brokenness condition (not-broken versus broken) and low AND high versus medium polarization
#         Code 5: the interaction between brokenness condition (not-broken versus broken) and low versus high (excluding medium polarization)


pols1_abbrev2 <- pols1_abbrev2 %>%
  mutate(code1 = recode(
                brokenpol_cond, '1'=-1, '2'=-1, '3'=-1, '4'=1, '5'=1, '6'=1)) %>%
  mutate(code2 = recode(
                brokenpol_cond, '1'=1, '2'=-2, '3'=1, '4'=1, '5'=-2, '6'=1)) %>%
  mutate(code3 = recode(
                brokenpol_cond, '1'=1, '2'=0, '3'=-1, '4'=1, '5'=0, '6'=-1)) %>%
  mutate(code4 = recode(
                brokenpol_cond, '1'=-1, '2'=2, '3'=-1, '4'=1, '5'=-2, '6'=1)) %>%
  mutate(code5 = recode(
                brokenpol_cond, '1'=-1, '2'=0, '3'=1, '4'=1, '5'=0, '6'=-1))

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

# 5.3a. Primary DV = Desire for inclusion of a third party candidate

# check for homogeneity of variance across the 6 conditions for the primary DV: desire_scale

polarbroken_desire <- pols1_abbrev2 %>%
  select(desire_scale, brokenpol_cond_fact) %>%
  filter(brokenpol_cond_fact != "NA" & desire_scale > -99)

with(polarbroken_desire, lawstat::levene.test(desire_scale, as.factor(brokenpol_cond_fact),location="mean"))

# desire DV omnibus test
# equal variances across conditions so no need for adjustment
amodel_omnibus_desire <- aov(desire_scale ~ brokenpol_cond_fact, data = pols1_abbrev2)
summary(amodel_omnibus_desire)

# the below provides the estimates for the two contrasts and the omnibus model
# Make sure to use summary.aov here or 'split' might not work
# make sure the contrasts are listed in the same order they appear in the matrix!
summary.aov(amodel_omnibus_desire, split=list(brokenpol_cond_fact=list("Code 1: contrast_bvnb" = 1,"Code 2: contrast_lhvm" = 2, 
                                                            "Code 3: contrast_lvh" = 3, "Code 4: contrast_blhvm" = 4,
                                                              "Code 5: contrast_blvh" = 5))) 

# effect sizes for the contrasts: contrast correlation (Rosnow, Rosenthal, & Rubin, 2000)
# the below provides the partial correlation between scores on the outcome variable
# and the lambdas associated with the groups, after eliminating all between-group non-contrast variation

# rcontrast = sqrt(F or t^2)/sqrt(F or t^2 + df_within)

# Broken versus not-broken contrast

sqrt(13.208)/sqrt(13.208 + 1218)

# High and low versus medium contrast

sqrt(0.945)/sqrt(0.945 + 1218)

# High versus low contrast

sqrt(0.530)/sqrt(0.530  + 1218)


# 5.3b. Secondary DV = General support for third parties 

# check for homogeneity of variance across the 6 conditions for the secondary DV: Support for third parties

polarbroken_support <- pols1_abbrev2 %>%
  select(thirdsupport_scale, brokenpol_cond_fact) %>%
  filter(brokenpol_cond_fact != "NA" & thirdsupport_scale > -99)

with(polarbroken_support, lawstat::levene.test(thirdsupport_scale, as.factor(brokenpol_cond_fact),location="mean"))

# secondary DV, support for third parties omnibus test
# equal variances across conditions so no need foradjustment
amodel_omnibus_thirdsupport <- aov(thirdsupport_scale ~ brokenpol_cond_fact, data = pols1_abbrev2)

summary.aov(amodel_omnibus_thirdsupport, split=list(brokenpol_cond_fact=list("Code 1: contrast_bvnb" = 1,"Code 2: contrast_lhvm" = 2, 
                                                                       "Code 3: contrast_lvh" = 3, "Code 4: contrast_blhvm" = 4,
                                                                       "Code 5: contrast_blvh" = 5))) 

# rcontrast = sqrt(F or t^2)/sqrt(F or t^2 + df_within)

# Broken versus not-broken contrast

sqrt(8.235)/sqrt(8.235 + 1135)

# High and low versus medium contrast

sqrt(0.125)/sqrt(0.125 + 1135)

# High versus low contrast

sqrt(1.552)/sqrt(1.552  + 1135)

# 5.3c. Secondary DV = Support for alternative fixes 

# check for homogeneity of variance across the 6 conditions for the secondary DV: Support for alternative fixes

polarbroken_fixes <- pols1_abbrev2 %>%
  select(genfixes_scale, brokenpol_cond_fact) %>%
  filter(brokenpol_cond_fact != "NA" & genfixes_scale > -99)

with(polarbroken_fixes, lawstat::levene.test(genfixes_scale, as.factor(brokenpol_cond_fact),location="mean"))

# equal variances, so no need to adjust 
amodel_omnibus_genfixes <- aov(genfixes_scale ~ brokenpol_cond_fact, data = pols1_abbrev2)
summary(amodel_omnibus_genfixes)

summary.aov(amodel_omnibus_genfixes, split=list(brokenpol_cond_fact=list("Code 1: contrast_bvnb" = 1,"Code 2: contrast_lhvm" = 2, 
                                                                             "Code 3: contrast_lvh" = 3, "Code 4: contrast_blhvm" = 4,
                                                                             "Code 5: contrast_blvh" = 5))) 

# rcontrast = sqrt(F or t^2)/sqrt(F or t^2 + df_within)

# Broken versus not-broken contrast

sqrt(11.412)/sqrt(11.412 + 1139)

# High and low versus medium contrast

sqrt(0.002)/sqrt(0.002 + 1139)

# High versus low contrast

sqrt(7.873)/sqrt(7.873  + 1139)

#6. ANALYSES IN REGRESSION ----

#predict the three DVs from the 5 contrast codes

glimpse(pols1_abbrev2)


#6.1a. model predicting the polarization manipulation check_scale 
rmodel_linear_polcheck <- lm(pol_check_scale ~ as.numeric(polar_cond), data = pols1_abbrev2)
summary(rmodel_linear_polcheck)

#6.2a. model predicting the brokenness manipulation check_scale
rmodel_linear_brokencheck <- lm(brtrust_check_scale ~ as.numeric(broken_cond), data = pols1_abbrev2)
summary(rmodel_linear_brokencheck)

#6.3a. model with the 5 contrast codes predicting desire
rmodel_desire <- lm(desire_scale ~ code1+code2+code3+code4+code5, data = pols1_abbrev2)
summary(rmodel_desire)

#6.4a. model with the 5 contrast codes predicting general support for third parties
rmodel_support <- lm(thirdsupport_scale ~ code1+code2+code3+code4+code5, data = pols1_abbrev2)
summary(rmodel_support)

#6.5a. model with the 5 contrast codes predicting alternative fixes
rmodel_fixes <- lm(genfixes_scale ~ code1+code2+code3+code4+code5, data = pols1_abbrev2)
summary(rmodel_fixes)

# ADD DEMAND AS A COVARIATE IN REGRESSION ----

library(QuantPsyc)

#6.3b. model with the 5 contrast codes predicting desire
rmodel_desire_b <- lm(desire_scale ~ code1+code2+code3+code4+code5+exptdemand_scale, data = pols1_abbrev2)
summary(rmodel_desire_b)
lm.beta(rmodel_desire_b)

#6.4b. model with the 5 contrast codes predicting general support for third parties
rmodel_support_b <- lm(thirdsupport_scale ~ code1+code2+code3+code4+code5+exptdemand_scale, data = pols1_abbrev2)
summary(rmodel_support)

#6.5b. model with the 5 contrast codes predicting alternative fixes
rmodel_fixes_b <- lm(genfixes_scale ~ code1+code2+code3+code4+code5+exptdemand_scale, data = pols1_abbrev2)
summary(rmodel_fixes)

# CONDITION CONTRASTS MODERATED BY DEMAND ----
# multiply the 5 contrast codes by experimenter bias 
# and enter them together in a model predicting the outcomes; ten terms in total. 

#6.3c. model with the 6 main effects (contrast codes predicting desire) and 5 interaction terms

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

rmodel_desire_c <- lm(desire_scale ~ code1+code2+code3+code4+code5+demand_C+
                        code1*demand_C+code2*demand_C+code3*demand_C+
                        code4*demand_C+code5*demand_C, data = pols1_abbrev2)
summary(rmodel_desire_c)
lm.beta(rmodel_desire_c)

# this method does not first center the main effect and I assume the multicollinearity
# washes out the effect of brokenness condition
rmodel_desire_d <- lm(desire_scale ~ code1+code2+code3+code4+code5+exptdemand_scale+
                        code1*exptdemand_scale+code2*exptdemand_scale+code3*exptdemand_scale+
                        code4*exptdemand_scale+code5*exptdemand_scale, data = pols1_abbrev2)
summary(rmodel_desire_d)
lm.beta(rmodel_desire_d)

#6.4c. model with the 5 contrast codes predicting general support for third parties
rmodel_support_c <- lm(thirdsupport_scale ~ code1+code2+code3+code4+code5+demand_C+
                         code1*demand_C+code2*demand_C+code3*demand_C+
                         code4*demand_C+code5*demand_C, data = pols1_abbrev2)
summary(rmodel_support_c)

#6.5c. model with the 5 contrast codes predicting alternative fixes
rmodel_fixes_c <- lm(genfixes_scale ~ code1+code2+code3+code4+code5+demand_C+
                       code1*demand_C+code2*demand_C+code3*demand_C+
                       code4*demand_C+code5*demand_C, data = pols1_abbrev2)
summary(rmodel_fixes_c)


#7. PLOT THE EFFECTS OF THE TWO MANIPULATIONS SIMULTANEOUSLY ----

# line graph with polarization condition on the x-axis and 2 lines representing levels of brokenness condition

library("ggpubr")
ggboxplot(my_data, x = "dose", y = "len", color = "supp",
          palette = c("#00AFBB", "#E7B800"))

ggline(my_data, x = "dose", y = "len", color = "supp",
       add = c("mean_se", "dotplot"),
       palette = c("#00AFBB", "#E7B800"))








  



