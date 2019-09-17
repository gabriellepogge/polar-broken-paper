# set the working directory
getwd()

setwd("/gabriellechristinemartin/Desktop/R_Pol_data_scripts/POL Expt 10")

# clear the environment
rm(list=ls())

library(tidyverse)
library(haven)

#1. IMPORTING, VIEWING, AND SELECTING THE DATA ----

#create data.frame called pols1 from the spss (.sav) file
pols1 <- data.frame(read_sav("/Users/gabriellechristinemartin/Desktop/R_Pol_data_scripts/POL Expt 10/POL_Expt10_original.sav"))

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
         desirepersonal, desireothers, desirebenefit,
         brokensystem, systemfailing, systemflawed, systempoorfunct,
         trust1, trust2, trust3, trust4, trust5, 
         nochoice1, nochoice2, nochoice3, nochoice4, nochoice5,
         exptdemand1, exptdemand2, exptdemand3, exptdemand4, exptdemand5,
         X._session_status, polorient, partyid, age, sex, education, ethnicityom, raceomb) %>%
      mutate(partyid = as.factor(partyid)) %>%
      mutate(education = as.factor(education)) %>%
      mutate(ethnicityom = as.factor(ethnicityom)) %>%
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
  

# this is what we will want to report in the manuscript: complete_DV & age > 18 (ignoring whether Ps got to the end)

# dropped 342 incomplete DV (all three DV responses not present), n = 877
#droped Ps under 18 (n = 4): results in 873 responses
# final n = 873

pols1_abbrev2 <- pols1_abbrev %>%
  select(polar_cond, broken_cond, attsimilar, attdifferent, attoverlap,
         desirepersonal, desireothers, desirebenefit,
         brokensystem, systemfailing, systemflawed, systempoorfunct,
         trust1, trust2, trust3, trust4, trust5, 
         nochoice1, nochoice2, nochoice3, nochoice4, nochoice5,
         exptdemand1, exptdemand2, exptdemand3, exptdemand4, exptdemand5,
         complete, polorient, partyid, age, gender, education, ethnicityom, raceomb) %>%
  mutate_if(is.character,as.numeric) %>%
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
# frequencies are low on the high end and high on the low end

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
# modal response is "1" for every item; floor effects?

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

# lack of choice items
# frequencies are low on the high end and high on the low end

hist(pols1_abbrev2$nochoice1)
hist(pols1_abbrev2$nochoice2)
hist(pols1_abbrev2$nochoice3)
hist(pols1_abbrev2$nochoice4)
hist(pols1_abbrev2$nochoice5)

summary(pols1_abbrev2$nochoice1)
sd(pols1_abbrev2$nochoice1, na.rm = TRUE)

summary(pols1_abbrev2$nochoice2)
sd(pols1_abbrev2$nochoice2, na.rm = TRUE)

summary(pols1_abbrev2$nochoice3)
sd(pols1_abbrev2$nochoice3, na.rm = TRUE)

summary(pols1_abbrev2$nochoice4)
sd(pols1_abbrev2$nochoice4, na.rm = TRUE)

summary(pols1_abbrev2$nochoice5)
sd(pols1_abbrev2$nochoice5, na.rm = TRUE)


#3. CHECKING FOR SCALE RELIABILITY, EFA (scale structure), & CREATING SCALES ----

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

#3.1a. Reliability for the three polarization manipulation check items: alpha = .82

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


#3.2a. Reliability for the four brokenness manipulation check items: alpha = .94
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

#3.4a. Reliability for the nine brokenness + dis(trust) items: alpha = .93

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


#3.5a. Reliability for the three primary DV items: alpha = .74

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


#3.8a. Reliability for the 5 experimenter demand items: alpha = .61

demand_scale <- select(pols1_abbrev2, exptdemand1, exptdemand2, exptdemand3, 
                      exptdemand4, exptdemand5)

psych::alpha(demand_scale)

demand_scale145 <- select(pols1_abbrev2, exptdemand1,exptdemand4, exptdemand5)

psych::alpha(demand_scale145)

cor.test(pols1_abbrev2$exptdemand2,pols1_abbrev2$exptdemand3)

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

pols1_abbrev2 <- pols1_abbrev2 %>%
  mutate(exptdemand_scale23 =
           (exptdemand2 + exptdemand3)/2)



pols1_abbrev2 %>%
  group_by(polar_cond, broken_cond) %>%
  summarize(
    mean(exptdemand_scale, na.rm = TRUE),
    sd(exptdemand_scale, na.rm = TRUE),
    n())

pols1_abbrev2 %>%
  group_by(polar_cond, broken_cond) %>%
  summarize(
    mean(exptdemand_scale23, na.rm = TRUE),
    sd(exptdemand_scale23, na.rm = TRUE),
    n())


#3.9a. Reliability for the 5 perceived lack of choice items: alpha = .52

# inter-item correlations suggest reverse-coding item 5 prior to combining the items
pols1_abbrev2 <- pols1_abbrev2 %>%
  mutate(nochoice5_r = 
           6 - nochoice5) %>%
  mutate(nochoice_scale =
           ((nochoice1 + nochoice2 + nochoice3 + nochoice4 + nochoice5_r)/5)) 

choice_scale <- select(pols1_abbrev2, nochoice1, nochoice2, nochoice3, nochoice4, nochoice5_r)

psych::alpha(choice_scale)

# try dropping item 3: perceived consensus (doesn't load in the below); alpha = .61

choice_scale2 <- select(pols1_abbrev2, nochoice1, nochoice2, nochoice4, nochoice5_r)

psych::alpha(choice_scale2)

# try dropping items 3 & 4: candidates too extreme (doesn't load in the below); alpha = .71

choice_scale3 <- select(pols1_abbrev2, nochoice1, nochoice2, nochoice5_r)

psych::alpha(choice_scale3)

# correlation between item 1 and 2

cor.test(pols1_abbrev2$nochoice1, pols1_abbrev2$nochoice2,
         alternative = "two.sided",
         method = c("pearson"),
         exact = NULL, conf.level = 0.95)

#3.9b. Factor analysis of lack of choice items

# step 1: create the matrix with only the items in the scale
choice_mat <- cor(choice_scale, use = "na.or.complete")
choice_mat

# step 2: factor analysis of the matrix
# items 3 & 4 do not load

fa_choice <- fa(r = choice_mat, nfactors = 1, rotate = "oblimin", fm = "pa")
fa_choice

# trying two factors: item 3 still doesnt load anywhere; factor 2 only has a single item: item 5
# no support for two factor structure
fa_choice2 <- fa(r = choice_mat, nfactors = 2, rotate = "oblimin", fm = "pa")
fa_choice2


#3.9c. Means, sd, and ns for each condition

pols1_abbrev2 %>%
  group_by(polar_cond, broken_cond) %>%
  summarize(
    mean(nochoice_scale, na.rm = TRUE),
    sd(nochoice_scale, na.rm = TRUE),
    n())

# dropping the two items that didn't load above moves the means around substantially
# BUT, we still see a negative linear effect from low to high, as we have been seeing for the desire DV
pols1_abbrev2 <- pols1_abbrev2 %>%
  mutate(nochoice_scale_3 =
           ((nochoice1 + nochoice2 + nochoice5_r)/3))

pols1_abbrev2 %>%
  group_by(polar_cond, broken_cond) %>%
  summarize(
    mean(nochoice_scale_3, na.rm = TRUE),
    sd(nochoice_scale_3, na.rm = TRUE),
    n())


#3.10a. Reliability for the 3 polarization manip check items and the 5 perceived lack of choice items: alpha = .75
# when combining with the polarization items, nochoice5 should NOT be reverse-coded?

polchoice_scale <- select(pols1_abbrev2, attsimilar_r, attdifferent, attoverlap_r,
                          nochoice1, nochoice2, nochoice3, nochoice4, nochoice5)

psych::alpha(polchoice_scale, check.keys=TRUE)

#3.10b. Factor analysis of the 3 polarization manip check items and the 5 lack of choice items

# step 1: create the matrix with only the items in the scale
polchoice_mat <- cor(polchoice_scale, use = "na.or.complete")
polchoice_mat

# step 2: factor analysis of the matrix
# choice items 3 & 4 do not load

fa_polchoice <- fa(r = polchoice_mat, nfactors = 1, rotate = "oblimin", fm = "pa")
fa_polchoice

# testing a two-factor solution

fa_polchoice2 <- fa(r = polchoice_mat, nfactors = 2, rotate = "oblimin", fm = "pa")
fa_polchoice2


#3.11a. Reliability for the 4 brokenness manip check items and the 5 perceived lack of choice items: alpha = .76
# when combining with the polarization items, nochoice5 SHOULD be reverse-coded?

brchoice_scale <- select(pols1_abbrev2, systemfailing, systempoorfunct, brokensystem, systemflawed,
                          nochoice1, nochoice2, nochoice3, nochoice4, nochoice5)

psych::alpha(brchoice_scale, check.keys=TRUE)

#3.11b. Factor analysis of the 4 brokenness manip check items and the 5 lack of choice items

# step 1: create the matrix with only the items in the scale
brchoice_mat <- cor(brchoice_scale, use = "na.or.complete")
brchoice_mat

# step 2: factor analysis of the matrix
# none of the choice items load well; 2, 3, & 5 don't load at all

fa_brchoice <- fa(r = brchoice_mat, nfactors = 1, rotate = "oblimin", fm = "pa")
fa_brchoice


#3.12a. Reliability for the 4 brokenness manip check items, 5 trust items, and the 5 perceived lack of choice items: alpha = .86
# when combining with the polarization items, nochoice5 SHOULD be reverse-coded?

tbrchoice_scale <- select(pols1_abbrev2, systemfailing, systempoorfunct, brokensystem, systemflawed,
                         trust1, trust2, trust3, trust4, trust5,
                         nochoice1, nochoice2, nochoice3, nochoice4, nochoice5)

psych::alpha(tbrchoice_scale, check.keys=TRUE)

#3.12b. Factor analysis of the 4 brokenness manip check items, 5 trust items, and the 5 lack of choice items

# step 1: create the matrix with only the items in the scale
tbrchoice_mat <- cor(tbrchoice_scale, use = "na.or.complete")
tbrchoice_mat

# step 2: factor analysis of the matrix
# none of the choice items load well; 2, 3, & 5 don't load at all

fa_tbrchoice <- fa(r = tbrchoice_mat, nfactors = 1, rotate = "oblimin", fm = "pa")
fa_tbrchoice

# testing a two-factor solution

fa_tbrchoice2 <- fa(r = tbrchoice_mat, nfactors = 2, rotate = "oblimin", fm = "pa")
fa_tbrchoice2


# assessing the subscale comprised of choice items #1 & #2 by polarization and brokenness condition

pols1_abbrev2 <- pols1_abbrev2 %>%
    mutate(nochoice12 = (nochoice1 + nochoice2)/2)

pols1_abbrev2 %>%
  group_by(polar_cond, broken_cond) %>%
  summarize(
    mean(nochoice12, na.rm = TRUE),
    sd(nochoice12, na.rm = TRUE),
    n())

# assessing the individual choice items by polarization and brokenness condition

pols1_abbrev2 %>%
  group_by(polar_cond, broken_cond) %>%
  summarize(
    mean(nochoice1, na.rm = TRUE),
    sd(nochoice1, na.rm = TRUE),
    mean(nochoice2, na.rm = TRUE),
    sd(nochoice2, na.rm = TRUE),
    n())    

pols1_abbrev2 %>%
  group_by(polar_cond, broken_cond) %>%
  summarize(  
    mean(nochoice3, na.rm = TRUE),
    sd(nochoice3, na.rm = TRUE),
    mean(nochoice4, na.rm = TRUE),
    sd(nochoice4, na.rm = TRUE),
    n())

pols1_abbrev2 %>%
  group_by(polar_cond, broken_cond) %>%
  summarize(
    mean(nochoice5_r, na.rm = TRUE),
    sd(nochoice5_r, na.rm = TRUE),
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
# solution: first filter for missing values on the vectors passed to levene.test

table(pols1_abbrev2$polar_cond)

polarcond_polarcheck <- pols1_abbrev2 %>%
  select(pol_check_scale, polar_cond) %>%
  filter(polar_cond != "NA" & pol_check_scale > -99)

with(polarcond_polarcheck, lawstat::levene.test(pol_check_scale, as.factor(polar_cond),location="mean"))

# we know we have unequal variances across levels 
# thus, need to report the Welch-adjusted ANOVA and games-howell-adjusted parameters for the pairwise comparisons

# 4.1c. Welch's test
wmodel_omnibus_polarcheck <- oneway.test(pol_check_scale ~ as.factor(polar_cond), data = polarcond_polarcheck,
                              var.equal = FALSE)
wmodel_omnibus_polarcheck

# 4.1d. Games-howell adjusted pairwise comparisons

library(userfriendlyscience)
library(MBESS)

gh.polarcheck_scale <- oneway(polarcond_polarcheck$polar_cond, y = polarcond_polarcheck$pol_check_scale, posthoc = 'games-howell')
gh.polarcheck_scale


# effect sizes for the adjusted pairwise comparisons

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
#     we know there are unequal variances across groups here
t.test(pols1_abbrev2$brtrust_check_scale~pols1_abbrev2$broken_cond, var.equal = FALSE) # where y is numeric and x is a binary factor


cohensD(x = brtrust_check_scale ~ broken_cond, data = pols1_abbrev2, method = "unequal")


#5. ANALYSES OF THE DV IN ANOVA WITH CONTRASTS (AS IN THE MANUSCRIPT) ----

# Question 1: Do the level of polarization (low, medium, high) between two fictional candidates AND 
#             the level of brokenness in an electoral system  (broken v not broken) 
#             exert unique effects on desire for inclusion of a third party or idependent candidate?

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

# 5.3a. Primary DV = desire

# check for homogeneity of variance across the 6 conditions for the primary DV: desire_scale

polarbroken_desire <- pols1_abbrev2 %>%
  dplyr::select(desire_scale, brokenpol_cond_fact) %>%
  filter(brokenpol_cond_fact != "NA" & desire_scale > -99)

with(polarbroken_desire, lawstat::levene.test(desire_scale, as.factor(brokenpol_cond_fact),location="mean"))

# desire DV omnibus test
amodel_omnibus_desire <- aov(desire_scale ~ brokenpol_cond_fact, data = pols1_abbrev2)
summary(amodel_omnibus_desire)

# the below provides the estimates for the two contrasts and the omnibus model
# Make sure to use summary.aov here or 'split' might not work
# make sure the contrasts are listed in the same order they appear in the matrix!
summary.aov(amodel_omnibus_desire, split=list(brokenpol_cond_fact=list("Code 1: contrast_bvnb" = 1,"Code 2: contrast_lhvm" = 2, 
                                                                       "Code 3: contrast_lvh" = 3, "Code 4: contrast_blhvm" = 4,
                                                                       "Code 5: contrast_blvh" = 5)))

# testing the separate/isolated effect of polarization condition on desire
amodel_omnibus_desireb <- aov(desire_scale ~ polar_cond, data = pols1_abbrev2)
summary.aov(amodel_omnibus_desireb, split=list(polar_cond=list("Code 2: contrast_lhvm" = 1, "Code 3: contrast_lvh" = 2)))

pols1_abbrev2 %>%
  group_by(polar_cond) %>%
  summarize(
    mean(desire_scale, na.rm = TRUE),
    sd(desire_scale, na.rm = TRUE),
    n())


# effect sizes for the contrasts: contrast correlation (Rosnow, Rosenthal, & Rubin, 2000)

# the below provides the partial correlation between scores on the outcome variable
# and the lambdas associated with the groups, after eliminating all between-group non-contrast variation

# rcontrast = sqrt(F or t^2)/sqrt(F or t^2 + df_within)

# Broken versus not-broken contrast

sqrt(12.205)/sqrt(12.205 + 867)

# High and low versus medium contrast

sqrt(2.360)/sqrt(2.360 + 867)

# High versus low contrast

sqrt(0.389)/sqrt(0.389  + 867)


# 5.3b. DV = lack of choice ----

# Q. Is measured lack of choice a result of polarization and/or brokenness?

# check for homogeneity of variance across the 6 conditions for the 5 lack of choice items

polarbroken_nochoice12 <- pols1_abbrev2 %>%
  dplyr::select(nochoice12, brokenpol_cond_fact) %>%
  filter(brokenpol_cond_fact != "NA" & nochoice12 > -99)

with(polarbroken_nochoice12, lawstat::levene.test(nochoice12, as.factor(brokenpol_cond_fact),location="mean"))

# we know we have unequal variances, so we will need to report Welch's omnibus test and adjusted contrasts
# Welch's test for non-homogeneity
wmodel_omnibus_nochoice12 <- oneway.test(nochoice12 ~ as.factor(brokenpol_cond_fact), data = polarbroken_nochoice12,
                                         var.equal = FALSE)
wmodel_omnibus_nochoice12


# analyses for adjusted contrasts
# export to csv to work on separately: for some reason the code does not work in this script but work separately

pols1_abbrev2 %>%
  dplyr::select(polar_cond, broken_cond, brokenpol_cond_fact, nochoice12, nochoice3, nochoice4, nochoice5) %>%
  write.csv("/Users/gabriellechristinemartin/Desktop/R_Pol_data_scripts/Supplement_S7toS10_adjustedcontrasts/S8_adjustedcontrasts/S8_simcomp.csv", row.names = TRUE)

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

polarbroken_nochoice12 <- pols1_abbrev2 %>%
  dplyr::select(nochoice12, brokenpol_cond_fact) %>%
  filter(brokenpol_cond_fact != "NA" & nochoice12 > -99)

SimTestDiff(data = polarbroken_nochoice12, grp="brokenpol_cond_fact", resp="nochoice12", ContrastMat = mat2,
            covar.equal = FALSE)



# separate analyses by nochoice3, nochoice4, nochoice5

polarbroken_nochoice3 <- pols1_abbrev2 %>%
  dplyr::select(nochoice3, brokenpol_cond_fact) %>%
  filter(brokenpol_cond_fact != "NA" & nochoice3 > -99)

with(polarbroken_nochoice3, lawstat::levene.test(nochoice3, as.factor(brokenpol_cond_fact),location="mean"))

amodel_omnibus_nochoice3 <- aov(nochoice3 ~ brokenpol_cond_fact, data = pols1_abbrev2)
summary(amodel_omnibus_nochoice3)

summary.aov(amodel_omnibus_nochoice3, split=list(brokenpol_cond_fact=list("Code 1: contrast_bvnb" = 1,"Code 2: contrast_lhvm" = 2, 
                                                                          "Code 3: contrast_lvh" = 3, "Code 4: contrast_blhvm" = 4,
                                                                          "Code 5: contrast_blvh" = 5))) 

# rcontrast = sqrt(F or t^2)/sqrt(F or t^2 + df_within)

# Broken versus not-broken contrast

sqrt(14.366)/sqrt(14.366 + 760)

# High and low versus medium contrast

sqrt(0.561)/sqrt(0.561 + 760)

# High versus low contrast

sqrt(1.546)/sqrt(1.546  + 760)


polarbroken_nochoice4 <- pols1_abbrev2 %>%
  dplyr::select(nochoice4, brokenpol_cond_fact) %>%
  filter(brokenpol_cond_fact != "NA" & nochoice4 > -99)

with(polarbroken_nochoice4, lawstat::levene.test(nochoice4, as.factor(brokenpol_cond_fact),location="mean"))

# Welch's test for non-homogeneity
wmodel_omnibus_nochoice4 <- oneway.test(nochoice4 ~ as.factor(brokenpol_cond_fact), data = polarbroken_nochoice4,
                                         var.equal = FALSE)
wmodel_omnibus_nochoice4

amodel_omnibus_nochoice4 <- aov(nochoice4 ~ brokenpol_cond_fact, data = pols1_abbrev2)
summary.aov(amodel_omnibus_nochoice4, split=list(brokenpol_cond_fact=list("Code 1: contrast_bvnb" = 1,"Code 2: contrast_lhvm" = 2, 
                                                                          "Code 3: contrast_lvh" = 3, "Code 4: contrast_blhvm" = 4,
                                                                          "Code 5: contrast_blvh" = 5))) 

# adjust contrasts for heteroscedasticity

# filter out missing data prior to analysis

polarbroken_nochoice4 <- pols1_abbrev2 %>%
  dplyr::select(nochoice4, brokenpol_cond_fact) %>%
  filter(brokenpol_cond_fact != "NA" & nochoice4 > -99)

SimTestDiff(data = polarbroken_nochoice4, grp="brokenpol_cond_fact", resp="nochoice4", ContrastMat = mat2,
            covar.equal = FALSE)




polarbroken_nochoice5 <- pols1_abbrev2 %>%
  dplyr::select(nochoice5, brokenpol_cond_fact) %>%
  filter(brokenpol_cond_fact != "NA" & nochoice5 > -99)

with(polarbroken_nochoice5, lawstat::levene.test(nochoice5, as.factor(brokenpol_cond_fact),location="mean"))

# Welch's test for non-homogeneity
wmodel_omnibus_nochoice5 <- oneway.test(nochoice5 ~ as.factor(brokenpol_cond_fact), data = polarbroken_nochoice5,
                                        var.equal = FALSE)
wmodel_omnibus_nochoice5

amodel_omnibus_nochoice5 <- aov(nochoice5 ~ brokenpol_cond_fact, data = pols1_abbrev2)
summary.aov(amodel_omnibus_nochoice2, split=list(brokenpol_cond_fact=list("Code 1: contrast_bvnb" = 1,"Code 2: contrast_lhvm" = 2, 
                                                                          "Code 3: contrast_lvh" = 3, "Code 4: contrast_blhvm" = 4,
                                                                          "Code 5: contrast_blvh" = 5))) 

# adjust contrasts for heteroscedasticity

# filter out missing data prior to analysis

polarbroken_nochoice5 <- pols1_abbrev2 %>%
  dplyr::select(nochoice5, brokenpol_cond_fact) %>%
  filter(brokenpol_cond_fact != "NA" & nochoice5 > -99)

SimTestDiff(data = polarbroken_nochoice5, grp="brokenpol_cond_fact", resp="nochoice5", ContrastMat = mat2,
            covar.equal = FALSE)



#6. Testing differences between all row-wise cell means in the table ----

B1 <- pols1_abbrev2 %>%
  filter(broken_cond == "Broken")

broken_bypolarcond_B1 <- oneway(B1$polar_cond, y = B1$brtrust_check_scale, posthoc = 'tukey')
broken_bypolarcond_B1

desire_bypolarcond_B1 <- oneway(B1$polar_cond, y = B1$desire_scale, posthoc = 'tukey')
desire_bypolarcond_B1

nochoice12_bypolarcond_B1 <- oneway(B1$polar_cond, y = B1$nochoice12, posthoc = 'tukey')
nochoice12_bypolarcond_B1

nochoice3_bypolarcond_B1 <- oneway(B1$polar_cond, y = B1$nochoice3, posthoc = 'tukey')
nochoice3_bypolarcond_B1

nochoice4_bypolarcond_B1 <- oneway(B1$polar_cond, y = B1$nochoice4, posthoc = 'tukey')
nochoice4_bypolarcond_B1

nochoice5_bypolarcond_B1 <- oneway(B1$polar_cond, y = B1$nochoice5, posthoc = 'tukey')
nochoice5_bypolarcond_B1



B2 <- pols1_abbrev2 %>%
  filter(broken_cond == "Not Broken")

broken_bypolarcond_B2 <- oneway(B2$polar_cond, y = B2$brtrust_check_scale, posthoc = 'tukey')
broken_bypolarcond_B2

desire_bypolarcond_B2 <- oneway(B2$polar_cond, y = B2$desire_scale, posthoc = 'tukey')
desire_bypolarcond_B2

nochoice12_bypolarcond_B2 <- oneway(B2$polar_cond, y = B2$nochoice12, posthoc = 'tukey')
nochoice12_bypolarcond_B2

nochoice3_bypolarcond_B2 <- oneway(B2$polar_cond, y = B2$nochoice3, posthoc = 'tukey')
nochoice3_bypolarcond_B2

nochoice4_bypolarcond_B2 <- oneway(B2$polar_cond, y = B2$nochoice4, posthoc = 'tukey')
nochoice4_bypolarcond_B2

nochoice5_bypolarcond_B2 <- oneway(B2$polar_cond, y = B2$nochoice5, posthoc = 'tukey')
nochoice5_bypolarcond_B2



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


# ADD DEMAND AS A COVARIATE IN REGRESSION ----

library(QuantPsyc)

#6.3b. model with the 5 contrast codes predicting desire
rmodel_desire_b <- lm(desire_scale ~ code1+code2+code3+code4+code5+exptdemand_scale, data = pols1_abbrev2)
summary(rmodel_desire_b)
lm.beta(rmodel_desire_b)

# condition moderated by demand: multiply the 5 contrast codes by experimenter bias 
# and enter them together in a model predicting the outcomes; 11 terms in total. 

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

rmodel_brokentr_c <- lm(brtrust_check_scale ~ code1+code2+code3+code4+code5+demand_C+
                        code1*demand_C+code2*demand_C+code3*demand_C+
                        code4*demand_C+code5*demand_C, data = pols1_abbrev2)
summary(rmodel_brokentr_c)
lm.beta(rmodel_brokentr_c)

# this method does not first center the main effect and I assume the multicollinearity
# washes out the effect of brokenness condition
rmodel_desire_d <- lm(desire_scale ~ code1+code2+code3+code4+code5+exptdemand_scale+
                        code1*exptdemand_scale+code2*exptdemand_scale+code3*exptdemand_scale+
                        code4*exptdemand_scale+code5*exptdemand_scale, data = pols1_abbrev2)
summary(rmodel_desire_d)
lm.beta(rmodel_desire_d)




#8. SIMULTANEOUS STATISTICAL MEDIATION WITH BROKENNESS/DISTRUST SCALE AND THE 5 LACK OF CHOICE ITEMS ----

# Test lack of choice as an alternative mediator: 
#   1. Is the polarization effect on desire explained by perceived lack of choice? 
#   2. Do the five lack of choice items result from both low and high pol?
#   3. Does brokenness scale predict unique variance in desire, controlling for lack of choice? 

# We can compare the effects on and of brokenness (measured) to the effects 
# on and of perceived lack of choice; compare effect sizes.

# note: mediation models with multicategorical predictors have to be broken into dummy comparisons
# that compare to a reference group (here, the medium condition)
#   one model comparing low and medium conditions
#   one model comparing high and medium conditions
# Multicategorical predictor models with three levels of the IV produce:
#   two sets of coefficients each for the a-path, total effect (C-path), direct effect (C’-path), and indirect effect, 
#   and a single coefficient for the b-path.

# first, create the two contrasts comparing to low and, separately, high to medium condition

pols1_abbrev2 <- pols1_abbrev2 %>% 
  mutate(contrast_lowvmed = as.numeric(recode(
    polar_cond, 'Low Polarization' = 1, 
    'Medium Polarization' = 0))) %>%
  mutate(contrast_highvmed = as.numeric(recode(
    polar_cond, 'Medium Polarization' = 0, 
    'High Polarization' = 1)))


table(pols1_abbrev2$polar_cond)
table(pols1_abbrev2$contrast_lowvmed)
table(pols1_abbrev2$contrast_highvmed)

#creating the contrast codes to implement in the mediation model
contrast_lvm <- c(1, -1, 0) #low v medium, excluding high
contrast_hvm <- c(0, -1, 1) #high v medium, excluding low 

# combine the above 2 lines into a matrix
mat <- cbind(contrast_lvm, contrast_hvm)

# tell R that the matrix gives the contrasts you want
contrasts(pols1_abbrev2$polar_cond) <- mat


# the below provides the estimates for the two contrasts and the omnibus model
# Make sure to use summary.aov here or 'split' might not work
# make sure the contrasts are listed in the same order they appear in the matrix!
# these look pretty good!
summary.aov(amodel_omnibus_desire, split=list(polar_cond=list("Low vs. Medium"=1, 
                                                              "High vs Medium" = 2))) 
sqrt(16.646)

library(knitr)
library(lavaan)

# set random seed so results can be reproduced
set.seed(1234)

#   ANALYSIS WITH NOCHOICE1
# specifying the model paths

mod_lvm_1 <- "# a1 path
         brtrust_check_scale ~ a1 * contrast_lowvmed
         # a2 path
         nochoice1 ~ a2 * contrast_lowvmed

         # b1 path
         desire_scale ~ b1 * brtrust_check_scale
         # b2 path
         desire_scale ~ b2 * nochoice1

         # c prime path 
         desire_scale ~ cp * contrast_lowvmed

         # indirect and total effects
         ab1 := a1 * b1
         ab2 := a2 * b2
         total := cp + ab1 + ab2"

mod_hvm_1 <- "# a1 path
         brtrust_check_scale ~ a1 * contrast_highvmed
         # a2 path
         nochoice1 ~ a2 * contrast_highvmed

         # b1 path
         desire_scale ~ b1 * brtrust_check_scale
         # b2 path
         desire_scale ~ b2 * nochoice1

         # c prime path 
         desire_scale ~ cp * contrast_highvmed

         # indirect and total effects
         ab1 := a1 * b1
         ab2 := a2 * b2
         total := cp + ab1 + ab2"

# You must specify bootstrapping in the sem() function
# used 5000 bootstrapped samples in the paper

# fit models
sem_lvm_1 <- sem(mod_lvm_1, data = pols1_abbrev2, se = "bootstrap", bootstrap = 1000)

sem_hvm_1 <- sem(mod_hvm_1, data = pols1_abbrev2, se = "bootstrap", bootstrap = 1000)


# summarize models
# note: total effect = c path; cp = c prime (c'); ab = indirect effect

# df and estimates look a little off, but not by much 
summary(sem_lvm_1, standardized = TRUE)

# df and estimates look a little off, but not by much
summary(sem_hvm_1, standardized = TRUE)


# print all model parameters
parameterestimates(sem_lvm_1, boot.ci.type = "bca.simple", standardized = TRUE) %>% 
  kable()

parameterestimates(sem_hvm_1, boot.ci.type = "bca.simple", standardized = TRUE) %>% 
  kable()


#   ANALYSIS WITH NOCHOICE2
# specifying the model paths

mod_lvm_2 <- "# a1 path
         brtrust_check_scale ~ a1 * contrast_lowvmed
         # a2 path
         nochoice2 ~ a2 * contrast_lowvmed

         # b1 path
         desire_scale ~ b1 * brtrust_check_scale
         # b2 path
         desire_scale ~ b2 * nochoice2

         # c prime path 
         desire_scale ~ cp * contrast_lowvmed

         # indirect and total effects
         ab1 := a1 * b1
         ab2 := a2 * b2
         total := cp + ab1 + ab2"

mod_hvm_2 <- "# a1 path
         brtrust_check_scale ~ a1 * contrast_highvmed
         # a2 path
         nochoice2 ~ a2 * contrast_highvmed

         # b1 path
         desire_scale ~ b1 * brtrust_check_scale
         # b2 path
         desire_scale ~ b2 * nochoice2

         # c prime path 
         desire_scale ~ cp * contrast_highvmed

         # indirect and total effects
         ab1 := a1 * b1
         ab2 := a2 * b2
         total := cp + ab1 + ab2"

# You must specify bootstrapping in the sem() function
# used 5000 bootstrapped samples in the paper

# fit models
sem_lvm_2 <- sem(mod_lvm_2, data = pols1_abbrev2, se = "bootstrap", bootstrap = 1000)

sem_hvm_2 <- sem(mod_hvm_2, data = pols1_abbrev2, se = "bootstrap", bootstrap = 1000)


# summarize models
# note: total effect = c path; cp = c prime (c'); ab = indirect effect

# df and estimates look a little off, but not by much 
summary(sem_lvm_2, standardized = TRUE)

# df and estimates look a little off, but not by much
summary(sem_hvm_2, standardized = TRUE)


# print all model parameters
parameterestimates(sem_lvm_2, boot.ci.type = "bca.simple", standardized = TRUE) %>% 
  kable()

parameterestimates(sem_hvm_2, boot.ci.type = "bca.simple", standardized = TRUE) %>% 
  kable()


#   ANALYSIS WITH NOCHOICE3
# specifying the model paths

mod_lvm_3 <- "# a1 path
         brtrust_check_scale ~ a1 * contrast_lowvmed
         # a2 path
         nochoice3 ~ a2 * contrast_lowvmed

         # b1 path
         desire_scale ~ b1 * brtrust_check_scale
         # b2 path
         desire_scale ~ b2 * nochoice3

         # c prime path 
         desire_scale ~ cp * contrast_lowvmed

         # indirect and total effects
         ab1 := a1 * b1
         ab2 := a2 * b2
         total := cp + ab1 + ab2"

mod_hvm_3 <- "# a1 path
         brtrust_check_scale ~ a1 * contrast_highvmed
         # a2 path
         nochoice3 ~ a2 * contrast_highvmed

         # b1 path
         desire_scale ~ b1 * brtrust_check_scale
         # b2 path
         desire_scale ~ b2 * nochoice3

         # c prime path 
         desire_scale ~ cp * contrast_highvmed

         # indirect and total effects
         ab1 := a1 * b1
         ab2 := a2 * b2
         total := cp + ab1 + ab2"

# You must specify bootstrapping in the sem() function
# used 5000 bootstrapped samples in the paper

# fit models
sem_lvm_3 <- sem(mod_lvm_3, data = pols1_abbrev2, se = "bootstrap", bootstrap = 1000)

sem_hvm_3 <- sem(mod_hvm_3, data = pols1_abbrev2, se = "bootstrap", bootstrap = 1000)


# summarize models
# note: total effect = c path; cp = c prime (c'); ab = indirect effect

# df and estimates look a little off, but not by much 
summary(sem_lvm_3, standardized = TRUE)

# df and estimates look a little off, but not by much
summary(sem_hvm_3, standardized = TRUE)


# print all model parameters
parameterestimates(sem_lvm_3, boot.ci.type = "bca.simple", standardized = TRUE) %>% 
  kable()

parameterestimates(sem_hvm_3, boot.ci.type = "bca.simple", standardized = TRUE) %>% 
  kable()

#   ANALYSIS WITH NOCHOICE4
# specifying the model paths

mod_lvm_4 <- "# a1 path
         brtrust_check_scale ~ a1 * contrast_lowvmed
         # a2 path
         nochoice4 ~ a2 * contrast_lowvmed

         # b1 path
         desire_scale ~ b1 * brtrust_check_scale
         # b2 path
         desire_scale ~ b2 * nochoice4

         # c prime path 
         desire_scale ~ cp * contrast_lowvmed

         # indirect and total effects
         ab1 := a1 * b1
         ab2 := a2 * b2
         total := cp + ab1 + ab2"

mod_hvm_4 <- "# a1 path
         brtrust_check_scale ~ a1 * contrast_highvmed
         # a2 path
         nochoice4 ~ a2 * contrast_highvmed

         # b1 path
         desire_scale ~ b1 * brtrust_check_scale
         # b2 path
         desire_scale ~ b2 * nochoice4

         # c prime path 
         desire_scale ~ cp * contrast_highvmed

         # indirect and total effects
         ab1 := a1 * b1
         ab2 := a2 * b2
         total := cp + ab1 + ab2"

# You must specify bootstrapping in the sem() function
# used 5000 bootstrapped samples in the paper

# fit models
sem_lvm_4 <- sem(mod_lvm_4, data = pols1_abbrev2, se = "bootstrap", bootstrap = 1000)

sem_hvm_4 <- sem(mod_hvm_4, data = pols1_abbrev2, se = "bootstrap", bootstrap = 1000)


# summarize models
# note: total effect = c path; cp = c prime (c'); ab = indirect effect

# df and estimates look a little off, but not by much 
summary(sem_lvm_4, standardized = TRUE)

# df and estimates look a little off, but not by much
summary(sem_hvm_4, standardized = TRUE)


# print all model parameters
parameterestimates(sem_lvm_4, boot.ci.type = "bca.simple", standardized = TRUE) %>% 
  kable()

parameterestimates(sem_hvm_4, boot.ci.type = "bca.simple", standardized = TRUE) %>% 
  kable()


#   ANALYSIS WITH NOCHOICE5
# specifying the model paths

mod_lvm_5 <- "# a1 path
         brtrust_check_scale ~ a1 * contrast_lowvmed
         # a2 path
         nochoice5 ~ a2 * contrast_lowvmed

         # b1 path
         desire_scale ~ b1 * brtrust_check_scale
         # b2 path
         desire_scale ~ b2 * nochoice5

         # c prime path 
         desire_scale ~ cp * contrast_lowvmed

         # indirect and total effects
         ab1 := a1 * b1
         ab2 := a2 * b2
         total := cp + ab1 + ab2"

mod_hvm_5 <- "# a1 path
         brtrust_check_scale ~ a1 * contrast_highvmed
         # a2 path
         nochoice5 ~ a2 * contrast_highvmed

         # b1 path
         desire_scale ~ b1 * brtrust_check_scale
         # b2 path
         desire_scale ~ b2 * nochoice5

         # c prime path 
         desire_scale ~ cp * contrast_highvmed

         # indirect and total effects
         ab1 := a1 * b1
         ab2 := a2 * b2
         total := cp + ab1 + ab2"

# You must specify bootstrapping in the sem() function
# used 5000 bootstrapped samples in the paper

# fit models
sem_lvm_5 <- sem(mod_lvm_5, data = pols1_abbrev2, se = "bootstrap", bootstrap = 1000)

sem_hvm_5 <- sem(mod_hvm_5, data = pols1_abbrev2, se = "bootstrap", bootstrap = 1000)


# summarize models
# note: total effect = c path; cp = c prime (c'); ab = indirect effect

# df and estimates look a little off, but not by much 
summary(sem_lvm_5, standardized = TRUE)

# df and estimates look a little off, but not by much
summary(sem_hvm_5, standardized = TRUE)


# print all model parameters
parameterestimates(sem_lvm_5, boot.ci.type = "bca.simple", standardized = TRUE) %>% 
  kable()

parameterestimates(sem_hvm_5, boot.ci.type = "bca.simple", standardized = TRUE) %>% 
  kable()
