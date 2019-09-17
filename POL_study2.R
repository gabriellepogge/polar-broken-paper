# set the working directory
getwd()

setwd("/gabriellechristinemartin/Desktop/R_Pol_data_scripts/Paper Study 2")

# clear the environment
rm(list=ls())

library(tidyverse)
library(haven)

#1. IMPORTING, VIEWING, AND SELECTING THE DATA ----

#create data.frame called pols1 from the spss (.sav) file
pols1 <- data.frame(read_sav("/Users/gabriellechristinemartin/Desktop/R_Pol_data_scripts/Paper Study 2/POL_Expt3_original.sav"))

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
# dropped attsimilar from collection for this study
# make sure to include the mediator items: systembroken, healthysystem, electpointless, electwellfunct 

pols1_abbrev <- pols1 %>%
  select(X._task_id, desirepersonal, desireothers, desirebenefit, attdifferent, 
         systembroken, healthysystem, electpointless, electwellfunct,
         X._session_status, polorient, partyid, age, sex, education, ethnicityom, raceomb) %>%
      mutate(desirepersonal = as.numeric(desirepersonal)) %>%
      mutate(desireothers = as.numeric(desireothers)) %>%
      mutate(desirebenefit = as.numeric(desirebenefit)) %>%
      mutate(attdifferent = as.numeric(attdifferent)) %>%
      mutate(systembroken = as.numeric(systembroken)) %>%
      mutate(healthysystem = as.numeric(healthysystem)) %>%
      mutate(electpointless = as.numeric(electpointless)) %>%
      mutate(electwellfunct = as.numeric(electwellfunct)) %>%
      mutate(polorient = as.numeric(polorient)) %>%
      mutate(partyid = as.factor(partyid)) %>%
      mutate(education = as.factor(education)) %>%
      mutate(ethnicityom = as.factor(ethnicityom)) %>%
      mutate(raceomb = as.factor(raceomb)) %>%
  mutate(polar_cond = as.factor(case_when( 
        X._task_id == "manipinstmedpol" ~ 2,
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
table(pols1_abbrev$polar_cond)
class(pols1_abbrev$polar_cond)

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

  
#drops the now unnecessary string vars
#filters for Ps age 18+ and complete responses
#n, complete = 540
#droped Ps under 18 (n = 19): results in 529 responses

pols1_abbrev2 <- pols1_abbrev %>%
  select(desirepersonal, desireothers, desirebenefit, attdifferent, 
         systembroken, healthysystem, electpointless, electwellfunct, complete,
         polorient, partyid, age, gender, education, ethnicityom, raceomb, polar_cond) %>%
  mutate(complete_DV = (as.factor(ifelse(
        desirepersonal > -99 & desirebenefit > -99 & desireothers > -99, 1, NA)))) %>%
    filter(complete == 'P reached study end') %>%
    filter(age >= 18)


# what we should report is complete_DV & age > 18 (ignoring whether Ps got to the end)
#n, complete_DV = 590
#droped Ps under 18 (n = 18): results in 578 responses

pols1_abbrev2 <- pols1_abbrev %>%
  select(desirepersonal, desireothers, desirebenefit, attdifferent, 
         systembroken, healthysystem, electpointless, electwellfunct, complete,
         polorient, partyid, age, gender, education, ethnicityom, raceomb, polar_cond) %>%
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

# polarization manip check item; this time attdifferent only
# counts a little low on the high end of the scale for attdifferent

hist(pols1_abbrev2$attdifferent)

summary(pols1_abbrev2$attdifferent)
sd(pols1_abbrev2$attdifferent, na.rm = TRUE)

# mediator items: system brokenness
# distributions look pretty awful for systembroken and electpointless

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

# checking attdifferent means by condition
# matches paper, condition ns look good

pols1_abbrev2 %>%
group_by(polar_cond) %>%
  summarize(
      mean(attdifferent, na.rm = TRUE),
      sd(attdifferent, na.rm = TRUE),
      n())

#get the reliability for the three primary DV items: alpha = .83

desire_scale <- select(pols1_abbrev, desirepersonal, desireothers, desirebenefit)

library(psych)

psych::alpha(desire_scale)

# create the desire scale by averaging the three items
pols1_abbrev2 <- pols1_abbrev2 %>%
  mutate(desire_scale =
           (desirepersonal + desireothers + desirebenefit)/3) 
  
# checking desire_scale means by condition
# matches paper, condition ns look good

pols1_abbrev2 %>%
  group_by(polar_cond) %>%
  summarize(
    mean(desire_scale, na.rm = TRUE),
    sd(desire_scale, na.rm = TRUE),
    n())


#get the reliability for the four mediator items: alpha = .72

#reverse-code healthysystem and electwellfunct before combining with systembroken and electpointless

pols1_abbrev2 <- pols1_abbrev2 %>%
  mutate(healthysystem_r = 
           6 - healthysystem) %>%
  mutate(electwellfunct_r =
           6 - electwellfunct) %>%
  mutate(brokenness_scale =
           ((healthysystem_r + electwellfunct_r + systembroken + electpointless)/4)) 

table(pols1_abbrev2$healthysystem)
table(pols1_abbrev2$healthysystem_r)

table(pols1_abbrev2$electwellfunct)
table(pols1_abbrev2$electwellfunct_r)

summary(pols1_abbrev2$brokenness_scale)
sd(pols1_abbrev2$brokenness_scale, na.rm = TRUE)

glimpse(pols1_abbrev2)

brokensc <- select(pols1_abbrev2, healthysystem_r, systembroken, 
                       electpointless, electwellfunct_r)

psych::alpha(brokensc)

# checking brokenness_scale means by condition
# matches paper, condition ns look good

pols1_abbrev2 %>%
  group_by(polar_cond) %>%
  summarize(
    mean(brokenness_scale, na.rm = TRUE),
    sd(brokenness_scale, na.rm = TRUE),
    n())


#4. ANALYSES IN ANOVA: ATTDIFFERENT MANIPULATION CHECK (AS IN THE MANUSCRIPT) ----

# Polarization manipulation effectiveness: check for a positive linear effect of condition (from low to high)
#     on the manipulation check item: attdifferent

#checking the condition variable class and levels before analysis
class(pols1_abbrev2$polar_cond)
table(pols1_abbrev2$polar_cond)

#get summary info for the manipulation check scale, primary DV, and brokenness by condition
describeBy(pols1_abbrev2$attdifferent, pols1_abbrev2$polar_cond, mat=TRUE)
describeBy(pols1_abbrev2$desire_scale, pols1_abbrev2$polar_cond, mat=TRUE)
describeBy(pols1_abbrev2$brokenness_scale, pols1_abbrev2$polar_cond, mat=TRUE)

#check distribution of attdifferent and desirescale across condition levels

library(ggpubr)

ggboxplot(pols1_abbrev2, x = "polar_cond", y = "attdifferent", 
          color = "polar_cond", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("Low Polarization", "Medium Polarization", "High Polarization"),
          ylab = "Manip Check Scale", xlab = "Polarization Condition")

ggboxplot(pols1_abbrev2, x = "polar_cond", y = "desire_scale", 
          color = "polar_cond", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("Low Polarization", "Medium Polarization", "High Polarization"),
          ylab = "Desire Scale", xlab = "Polarization Condition")


#models predicting attdifferent

# car package appears to be unavailable although listed on CRAN
# can also get levene's test in lawstat package
library(lawstat)

# test whether the homogeniety of variance across levels assumption holds
# this code does not work if there is missing data!
# solution: first filter for missing values on the vectors passed to levene.test

table(pols1_abbrev2$polar_cond)

polarcond_polarcheck <- pols1_abbrev2 %>%
  select(attdifferent, polar_cond) %>%
  filter(polar_cond != "NA" & attdifferent > -99)

# polarcond_polarcheck, n = 552
with(polarcond_polarcheck, lawstat::levene.test(attdifferent, as.factor(polar_cond),location="mean"))

# we know we have unequal variances across levels 
# thus, need to report Welch's adjusted ANOVA and games-howell adjustment

# Welch's test
wmodel_omnibus_polarcheck <- oneway.test(attdifferent ~ as.factor(polar_cond), data = polarcond_polarcheck)
wmodel_omnibus_polarcheck

# adjusted pairwise comparisons
library(userfriendlyscience)
library(MBESS)
gh.manip_check_scale <- oneway(polarcond_polarcheck$polar_cond, y = polarcond_polarcheck$attdifferent, posthoc = 'games-howell')
gh.manip_check_scale

# effect sizes for the adjusted pairwise comparisons

library(lsr)

# Error: grouping factor must have exactly 2 levels
# Solution: create new dfs by filtering for the relevant conditions for comparison

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


# 5. PLOTTING THE EFFECT OF CONDITION ON ATTDIFFERENT ----

#this is terrible; too much overplotting to see anything because of the interval nature of the x axis
ggplot(pols1_abbrev2, aes(x = polar_cond, y = attdifferent)) +
      geom_point()

#geom_jitter corrects the overplotting
#geom_smooth adds a regression line that shows a clearly linear effect
#although, there is clearly a lot of variance in each condition judging by the spread between points
ggplot(pols1_abbrev2, aes(x = as.numeric(polar_cond), y = attdifferent)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE, na.rm = TRUE)

# y axis scale is wrong
# depicts an erroneous inverse quadratic effect, 
#rather than a pos linear effect from low to high pol
ggplot(pols1_abbrev2, aes(x = polar_cond, y = mean(attdifferent, na.rm = TRUE))) +
  geom_col()

#y axis still wrong, but better! We now see the pos. linear effect
# axis runs 0-3 instead of 1-5
pols1_abbrev2 %>%
        group_by(polar_cond) %>%
        summarize(avg_check = mean(attdifferent, na.rm = TRUE)) %>%
        ggplot(aes(x = polar_cond, y = avg_check)) + 
                 geom_col()

#fixing the y axis to represent the range of attdifferent
#changing from bars to single dots representing the mean
# now we see the positive linear effect from low to high!
pols1_abbrev2 %>%
  group_by(polar_cond) %>%
  summarize(avg_check = mean(attdifferent, na.rm = TRUE)) %>%
  ggplot(aes(x = polar_cond, y = avg_check)) + 
  geom_point() +
  scale_y_continuous(limits = c(1, 5)) 


#6. ANALYSES IN ANOVA: DV = DESIRE SCALE (AS IN THE MANUSCRIPT) ----

# Question 1: Does the level of polarization between two fictional candidates affect 
#             desire for inclusion of a third party or idependent candidate?
# Q1a. Do people in the low AND high polarization conditions report greater desire compared to
#     people in the medium condition?
# Q1b. Do people in the low polarization and high polarization conditions (excluding medium condition)  
#     report equal levels of desire?


# test whether the homogeniety of variance across levels assumption holds
# this code does not work if there is missing data!
# solution: first filter for missing values on the vectors passed to levene.test

polarcond_desire <- pols1_abbrev2 %>%
  select(desire_scale, polar_cond) %>%
  filter(polar_cond != "NA" & desire_scale > -99)

# polarcond_desire, n = 578
with(polarcond_desire, lawstat::levene.test(desire_scale, as.factor(polar_cond),location="mean"))


# run an omnibus ANOVA for the primary DV = desire scale by condition
#df and F look good!

amodel_omnibus_desire <- aov(desire_scale ~ polar_cond, data = pols1_abbrev2)
summary(amodel_omnibus_desire)

#creating the contrast codes to implement in the ANOVA 
contrast_hvl <- c(-1, 0, 1) #high v low, excluding medium
contrast_hlvm <- c(1, -2, 1) #high and low versus medium

# combine the above 2 lines into a matrix
mat <- cbind(contrast_hvl, contrast_hlvm)

# tell R that the matrix gives the contrasts you want
contrasts(pols1_abbrev2$polar_cond) <- mat


# the below provides the estimates for the two contrasts and the omnibus model
# Make sure to use summary.aov here or 'split' might not work
# make sure the contrasts are listed in the same order they appear in the matrix!
# these look pretty good!
summary.aov(amodel_omnibus_desire, split=list(polar_cond=list("High vs. Low"=2, 
                                           "High & Low vs Medium" = 1))) 


# effect sizes for the contrasts: contrast correlation (Rosnow, Rosenthal, & Rubin, 2000)

# the below provides the partial correlation between scores on the outcome variable
# and the lambdas associated with the groups, after eliminating all between-group non-contrast variation

# rcontrast = sqrt(F or t^2)/sqrt(F or t^2 + df_within)

# High and low versus medium contrast

sqrt(16.646)/sqrt(16.646 + 575)

# High versus low contrast

sqrt(.0001)/sqrt(.0001 + 575)



# 7. PLOTTING THE EFFECT OF CONDITION ON DESIRE SCALE ----


#this is terrible; too much overplotting to see anything because of the interval nature of the x axis
ggplot(pols1_abbrev2, aes(x = polar_cond, y = desire_scale)) +
  geom_point()

# geom_jitter corrects the overplotting
# aclearly a lot of variance in each condition judging by the spread between points
# we see the quadratic effect with geom_smooth: 
# desire scale is higher in the low and high polarization conditions compared to the medium pol cond
ggplot(pols1_abbrev2, aes(x = as.numeric(polar_cond), y = desire_scale)) +
  geom_jitter() +
  geom_smooth(method = "auto", se = FALSE, na.rm = TRUE)

# y axis is wrong, runs 0-3 instead of 1-5
# but, we can still see the quadratic effect a bit clearer here
pols1_abbrev2 %>%
  group_by(polar_cond) %>%
  summarize(avg_desire = mean(desire_scale, na.rm = TRUE)) %>%
  ggplot(aes(x = polar_cond, y = avg_desire)) + 
  geom_col()

#fixing the y axis to represent the range of desire_scale
#changing from bars to single dots representing the mean
# now we see the quadratic effect of condition!
pols1_abbrev2 %>%
  group_by(polar_cond) %>%
  summarize(avg_desire = mean(desire_scale, na.rm = TRUE)) %>%
  ggplot(aes(x = as.numeric(polar_cond), y = avg_desire)) + 
  geom_point() +
  scale_y_continuous(limits = c(1, 5)) 

#examine the distributions of desire by condition with boxplots
pols1_abbrev2 %>%
    ggplot(aes(x = polar_cond, y = desire_scale)) +
  # add jittered points before the boxplot geometry  
  geom_jitter(alpha = 0.3, color = "steelblue") +
  # make boxplot transparent with alpha = 0  
  geom_boxplot(alpha = 0) +
    labs(title = "Distribution of desire scale DV by condition")

#an alternative view of distributions of the DV by condition: ridgeline plot

library(ggridges)

pols1_abbrev2 %>%
  #polar_cond is already a factor with defined levels
  #otherwise, we would want to mutate and define as factor with levels
  #note we have flipped the x and y aesthetics here
  ggplot(aes(x = desire_scale, y = polar_cond)) +
  
  geom_point(
    # make semi-transparent with alpha = 0.2
    alpha = 0.2,
    # turn points to vertical lines with shape = '|'
    shape = '|',
    # nudge the points downward by 0.05
    position = position_nudge(y = -0.05)
  ) +
  
  geom_density_ridges(bandwidth = 0.25, alpha = 0.7) +
  # add limits of 1 to 5 to x-scale
  # set expand values to c(0,0) to get rid of the extra space that ggplot puts around the extremes
  # of the data to avoid the awkward empty strip on the right and left where the densities don't interpolate.
  scale_x_continuous(limits = c(1, 5), expand = c(0, 0)) +
# provide subtitle with bandwidth
labs(subtitle = 'Gaussian kernel SD = 0.25') +
  theme(axis.ticks.y = element_blank())


#8. ANALYSES IN ANOVA: MEDIATOR = BROKEN SCALE (AS IN THE MANUSCRIPT) ----

# Question 2: Does the level of polarization between two fictional candidates affect 
#             perceived brokenness in the electoral system?
# Q2a. Do people in the low AND high polarization conditions report greater perceived brokenness compared to
#     people in the medium condition?
# Q2b. Do people in the low polarization and high polarization conditions (excluding medium condition)  
#     report equal levels of perceived brokenness?

# test whether the homogeniety of variance across levels assumption holds
# this code does not work if there is missing data!
# solution: first filter for missing values on the vectors passed to levene.test

polarcond_broken <- pols1_abbrev2 %>%
  select(brokenness_scale, polar_cond) %>%
  filter(polar_cond != "NA" & brokenness_scale > -99)

# polarcond_broken, n = 567
with(polarcond_broken, lawstat::levene.test(brokenness_scale, as.factor(polar_cond),location="mean"))

# run an omnibus ANOVA for the mediator = brokenness by condition
#df and F look good!

amodel_omnibus_brokenness <- aov(brokenness_scale ~ polar_cond, data = pols1_abbrev2)
summary(amodel_omnibus_brokenness)

# using the contrasts defined above

# the below provides the estimates for the two contrasts and the omnibus model
# Make sure to use summary.aov here or 'split' might not work
# make sure the contrasts are listed in the same order they appear in the matrix!
# Fs are a little off but otherwise these look pretty good!
summary.aov(amodel_omnibus_brokenness, split=list(polar_cond=list("High vs. Low"=1, 
                                                              "High & Low vs Medium" = 2))) 

# effect sizes for the contrasts: contrast correlation (Rosnow, Rosenthal, & Rubin, 2000)

# the below provides the partial correlation between scores on the outcome variable
# and the lambdas associated with the groups, after eliminating all between-group non-contrast variation

# rcontrast = sqrt(F or t^2)/sqrt(F or t^2 + df_within)

# High and low versus medium contrast

sqrt(17.898)/sqrt(17.898 + 564)

# High versus low contrast

sqrt(4.767)/sqrt(4.767 + 564)


# 9. PLOTTING THE EFFECT OF CONDITION ON BROKENNESS SCALE ----


#this is terrible; too much overplotting to see anything because of the interval nature of the x axis
ggplot(pols1_abbrev2, aes(x = polar_cond, y = brokenness_scale)) +
  geom_point()

# geom_jitter corrects the overplotting
# aclearly a lot of variance in each condition judging by the spread between points
# we see the quadratic effect with geom_smooth: 
# brokenness scale is higher in the low and high polarization conditions compared to the medium pol cond
ggplot(pols1_abbrev2, aes(x = as.numeric(polar_cond), y = brokenness_scale)) +
  geom_jitter() +
  geom_smooth(method = "auto", se = FALSE, na.rm = TRUE)

# y axis is wrong, runs 0-3 instead of 1-5
# but, we can still see the quadratic effect a bit clearer here
pols1_abbrev2 %>%
  group_by(polar_cond) %>%
  summarize(avg_brokenness = mean(brokenness_scale, na.rm = TRUE)) %>%
  ggplot(aes(x = polar_cond, y = avg_brokenness)) + 
  geom_col()

#fixing the y axis to represent the range of brokenness_scale
#changing from bars to single dots representing the mean
# now we see the quadratic effect of condition!
pols1_abbrev2 %>%
  group_by(polar_cond) %>%
  summarize(avg_brokenness = mean(brokenness_scale, na.rm = TRUE)) %>%
  ggplot(aes(x = as.numeric(polar_cond), y = avg_brokenness)) + 
  geom_point() +
  scale_y_continuous(limits = c(1, 5)) 

#examine the distributions of brokenness_scale by condition with boxplots
pols1_abbrev2 %>%
  ggplot(aes(x = polar_cond, y = brokenness_scale)) +
  # add jittered points before the boxplot geometry  
  geom_jitter(alpha = 0.3, color = "steelblue") +
  # make boxplot transparent with alpha = 0  
  geom_boxplot(alpha = 0) +
  labs(title = "Distribution of brokenness scale mediator by condition")



#10. ANALYSES IN REGRESSION ----

#recode polar_cond into two new variables to represent the contrast codes
#lowvhigh comparing the low and high polarization conditions
#lowhighvmed comparing the low AND high polarization conditions together to the medium condition

pols1_abbrev2 <- pols1_abbrev2 %>% 
  mutate(contrast_lowvhigh = as.numeric(recode(
    polar_cond, 'Low Polarization' = -1, 
                'Medium Polarization' = 0, 
                'High Polarization' = 1))) %>%
  mutate(contrast_lowhighvmed = as.numeric(recode(
    polar_cond, 'Low Polarization' = 1, 
                'Medium Polarization' = -2, 
                'High Polarization' = 1)))

glimpse(pols1_abbrev2)
class(contrast_hvl)
class(contrast_hlvm)

#checking to make sure the ns in each contrast match the condition ns after re-coding

table(pols1_abbrev2$polar_cond)
table(pols1_abbrev2$contrast_lowvhigh)
table(pols1_abbrev2$contrast_lowhighvmed)

# model predicting attdifferent (manipulation check)
# F and t values looks off here
rmodel_linear_check <- lm(attdifferent ~ as.numeric(polar_cond), data = pols1_abbrev2)
summary(rmodel_linear_check)

# models predicting DV = desire

#first, check the linear effect on desire. No effect, as expected.
rmodel_linear_desire <- lm(desire_scale ~ as.numeric(polar_cond), data = pols1_abbrev2)
summary(rmodel_linear_desire)

# contrast between low and high pol, excluding medium pol
#looks good!
rmodel_lvh_desire <- lm(desire_scale ~ contrast_lowvhigh, data = pols1_abbrev2)
summary(rmodel_lvh_desire)

# contrast between low and high pol together versus medium pol
#looks good!
rmodel_lhvm_desire <- lm(desire_scale ~ contrast_lowhighvmed, data = pols1_abbrev2)
summary(rmodel_lhvm_desire)

# models predicting mediator = brokenness

#first, check the linear effect on desire. No effect, as expected.
rmodel_linear_brokenness <- lm(brokenness_scale ~ as.numeric(polar_cond), data = pols1_abbrev2)
summary(rmodel_linear_brokenness)

# contrast between low and high pol, excluding medium pol
#looks good!
rmodel_lvh_brokenness <- lm(brokenness_scale ~ contrast_lowvhigh, data = pols1_abbrev2)
summary(rmodel_lvh_brokenness)

# contrast between low and high pol together versus medium pol
#looks good!
rmodel_lhvm_brokenness <- lm(brokenness_scale ~ contrast_lowhighvmed, data = pols1_abbrev2)
summary(rmodel_lhvm_brokenness)

#11. MEDIATION ANALYSIS OF THE INDIRECT EFFECT OF POL COND ON DESIRE THROUGH BROKENNESS ----

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
  mutate(contrast_lowvmed = as.numeric(recode(
    polar_cond, 'Low Polarization' = 1, 
    'Medium Polarization' = 0))) %>%
  mutate(contrast_highvmed = as.numeric(recode(
    polar_cond, 'Medium Polarization' = 0, 
    'High Polarization' = 1)))

pols1_abbrev2 <- pols1_abbrev2 %>% 
  mutate(contrast_lowvmed = as.numeric(case_when(
    polar_cond == 'Low Polarization' ~ 1, 
    polar_cond == 'Medium Polarization' ~ 0))) %>%
  mutate(contrast_highvmed = as.numeric(case_when(
    polar_cond == 'Medium Polarization' ~ 0, 
    polar_cond == 'High Polarization' ~ 1)))


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


library(knitr)
library(lavaan)

# specifying the model paths

mod_lvm <- "# a path
         brokenness_scale ~ a * contrast_lowvmed

         # b path
         desire_scale ~ b * brokenness_scale

         # c prime path 
         desire_scale ~ cp * contrast_lowvmed

         # indirect and total effects
         ab := a * b
         total := cp + ab"

mod_hvm <- "# a path
         brokenness_scale ~ a * contrast_highvmed

         # b path
         desire_scale ~ b * brokenness_scale

         # c prime path 
         desire_scale ~ cp * contrast_highvmed

         # indirect and total effects
         ab := a * b
         total := cp + ab"


# set random seed so results can be reproduced
set.seed(1234)

# You must specify bootstrapping in the sem() function
# used 5000 bootstrapped samples in the paper

# fit models
sem_lvm <- sem(mod_lvm, data = pols1_abbrev2, se = "bootstrap", bootstrap = 5000)

sem_hvm <- sem(mod_hvm, data = pols1_abbrev2, se = "bootstrap", bootstrap = 5000)


# summarize models
# note: total effect = c path; cp = c prime (c'); ab = indirect effect

# df and estimates look a little off, but not by much 
summary(sem_lvm, standardized = TRUE)

# df and estimates look a little off, but not by much
summary(sem_hvm, standardized = TRUE)


# print all model parameters
parameterestimates(sem_lvm, boot.ci.type = "bca.simple", standardized = TRUE) %>% 
  kable()

parameterestimates(sem_hvm, boot.ci.type = "bca.simple", standardized = TRUE) %>% 
  kable()




