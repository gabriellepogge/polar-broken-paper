# set the working directory
getwd()

setwd("/gabriellechristinemartin/Desktop/R_Pol_data_scripts/Paper Study 1")

# clear the environment
rm(list=ls())

library(tidyverse)

#1. IMPORTING, VIEWING, AND SELECTING THE DATA ----

library(haven)

#create data.frame called pols1 from the spss (.sav) file
pols1 <- data.frame(read_sav("/Users/gabriellechristinemartin/Desktop/R_Pol_data_scripts/Paper Study 1/POL_Expt2_original.sav"))

#returns class: data.frame
class(pols1)

#provides the structure of the data: number observations, number variables, variable names, variable type/classes 
#plus a preview of the data
glimpse(pols1)

library(naniar)

# tidyr::replace_na: Missing values turns into a value (NA –> -99)
# naniar::replace_with_na: Value becomes a missing value (-99 –> NA)
#Use replace_with_na_all() when you want to replace ALL values that meet a condition across an entire dataset. 
#The syntax here is a little different, and follows the rules for rlang’s expression of simple functions. 
#This means that the function starts with ~, and when referencing a variable, you use .x.
pols1 <- pols1 %>%
  replace_with_na_all(condition = ~.x == "")


library(visdat)

# There are two main functions in the visdat package:
# vis_dat: vis_dat visualises the whole dataframe at once, and provides information about the class 
#           of the data input into R, as well as whether the data is missing or not.
# vis_miss: The function vis_miss provides a summary of whether the data is missing or not. 
#           It also provides the amount of missings in each columns.

# One approach to visualising missing data comes from ggobi and manet, 
#   where we replace “NA” values with values 10% lower than the minimum value in that variable.
# This process is performed and visualised for you with the geom_miss_point() ggplot2 geom. 
# Being a proper ggplot geom, it supports all of the standard features of ggplot2, such as facets and themes.
# Another approach to visualising the missings in a dataset is to use the gg_miss_var plot:
#     gg_miss_var(data)
# The plots created with the gg_miss family all have a basic theme, but you can customise them.
# It is important to note that for every visualisation of missing data in naniar, there is an accompanying function to get the dataframe of the plot out. 
# This is important as the plot should not return a dataframe - but we also need to make the data available for use by the user so that it isn’t locked into a plot. 
# You can find these summary plots below, with miss_var_summary providing the dataframe that gg_miss_var() is based on.



#pulls out all rows for only the columns in pols1 that we need to examine the primary analyses and report demographics
#sets all vars to numeric except for the factor vars
#recodes the string condition var to factor
#recodes the string indicator of whether the participant got to the end of the study: complete
#recodes string sex to factor gender

pols1_abbrev <- pols1 %>%
  select(condition, desirepersonal, desireothers, desirebenefit, attsimilar, attdifferent, X._session_status, 
         polorient, partyid, age, sex, education, ethnicityom, raceomb) %>%
      mutate(desirepersonal = as.numeric(desirepersonal)) %>%
      mutate(desireothers = as.numeric(desireothers)) %>%
      mutate(desirebenefit = as.numeric(desirebenefit)) %>%
      mutate(attsimilar = as.numeric(attsimilar)) %>%
      mutate(attdifferent = as.numeric(attdifferent)) %>%
      mutate(polorient = as.numeric(polorient)) %>%
      mutate(partyid = as.factor(partyid)) %>%
      mutate(education = as.factor(education)) %>%
      mutate(ethnicityom = as.factor(ethnicityom)) %>%
      mutate(raceomb = as.factor(raceomb)) %>%
  mutate(polar_cond = as.factor(case_when( 
          condition == "manipinstmedpol" ~ 2,
          condition ==  "manipinsthighpol" ~ 3,
          condition ==  "manipinstlowpol" ~ 1))) %>%
      mutate(complete = as.factor(case_when(
        X._session_status == "C" ~ 1,
        X._session_status == "null" ~ 0))) %>%
      mutate(gender = as.factor(case_when(
        sex == "m" ~ 1,
        sex == "f" ~ 2))) 

glimpse(pols1_abbrev)

#checking to make sure the ns in each factor level are the same after re-coding into new var

table(pols1_abbrev$condition)
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

# What we currently report in the paper: filters for complete cases and for Ps age 18+ 
#n, complete = 545
#droped Ps under 18 (n = 20): results in 525 responses
#Drops the now unnecessary string vars

pols1_abbrev2 <- pols1_abbrev %>%
  select(desirepersonal, desireothers, desirebenefit, attsimilar, attdifferent, complete, 
         polorient, partyid, age, gender, education, ethnicityom, raceomb, polar_cond) %>%
  filter(complete == "P reached study end") %>%
  filter(age >= 18)

# what we should report is complete_DV (responses present for all three DV items) & age > 18 (ignoring whether Ps got to the end)
# n, complete_DV = 656
#droped Ps under 18 (n = 26): results in 630 responses

pols1_abbrev2 <- pols1_abbrev %>%
  select(desirepersonal, desireothers, desirebenefit, attsimilar, attdifferent, complete, 
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
# counts a little low on the high end of the scale for desireothers and attdifferent

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

# polarization manip check items
hist(pols1_abbrev2$attsimilar)
hist(pols1_abbrev2$attdifferent)

summary(pols1_abbrev2$attsimilar)
sd(pols1_abbrev2$attsimilar, na.rm = TRUE)

summary(pols1_abbrev2$attdifferent)
sd(pols1_abbrev2$attdifferent, na.rm = TRUE)

#3. CHECKING FOR SCALE RELIABILITY AND CREATING SCALES ----

# get the correlation between the two polarization manipulation check items: r(588) = -.60
cor.test(pols1_abbrev2$attsimilar, pols1_abbrev2$attdifferent)

#reverse-code attsimilar before combining with attdifferent

pols1_abbrev2 <- pols1_abbrev2 %>%
      mutate(attsimilar_r = 
              6 - attsimilar) %>%
      mutate(manip_check_scale =
              ((attsimilar_r + attdifferent)/2)) 

table(pols1_abbrev2$attsimilar)
table(pols1_abbrev2$attsimilar_r)

summary(pols1_abbrev2$manip_check_scale)
sd(pols1_abbrev2$manip_check_scale, na.rm = TRUE)

# checking manip_check_scale means by condition
#mostly matches paper, condition ns look good

pols1_abbrev2 %>%
group_by(polar_cond) %>%
  summarize(
      mean(manip_check_scale, na.rm = TRUE),
      sd(manip_check_scale, na.rm = TRUE),
      n())

#get the reliability for the three primary DV items: alpha = .80

desire_scale <- select(pols1_abbrev, desirepersonal, desireothers, desirebenefit)

library(psych)

psych::alpha(desire_scale)

pols1_abbrev2 <- pols1_abbrev2 %>%
  mutate(desire_scale =
           (desirepersonal + desireothers + desirebenefit)/3) 
  
# checking desire_scale means by condition
#mostly matches paper, condition ns look good

pols1_abbrev2 %>%
  group_by(polar_cond) %>%
  summarize(
    mean(desire_scale, na.rm = TRUE),
    sd(desire_scale, na.rm = TRUE),
    n())


#4. ANALYSES IN ANOVA: MANIPULATION CHECK SCALE (AS IN THE MANUSCRIPT) ----

# Question 1: Does the level of polarization between two fictional candidates affect 
#             desire for inclusion of a third party or idependent candidate?

# Polarization manipulation effectiveness: check for a positive linear effect of condition on
#     the manipulation check scale.

#checking the condition variable class and levels before analysis
class(pols1_abbrev2$polar_cond)
table(pols1_abbrev2$polar_cond)

#get summary info for the manipulation check scale and primary DV by condition
describeBy(pols1_abbrev2$manip_check_scale, pols1_abbrev2$polar_cond, mat=TRUE)
describeBy(pols1_abbrev2$desire_scale, pols1_abbrev2$polar_cond, mat=TRUE)

#check distribution of manip check scale and desirescale across condition levels

library(ggpubr)

ggboxplot(pols1_abbrev2, x = "polar_cond", y = "manip_check_scale", 
          color = "polar_cond", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("Low Polarization", "Medium Polarization", "High Polarization"),
          ylab = "Manip Check Scale", xlab = "Polarization Condition")

ggboxplot(pols1_abbrev2, x = "polar_cond", y = "desire_scale", 
          color = "polar_cond", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("Low Polarization", "Medium Polarization", "High Polarization"),
          ylab = "Desire Scale", xlab = "Polarization Condition")

# car package appears to be unavailable although listed on CRAN
# can also get levene's test in lawstat package
library(car)
library(lawstat)

# models predicting the manipulation check scale

# test whether the homogeniety of variance across levels assumption holds
# this code does not work if there is missing data!
# solution: first filter for missing values on the vectors passed to levene.test

table(pols1_abbrev2$polar_cond)

polarcond_polarcheck <- pols1_abbrev2 %>%
  select(manip_check_scale, polar_cond) %>%
  filter(polar_cond != "NA" & manip_check_scale > -99)

# polarcond_polarcheck, n = 590
with(polarcond_polarcheck, lawstat::levene.test(manip_check_scale, as.factor(polar_cond),location="mean"))

# we know we have unequal variances across levels 
# thus, need to report the Welch-adjusted ANOVA and games-howell-adjusted parameters for the pairwise comparisons

# Welch's test
wmodel_omnibus_polarcheck <- oneway.test(manip_check_scale ~ as.factor(polar_cond), data = polarcond_polarcheck)
wmodel_omnibus_polarcheck

# games-howell adjustment for pairwise comparisons

library(userfriendlyscience)
library(MBESS)
gh.manip_check_scale <- oneway(polarcond_polarcheck$polar_cond, y = polarcond_polarcheck$manip_check_scale, posthoc = 'games-howell')
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

cohensD(x = manip_check_scale ~ LvM, data = pols1_abbrev2, method = "unequal")
cohensD(x = manip_check_scale ~ HvM, data = pols1_abbrev2, method = "unequal")
cohensD(x = manip_check_scale ~ LvH, data = pols1_abbrev2, method = "unequal")


# 5. PLOTTING THE EFFECT OF CONDITION ON MANIP CHECK SCALE ----

#this is terrible; too much overplotting to see anything because of the interval nature of the x axis
ggplot(pols1_abbrev2, aes(x = polar_cond, y = manip_check_scale)) +
      geom_point()

#geom_jitter corrects the overplotting
#geom_smooth adds a regression line that shows a clearly linear effect
#although, there is clearly a lot of variance in each condition judging by the spread between points
ggplot(pols1_abbrev2, aes(x = as.numeric(polar_cond), y = manip_check_scale)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE, na.rm = TRUE)

# y axis scale is wrong
# depicts an erroneous inverse quadratic effect, 
#rather than a pos linear effect from low to high pol
ggplot(pols1_abbrev2, aes(x = polar_cond, y = mean(manip_check_scale, na.rm = TRUE))) +
  geom_col()

#y axis still wrong, but better! We now see the pos. linear effect
# axis runs 0-3 instead of 1-5
pols1_abbrev2 %>%
        group_by(polar_cond) %>%
        summarize(avg_check = mean(manip_check_scale, na.rm = TRUE)) %>%
        ggplot(aes(x = polar_cond, y = avg_check)) + 
                 geom_col()

#fixing the y axis to represent the range of manip_check_scale
#changing from bars to single dots representing the mean
# now we see the positive linear effect from low to high!
pols1_abbrev2 %>%
  group_by(polar_cond) %>%
  summarize(avg_check = mean(manip_check_scale, na.rm = TRUE)) %>%
  ggplot(aes(x = polar_cond, y = avg_check)) + 
  geom_point() +
  scale_y_continuous(limits = c(1, 5)) 


#6. ANALYSES IN ANOVA: DV = DESIRE SCALE (AS IN THE MANUSCRIPT) ----

# Q1a. Do people in the low AND high polarization conditions report greater desire compared to
#     people in the medium condition?
# Q1b. Do people in the low polarization and high polarization conditions (excluding medium condition)  
#     report equal levels of desire?

# run an omnibus ANOVA for the primary DV = desire scale by condition
#df and F look good!

amodel_omnibus_desire <- aov(desire_scale ~ polar_cond, data = pols1_abbrev2)
summary(amodel_omnibus_desire)

# test whether the homogeniety of variance across levels assumption holds
# this code does not work if there is missing data!
# solution: first filter for missing values on the vectors passed to levene.test

table(pols1_abbrev2$polar_cond)

polarcond_desire <- pols1_abbrev2 %>%
  select(desire_scale, polar_cond) %>%
  filter(polar_cond != "NA" & desire_scale > -99)

# polarcond_polarcheck, n = 630
# equal variances, so no need for adjustment
with(polarcond_desire, lawstat::levene.test(desire_scale, as.factor(polar_cond),location="mean"))


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
# these look good!
summary.aov(amodel_omnibus_desire, split=list(polar_cond=list("High vs. Low"=2, 
                                           "High & Low vs Medium" = 1))) 


# effect sizes for the contrasts: contrast correlation (Rosnow, Rosenthal, & Rubin, 2000)

# the below provides the partial correlation between scores on the outcome variable
# and the lambdas associated with the groups, after eliminating all between-group non-contrast variation

# rcontrast = sqrt(F or t^2)/sqrt(F or t^2 + df_within)

# High and low versus medium contrast

sqrt(9.345)/sqrt(9.345 + 627)

# High versus low contrast

sqrt(.935)/sqrt(.935 + 627)


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




#8. ANALYSES IN REGRESSION ----

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

# model predicting manip_check_scale 
rmodel_linear_check <- lm(manip_check_scale ~ as.numeric(polar_cond), data = pols1_abbrev2)
summary(rmodel_linear_check)

#first, check the linear effect on DV = desire. No effect, as expected.
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








