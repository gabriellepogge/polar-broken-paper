# set the working directory
getwd()

setwd("/gabriellechristinemartin/Desktop/R_Pol_data_scripts/POL Corr 6")

# clear the environment
rm(list=ls())

library(tidyverse)
library(haven)

#1. IMPORTING, VIEWING, AND SELECTING THE DATA ----

#create data.frame called pols1 from the spss (.sav) file
pols1 <- data.frame(read_sav("/Users/gabriellechristinemartin/Desktop/R_Pol_data_scripts/POL Corr 6/POL_CorrS6_original.sav"))

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
#recodes the string indicator of whether the participant got to the end of the study: complete
#recodes string sex to factor gender

pols1_abbrev <- pols1 %>%
  dplyr::select(attsimilar, attdifferent, attoverlap, dbias, rbias, dhostile, rhostile, drvalues, lcvalues,
         brokensystem, systemfailing, electflawed, electpoorfunct,
         trustgov1, trustgov2, trustgov3, trustpols1, trustpols2,
         twopartyprob1, twopartyprob2, notransp1, notransp2, efficacy1, efficacy2, efficacy3,
         crony1, crony2, crony3, voteintegrity1, voteintegrity2, voteintegrity3, 
         sizegov1, sizegov2, sizegov3, liberty1, liberty2,
         disenf1, disenf2, disenf3, negparty1, negparty2, negparty3, easychoose, clearchoices,
         desirepersonal, desireothers, desirebenefit, 
         thirdsupport1, thirdsupport2, thirdsupport3, thirdsupport4, thirdsupport5,
         fixsystem1, fixsystem4, fixsystem5,fixsystem9, fixsystem10,
         X._session_status, polorient, partyid, polknowledge, sex, education, ethnicityomb, raceomb,
         birthyear, birthmonth) %>%
  mutate(partyid = as.factor(partyid)) %>%
  mutate(education = as.factor(education)) %>%
  mutate(ethnicityomb = as.factor(ethnicityomb)) %>%
  mutate(raceomb = as.factor(raceomb)) %>%
  mutate(complete = as.factor(case_when(
    X._session_status == "C" ~ 1,
    X._session_status == "null" ~ 0))) %>%
  mutate(gender = as.factor(case_when(
    sex == "m" ~ 1,
    sex == "f" ~ 2))) 

glimpse(pols1_abbrev)

#checking to make sure the ns in each factor level are the same after re-coding into new var

table(pols1_abbrev$X._session_status)
table(pols1_abbrev$complete)
class(pols1_abbrev$complete)

table(pols1_abbrev$sex)
table(pols1_abbrev$gender)
class(pols1_abbrev$gender)

# set factor levels 

levels(pols1_abbrev$complete) <- c("P exited study early", "P reached study end")
summary(pols1_abbrev$complete)

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

# this is what we will want to report in the supplement: complete_DV & age > 18 (ignoring whether Ps got to the end)

# dropped 381 incomplete DV (all three DV responses not present), n = 400
#droped Ps under 18 (n = 5): results in 395 responses

pols1_abbrev2 <- pols1_abbrev %>%
  dplyr::select(-X._session_status, -sex) %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(age = 2019 - birthyear) %>%
  mutate(complete_DV = (as.factor(ifelse(
    desirepersonal > -99 & desirebenefit > -99 & desireothers > -99, 1, NA)))) %>%
  filter(complete_DV == 1) %>%
  filter(age >= 18)

glimpse(pols1_abbrev2)

summary(pols1_abbrev2$age, na.rm = TRUE)
sd(pols1_abbrev2$age, na.rm = TRUE)
summary(pols1_abbrev2$partyid)
sd(pols1_abbrev2$polorient, na.rm = TRUE)
summary(pols1_abbrev2$raceomb)
summary(pols1_abbrev2$gender)


pols1_abbrev3 <- pols1_abbrev2 %>%
  mutate(polorient17 = (ifelse(
    polorient == 8, NA, polorient))) 

table(pols1_abbrev3$polorient17)
summary(pols1_abbrev3$polorient17, na.rm = TRUE)
sd(pols1_abbrev3$polorient17, na.rm = TRUE)

#3. CHECKING FOR SCALE RELIABILITY, EFA (scale structure), & CREATING SCALES ----


library(psych)

#3.1a. Factor analysis of polarization items

#reverse-code attsimilar and attoverlap before testing alpha
# then, combine with attdifferent by taking the average

pols1_abbrev2 <- pols1_abbrev2 %>%
  mutate(attsimilar_r = 
           6 - attsimilar) %>%
  mutate(attoverlap_r = 
           6 - attoverlap) 

table(pols1_abbrev2$attsimilar)
table(pols1_abbrev2$attsimilar_r)

table(pols1_abbrev2$attoverlap)
table(pols1_abbrev2$attoverlap_r)

polar_scale <- select(pols1_abbrev2, attsimilar_r, attdifferent, attoverlap_r, 
                      dhostile, rhostile, dbias, rbias, drvalues, lcvalues)

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
# oblique.scores = TRUE uses the rotated solution, i.e., pattern matrix rather than structure matrix
# n.iter provides CIs for the loadings (does not appear to alter the estimates); must use n.obs with n.iter

fa_pol1 <- fa(r = pol_mat, nfactors = 1, rotate = "oblimin", fm = "pa", oblique.scores = TRUE, n.iter = 25, n.obs = 400)
fa_pol1

fa_pol2 <- fa(r = pol_mat, nfactors = 2, rotate = "oblimin", fm = "pa", oblique.scores = TRUE)
fa_pol2

#3.1b. Reliability for the two polarization factors

affect_pol <- select(pols1_abbrev2,dhostile, rhostile, dbias, rbias)

att_pol <- select(pols1_abbrev2, attsimilar_r, attdifferent, attoverlap_r)

att_affect_pol <- select(pols1_abbrev2, attsimilar_r, attdifferent, attoverlap_r,
                         dhostile, rhostile, dbias, rbias)


psych::alpha(affect_pol)
psych::alpha(att_pol)
psych::alpha(att_affect_pol)


#3.2a. Factor analysis of brokenness items

pols1_abbrev2 <- pols1_abbrev2 %>%
  mutate(easychoose_r = 
           6 - easychoose) %>%
  mutate(clearchoices_r = 
           6 - clearchoices) %>%
  mutate(trustgov2_r = 
           6 - trustgov2) %>%
  mutate(trustgov3_r =
           6 - trustgov3) %>%
  mutate(sizegov2_r = 
           6 - sizegov2) %>%
  mutate(liberty1_r = 
           6 - liberty1) 

table(pols1_abbrev2$easychoose)
table(pols1_abbrev2$easychoose_r)

table(pols1_abbrev2$clearchoices)
table(pols1_abbrev2$clearchoices_r)

table(pols1_abbrev2$trustgov2)
table(pols1_abbrev2$trustgov2_r)

table(pols1_abbrev2$trustgov3)
table(pols1_abbrev2$trustgov3_r)

table(pols1_abbrev2$sizegov2)
table(pols1_abbrev2$sizegov2_r)

table(pols1_abbrev2$liberty1)
table(pols1_abbrev2$liberty1_r)

broken_scale <- select(pols1_abbrev2, brokensystem, systemfailing, electflawed, electpoorfunct,
                      trustgov1, trustgov2_r, trustgov3_r, trustpols1, trustpols2,
                      twopartyprob1, twopartyprob2, notransp1, notransp2, efficacy1, efficacy2, efficacy3,
                      crony1, crony2, crony3, voteintegrity1, voteintegrity2, voteintegrity3, 
                      sizegov1, sizegov2_r, sizegov3, liberty1_r, liberty2,
                      disenf1, disenf2, disenf3, negparty1, negparty2, negparty3, easychoose_r, clearchoices_r)

# step 1: create the matrix with only the items in the scale
broken_mat <- cor(broken_scale, use = "na.or.complete")
broken_mat

# step 2: factor analysis of the matrix

fa_broken1 <- fa(r = broken_mat, nfactors = 1, rotate = "oblimin", fm = "pa", oblique.scores = TRUE, n.iter = 25, n.obs = 400)
fa_broken1

fa_broken2 <- fa(r = broken_mat, nfactors = 2, rotate = "oblimin", fm = "pa", oblique.scores = TRUE, n.iter = 25, n.obs = 400)
fa_broken2

#3.2b. Reliability for the two brokenness factors

broken21_scale <- select(pols1_abbrev2, brokensystem, systemfailing, electflawed, electpoorfunct,
                       trustgov1, trustgov3_r, trustpols1, trustpols2,
                       twopartyprob1, twopartyprob2, notransp1, notransp2, efficacy2,
                       crony2, crony3, voteintegrity1, voteintegrity2, voteintegrity3, 
                       sizegov2_r, liberty1_r, liberty2)

broken9_scale <- select(pols1_abbrev2, crony1,sizegov1, sizegov3, 
                       disenf1, disenf2, disenf3, negparty1, negparty2, negparty3)

psych::alpha(broken21_scale, check.keys = TRUE)
psych::alpha(broken9_scale, check.keys = TRUE)

broken30_scale <- dplyr::select(pols1_abbrev2, brokensystem, systemfailing, electflawed, electpoorfunct,
                         trustgov1, trustgov3_r, trustpols1, trustpols2,
                         twopartyprob1, twopartyprob2, notransp1, notransp2, efficacy2,
                         crony2, crony3, voteintegrity1, voteintegrity2, voteintegrity3, 
                         sizegov2_r, liberty1_r, liberty2, crony1,sizegov1, sizegov3, 
                         disenf1, disenf2, disenf3, negparty1, negparty2, negparty3)

psych::alpha(broken30_scale, check.keys = TRUE)

#3.3a. Factor analysis of support items

support_scale <- select(pols1_abbrev2,desirepersonal, desireothers, desirebenefit, 
                        thirdsupport1, thirdsupport2, thirdsupport3, thirdsupport4, thirdsupport5,
                        fixsystem1, fixsystem4, fixsystem5,fixsystem9, fixsystem10)

# step 1: create the matrix with only the items in the scale
support_mat <- cor(support_scale, use = "na.or.complete")
support_mat

# step 2: factor analysis of the matrix

fa_broken1 <- fa(r = support_mat, nfactors = 1, rotate = "oblimin", fm = "pa", oblique.scores = TRUE, n.iter = 25, n.obs = 400)
fa_broken1

fa_broken2 <- fa(r = support_mat, nfactors = 2, rotate = "oblimin", fm = "pa", oblique.scores = TRUE, n.iter = 25, n.obs = 400)
fa_broken2

#3.3b. Reliability for the fixes factor

psych::alpha(support_mat, check.keys = TRUE)

desire <- select(pols1_abbrev2,desirepersonal, desireothers, desirebenefit)
desire_mat <- cor(desire, use = "na.or.complete")
psych::alpha(desire_mat, check.keys = TRUE)

gensupport <- select(pols1_abbrev2,thirdsupport1, thirdsupport2, thirdsupport3, thirdsupport4, thirdsupport5)
gensupp_mat <- cor(gensupport, use = "na.or.complete")
psych::alpha(gensupp_mat, check.keys = TRUE)

genfixes <- select(pols1_abbrev2,fixsystem1, fixsystem4, fixsystem5,fixsystem9, fixsystem10)
genfixes_mat <- cor(genfixes, use = "na.or.complete")
psych::alpha(genfixes_mat, check.keys = TRUE)


#3.4a. Factor analysis of all items

allitems_scale <- select(pols1_abbrev2, attsimilar_r, attdifferent, attoverlap_r, dbias, rbias, dhostile, rhostile, drvalues, lcvalues,
       brokensystem, systemfailing, electflawed, electpoorfunct,
       trustgov1, trustgov2_r, trustgov3_r, trustpols1, trustpols2,
       twopartyprob1, twopartyprob2, notransp1, notransp2, efficacy1, efficacy2, efficacy3,
       crony1, crony2, crony3, voteintegrity1, voteintegrity2, voteintegrity3, 
       sizegov1, sizegov2_r, sizegov3, liberty1_r, liberty2,
       disenf1, disenf2, disenf3, negparty1, negparty2, negparty3, easychoose_r, clearchoices_r,
       desirepersonal, desireothers, desirebenefit, 
       thirdsupport1, thirdsupport2, thirdsupport3, thirdsupport4, thirdsupport5,
       fixsystem1, fixsystem4, fixsystem5,fixsystem9, fixsystem10)

# step 1: create the matrix with only the items in the scale
allitems_mat <- cor(allitems_scale, use = "na.or.complete")
allitems_mat

# step 2: factor analysis of the matrix: try nfactors = 1, 3, 4, 5 

fa_allitems1 <- fa(r = allitems_mat, nfactors = 1, rotate = "oblimin", fm = "pa", oblique.scores = TRUE, n.iter = 25, n.obs = 400)
fa_allitems1

fa_allitems3 <- fa(r = allitems_mat, nfactors = 3, rotate = "oblimin", fm = "pa", oblique.scores = TRUE, n.iter = 25, n.obs = 400)
fa_allitems3

fa_allitems5 <- fa(r = allitems_mat, nfactors = 5, rotate = "oblimin", fm = "pa", oblique.scores = TRUE, n.iter = 25, n.obs = 400)
fa_allitems5

#4. CREATING FACTOR SCALES & SCALE CORRELATIONS---- 

pols1_abbrev2 <- pols1_abbrev2 %>%
    mutate(affectpol_scale = (dhostile + rhostile + dbias + rbias)/4) %>%
    mutate(attpol_scale = (attsimilar_r + attdifferent + attoverlap_r)/3) %>%
    mutate(allpolscale = (dhostile + rhostile + dbias + rbias + attsimilar_r + attdifferent + attoverlap_r)/7) %>%
    mutate(brokentrust_scale = (brokensystem + systemfailing + electflawed + electpoorfunct +
           trustgov1 + trustgov3_r + trustpols1 + trustpols2 + twopartyprob1 + twopartyprob2 + 
          notransp1 + notransp2 + efficacy2 + crony2 + crony3 + voteintegrity1 + voteintegrity2 + 
          voteintegrity3 + sizegov2_r + liberty1_r + liberty2)/21) %>%
    mutate(brokenchoice_scale = (crony1 + sizegov1 + sizegov3 + disenf1 + disenf2 + disenf3 +
                                   negparty1 + negparty2 + negparty3)/9) %>%
    mutate(brokentrustchoice_scale = (brokensystem + systemfailing + electflawed + electpoorfunct +
             trustgov1 + trustgov3_r + trustpols1 + trustpols2 + twopartyprob1 + twopartyprob2 + 
             notransp1 + notransp2 + efficacy2 + crony2 + crony3 + voteintegrity1 + voteintegrity2 + 
             voteintegrity3 + sizegov2_r + liberty1_r + liberty2 + crony1 + sizegov1 + sizegov3 + disenf1 + disenf2 + disenf3 +
               negparty1 + negparty2 + negparty3)/30) %>%
  mutate(desire_scale = (desirepersonal + desireothers + desirebenefit)/3) %>%
  mutate(thirdsupport_scale = (thirdsupport1 + thirdsupport2 + thirdsupport3 + thirdsupport4 + thirdsupport5)/5) %>%
  mutate(fixes_scale = (fixsystem1 + fixsystem4 + fixsystem5 + fixsystem9 + fixsystem10)/5) %>%
  mutate(allsupport_scale = (desirepersonal + desireothers + desirebenefit +
                               thirdsupport1 + thirdsupport2 + thirdsupport3 + thirdsupport4 + thirdsupport5 +
                               fixsystem1 + fixsystem4 + fixsystem5 + fixsystem9 + fixsystem10)/13)


factor_scales <- as.data.frame(select(pols1_abbrev2, affectpol_scale, attpol_scale, allpolscale, brokentrust_scale, brokenchoice_scale, brokentrustchoice_scale,
       desire_scale, thirdsupport_scale,fixes_scale, allsupport_scale))

lowerCor(x = factor_scales, digits=2,use="pairwise",method="pearson")


#5. CREATING QUADRATIC TERMS BEFORE REGRESSION  ----

# creating the function to center with 'scale()' before passing to mutate() below
center_scale <- function(x) {
  scale(x, scale = FALSE)
}

#mean-centering the three polarization scales before squaring and adding them back

pols1_abbrev2 <- pols1_abbrev2 %>%
  mutate(affectpol_C = 
           as.vector(center_scale(affectpol_scale))) %>%
  mutate(attpol_C = 
           as.vector(center_scale(attpol_scale))) %>%
  mutate(allpol_C = 
           as.vector(center_scale(allpolscale))) %>%
  mutate(affectpol_Csq =
           affectpol_C*affectpol_C) %>%
  mutate(attpol_Csq = 
           attpol_C*attpol_C) %>%
  mutate(allpol_Csq = 
           allpol_C*allpol_C)

#shows that the means for the three is zero because it is mean centered

summary(pols1_abbrev2$affectpol_C) 
summary(pols1_abbrev2$attpol_C) 
summary(pols1_abbrev2$allpol_C) 

#6. REGRESSION TESTING THE LINEAR AND QUADRATIC EFFECTS OF POLARIZATION ----

# effects of affective, issue, and combined polarization on general brokenness, lack of choice, and combined brokenness

# DV = general brokenness and trust

# 6.1.a

mod1= lm(brokentrust_scale~affectpol_C+affectpol_Csq, data = pols1_abbrev2)
summary(mod1)

mod2= lm(brokentrust_scale~attpol_C+attpol_Csq, data = pols1_abbrev2)
summary(mod2)

mod3= lm(brokentrust_scale~allpol_C+allpol_Csq, data = pols1_abbrev2)
summary(mod3)

# DV = lack of choice

# 6.1.b

mod4= lm(brokenchoice_scale~affectpol_C+affectpol_Csq, data = pols1_abbrev2)
summary(mod4)

mod5= lm(brokenchoice_scale~attpol_C+attpol_Csq, data = pols1_abbrev2)
summary(mod5)

mod6= lm(brokenchoice_scale~allpol_C+allpol_Csq, data = pols1_abbrev2)
summary(mod6)


# DV = Combined brokenness/trust and lack of choice

# 6.1.c

mod7= lm(brokentrustchoice_scale~affectpol_C+affectpol_Csq, data = pols1_abbrev2)
summary(mod7)

mod8= lm(brokentrustchoice_scale~attpol_C+attpol_Csq, data = pols1_abbrev2)
summary(mod8)

mod9= lm(brokentrustchoice_scale~allpol_C+allpol_Csq, data = pols1_abbrev2)
summary(mod9)


# effects of affective, issue, and combined polarization on desire, general support, support for alternative fixes, and combined support

# DV = desire for inclusion of a third party candidate

# 6.2.a

mod10= lm(desire_scale~affectpol_C+affectpol_Csq, data = pols1_abbrev2)
summary(mod10)

mod11= lm(desire_scale~attpol_C+attpol_Csq, data = pols1_abbrev2)
summary(mod11)

mod12= lm(desire_scale~allpol_C+allpol_Csq, data = pols1_abbrev2)
summary(mod12)

# DV = general support for third parties and candidates

# 6.2.b

mod13= lm(thirdsupport_scale~affectpol_C+affectpol_Csq, data = pols1_abbrev2)
summary(mod13)

mod14= lm(thirdsupport_scale~attpol_C+attpol_Csq, data = pols1_abbrev2)
summary(mod14)

mod15= lm(thirdsupport_scale~allpol_C+allpol_Csq, data = pols1_abbrev2)
summary(mod15)

# DV = support for alternative fixes

# 6.2.c

mod16= lm(fixes_scale~affectpol_C+affectpol_Csq, data = pols1_abbrev2)
summary(mod16)

mod17= lm(fixes_scale~attpol_C+attpol_Csq, data = pols1_abbrev2)
summary(mod17)

mod18= lm(fixes_scale~allpol_C+allpol_Csq, data = pols1_abbrev2)
summary(mod18)

# DV = combined support scale with the three subscales

# 6.2.d

mod19= lm(allsupport_scale~affectpol_C+affectpol_Csq, data = pols1_abbrev2)
summary(mod19)

mod20= lm(allsupport_scale~attpol_C+attpol_Csq, data = pols1_abbrev2)
summary(mod20)

mod21= lm(allsupport_scale~allpol_C+allpol_Csq, data = pols1_abbrev2)
summary(mod21)

