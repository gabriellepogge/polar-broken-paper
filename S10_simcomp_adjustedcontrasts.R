
library(readr)
library(dplyr)

# Adjusted contrasts for Study 10: lack of choice DV

pols1 <- data.frame(read_csv("/Users/gabriellechristinemartin/Desktop/S10_simcomp.csv", col_names = TRUE))

#returns class: data.frame
class(pols1)

#provides the structure of the data: number observations, number variables, variable names, variable type/classes 
#plus a preview of the data
glimpse(pols1)

# first coerce brokenpol_cond_fact to factor 

pols1 <- pols1 %>%
  mutate(brokenpol_cond_fact = as.factor(brokenpol_cond_fact))

levels(pols1$brokenpol_cond_fact) <- c("Not Broken, Low Polarization", "Not Broken, Medium Polarization","Not Broken, High Polarization",
                                               "Broken, Low Polarization", "Broken, Medium Polarization","Broken, High Polarization")
class(pols1$brokenpol_cond_fact)
summary(pols1$brokenpol_cond_fact)


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

# analyses for lack of choice subscale nochoice12

# filter out missing data prior to analysis

polarbroken_nochoice12 <- pols1 %>%
  dplyr::select(nochoice12, brokenpol_cond_fact) %>%
  filter(brokenpol_cond_fact != "NA" & nochoice12 > -99)

SimTestDiff(data = polarbroken_nochoice12, grp="brokenpol_cond_fact", resp="nochoice12", ContrastMat = mat2,
            covar.equal = FALSE)


# effect sizes for the contrasts: contrast correlation (Rosnow, Rosenthal, & Rubin, 2000)
# the below provides the partial correlation between scores on the outcome variable
# and the lambdas associated with the groups, after eliminating all between-group non-contrast variation

# rcontrast = sqrt(F or t^2)/sqrt(F or t^2 + df_within)

# rcontrast: brokenness effect
sqrt(1.8107^2)/sqrt(1.8107^2 + 670.7)

#rcontrast: high and low versus medium polarization
sqrt(12.9013^2)/sqrt(12.9013^2 + 382.6)

#rcontrast: high versus low excluding medium polarization
sqrt(6.0553^2)/sqrt(6.0553^2 + 483.4)


# analyses for nochoice4

# filter out missing data prior to analysis

polarbroken_nochoice4 <- pols1 %>%
  dplyr::select(nochoice4, brokenpol_cond_fact) %>%
  filter(brokenpol_cond_fact != "NA" & nochoice4 > -99)

SimTestDiff(data = polarbroken_nochoice4, grp="brokenpol_cond_fact", resp="nochoice4", ContrastMat = mat2,
            covar.equal = FALSE)

# rcontrast = sqrt(F or t^2)/sqrt(F or t^2 + df_within)

# rcontrast: brokenness effect
sqrt(1.183^2)/sqrt(1.183^2 + 725.3)

#rcontrast: high and low versus medium polarization
sqrt(2.884^2)/sqrt(2.884^2 + 511.2)

#rcontrast: high versus low excluding medium polarization
sqrt(4.135^2)/sqrt(4.135^2 + 480.3)


# analyses for nochoice5

polarbroken_nochoice5 <- pols1 %>%
  dplyr::select(nochoice5, brokenpol_cond_fact) %>%
  filter(brokenpol_cond_fact != "NA" & nochoice5 > -99)

SimTestDiff(data = polarbroken_nochoice5, grp="brokenpol_cond_fact", resp="nochoice5", ContrastMat = mat2,
            covar.equal = FALSE)

# rcontrast = sqrt(F or t^2)/sqrt(F or t^2 + df_within)

# rcontrast: brokenness effect
sqrt(1.8478^2)/sqrt(1.8478^2 + 710.9)

#rcontrast: high and low versus medium polarization
sqrt(9.2568^2)/sqrt(9.2568^2 + 522.0)

#rcontrast: high versus low excluding medium polarization
sqrt(6.1101^2)/sqrt(6.1101^2 + 468.2)

