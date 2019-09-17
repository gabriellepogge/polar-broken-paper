
library(readr)
library(dplyr)

# Adjusted contrasts for Study 7: desire DV

pols1 <- data.frame(read_csv("/Users/gabriellechristinemartin/Desktop/S7_simcomp.csv", col_names = TRUE))

#returns class: data.frame
class(pols1)

#provides the structure of the data: number observations, number variables, variable names, variable type/classes 
#plus a preview of the data
glimpse(pols1)

# first coerce polar_cond_m2 to factor 

pols1 <- pols1 %>%
  mutate(polar_cond_m2_fact = as.factor(polar_cond_m2))

levels(pols1$polar_cond_m2_fact) <- c("High Polarization", "Low Polarization","Med2 Polarization")
class(pols1$polar_cond_m2_fact)
summary(pols1$polar_cond_m2_fact)


# adjusted contrasts to account for heteroscedasticity 
# load libraries before analysis

library(SimComp)
library(multcomp)
library(mvtnorm)

#creating the contrast codes 
Low <- c(-1, 1) #high v low, excluding medium
Med2 <- c(0, -2) #high and low versus medium
High <- c(1, 1) #high v low, excluding medium

# combine the above 3 lines into a matrix
mat2 <- cbind(Low, Med2, High)

# filter out missing data prior to analysis

polarcond_desire_m2 <- pols1 %>%
  dplyr::select(desire_scale, polar_cond_m2_fact) %>%
  filter(polar_cond_m2_fact != "NA" & desire_scale > -99)

SimTestDiff(data = polarcond_desire_m2, grp="polar_cond_m2_fact", resp="desire_scale", ContrastMat = mat2,
            covar.equal = FALSE)


# effect sizes for the contrasts: contrast correlation (Rosnow, Rosenthal, & Rubin, 2000)
# the below provides the partial correlation between scores on the outcome variable
# and the lambdas associated with the groups, after eliminating all between-group non-contrast variation

# rcontrast = sqrt(F or t^2)/sqrt(F or t^2 + df_within)

#rcontrast: high and low versus medium polarization
sqrt(0.960^2)/sqrt(0.960^2 + 164.7)

#rcontrast: high versus low excluding medium polarization
sqrt(1.252^2)/sqrt(1.252^2 + 330.2)




