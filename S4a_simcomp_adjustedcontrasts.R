
# Adjusted contrasts for Study 4a: desire DV and alternative fixes DV

pols1 <- data.frame(read_csv("/Users/gabriellechristinemartin/Desktop/R_Pol_data_scripts/S4ab_adjusted_contrasts/S4a_adjusted_contrasts/S4a_simcomp.csv"))

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

# filter out missing data prior to analysis

brokenpolar_desire<- pols1 %>%
  dplyr::select(desire_scale, brokenpol_cond_fact) %>%
  filter(brokenpol_cond_fact != "NA" & desire_scale > -99)

SimTestDiff(data = brokenpolar_desire, grp="brokenpol_cond_fact", resp="desire_scale", ContrastMat = mat2,
            covar.equal = FALSE)

# effect sizes for the contrasts: contrast correlation (Rosnow, Rosenthal, & Rubin, 2000)
# the below provides the partial correlation between scores on the outcome variable
# and the lambdas associated with the groups, after eliminating all between-group non-contrast variation

# rcontrast = sqrt(F or t^2)/sqrt(F or t^2 + df_within)

# rcontrast: brokenness 
sqrt(9.48^2)/sqrt(9.48^2 + 1262.60)

#rcontrast: high and low versus medium polarization
sqrt(4.18^2)/sqrt(4.18^2 + 866.00)

#rcontrast: high versus low excluding medium polarization
sqrt(2.98^2)/sqrt(2.98^2 + 827.40)

# filter out missing data prior to analysis

brokenpolar_fixes<- pols1 %>%
  dplyr::select(genfixes_scale, brokenpol_cond_fact) %>%
  filter(brokenpol_cond_fact != "NA" & genfixes_scale > -99)

SimTestDiff(data = brokenpolar_fixes, grp="brokenpol_cond_fact", resp="genfixes_scale", ContrastMat = mat,
            covar.equal = FALSE)

# rcontrast: brokenness 
sqrt(4.09^2)/sqrt(4.09^2 + 1108.7)

#rcontrast: high and low versus medium polarization
sqrt(1.48^2)/sqrt(1.48^2 + 705.90)

#rcontrast: high versus low excluding medium polarization
sqrt(.02^2)/sqrt(.02^2 + 732.60)
