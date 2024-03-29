
# Adjusted contrasts for Study 4b: general support for third parties DV

pols1 <- data.frame(read_csv("/Users/gabriellechristinemartin/Desktop/R_Pol_data_scripts/S4ab_adjusted_contrasts/S4b_adjusted_contrasts/S4b_simcomp.csv"))

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

brokenpolar_thirdsupport<- pols1 %>%
  dplyr::select(thirdsupport_scale, brokenpol_cond_fact) %>%
  filter(brokenpol_cond_fact != "NA" & thirdsupport_scale > -99)

SimTestDiff(data = brokenpolar_thirdsupport, grp="brokenpol_cond_fact", resp="thirdsupport_scale", ContrastMat = mat2,
            covar.equal = FALSE)

# effect sizes for the contrasts: contrast correlation (Rosnow, Rosenthal, & Rubin, 2000)
# the below provides the partial correlation between scores on the outcome variable
# and the lambdas associated with the groups, after eliminating all between-group non-contrast variation

# rcontrast = sqrt(F or t^2)/sqrt(F or t^2 + df_within)

# rcontrast: brokenness effect
sqrt(2.2233^2)/sqrt(2.2233^2 + 853.6)

#rcontrast: high and low versus medium polarization
sqrt(1.0417^2)/sqrt(1.0417^2 + 706.8)

#rcontrast: high versus low excluding medium polarization
sqrt(0.7431^2)/sqrt(0.7431^2 + 557.2)

#rcontrast: brokenness x high versus low polarization (excluding medium)
sqrt(2.2164^2)/sqrt(2.2164^2 + 557.2)



