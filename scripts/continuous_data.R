###############################################################################

# Read libraries
library(dplyr)
library(glmulti)
library(ggplot2)

###############################################################################

## read in data
usa.energy <- read.csv("~/Energy_Analysis/data/usa_states_energy.csv")

###############################################################################

## Make usa.energy continuous
energy.continuous <- select(usa.energy, c("year", "CLTCB", "FFTCB", "GETCB",
                                      "HYTCB", "MGTCB", "NGTCB", "SOTCB",
                                      "WYTCB", "GDP.growth"))
energy.continuous[,10] <- as.data.frame(ifelse(energy.continuous[,10] > 1.013, 1, 0))
energy.continuous[,10] <- as.factor(energy.continuous[,10])

###############################################################################

## Generate some models using glmulti
best.mods <- glmulti::glmulti(GDP.growth ~ year + CLTCB + FFTCB + GETCB + HYTCB + 
                                MGTCB + NGTCB + SOTCB + WYTCB,
                              data = energy.continuous, level = 1, method = "h", 
                              crit = "aic", confsetsize = 5, plotty = F, 
                              report = F, fitfunction = "glm", family = binomial)

## Best models from glmulti
best.mods@formulas

## Run the model
mod <- glm(GDP.growth ~ HYTCB + NGTCB, data = energy.continuous, family = "binomial")
summary(mod)



