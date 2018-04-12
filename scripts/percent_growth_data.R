###############################################################################

# Read libraries
library(dplyr)
library(glmulti)
library(ggplot2)

###############################################################################

## read in data
usa.energy <- read.csv("~/Energy_Analysis/data/usa_energy.csv")

###############################################################################

## Make select percent growth variables
energy.pg <- select(usa.energy, c("year", "coal.growth", "fossil_fuel.growth", 
                                      "geothermal.growth", "hydro.growth", 
                                      "motor_gas.growth", "natural_gas.growth", 
                                      "solar.growth", "wind.growth", "GDP.growth"))

## Make GDP binary
energy.pg[,10] <- as.data.frame(ifelse(energy.pg[,10] > 1.013, 1, 0))
energy.pg[,10] <- as.factor(energy.pg[,10])

###############################################################################

## Glmulti function
best.mods <- glmulti::glmulti(GDP.growth ~ geothermal.growth + hydro.growth + 
                                solar.growth + wind.growth + coal.growth + 
                                fossil_fuel.growth + geothermal.growth + 
                                hydro.growth + motor_gas.growth + 
                                natural_gas.growth, data = energy.pg, 
                              level = 1, method = "h", crit = "bic", 
                              confsetsize = 5, plotty = F, report = F, 
                              fitfunction = "glm", family = binomial)

## Best models from glmulti
best.mods@formulas

## Run the model
mod <- glm(GDP.growth ~ motor_gas.growth, data = energy.pg, family = "binomial")
summary(mod)
MKmisc::HLgof.test(fit = fitted(mod), obs = mod.1$y)