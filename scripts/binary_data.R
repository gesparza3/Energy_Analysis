###############################################################################

# Read libraries
library(dplyr)
library(glmulti)
library(ggplot2)

###############################################################################

## read in data
usa.energy <- read.csv("~/Energy_Analysis/data/usa_energy.csv")

###############################################################################

## Make usa.energy binary
energy.binary <- select(usa.energy, c("year", "coal.growth", "fossil_fuel.growth", 
                                      "geothermal.growth", "hydro.growth", 
                                      "motor_gas.growth", "natural_gas.growth", 
                                      "solar.growth", "wind.growth", "GDP.growth"))

## Convert variables
energy.binary[,2:9] <- as.data.frame(ifelse(energy.binary[,2:9] > 1, 1, 0))
energy.binary[,10] <- as.data.frame(ifelse(energy.binary[,10] > mean(usa.energy$GDP.growth), 1, 0))
energy.binary[,2:10] <- lapply(energy.binary[,2:10], as.factor)

###############################################################################

## Glmulti function
best.mods <- glmulti::glmulti(GDP.growth ~ geothermal.growth + hydro.growth + 
                                solar.growth + wind.growth + coal.growth + 
                                fossil_fuel.growth + geothermal.growth + 
                                hydro.growth + motor_gas.growth + 
                                natural_gas.growth, data = energy.binary, 
                              level = 1, method = "h", crit = "bic", 
                              confsetsize = 5, plotty = F, report = F, 
                              fitfunction = "glm", family = binomial)

## Best models from glmulti
best.mods@formulas

## Run the model
mod <- glm(GDP.growth ~ motor_gas.growth, data = energy.binary, family = "binomial")
summary(mod)
MKmisc::HLgof.test(fit = fitted(mod.1), obs = mod.1$y)

###############################################################################

## Bar Chart for Coal
ggplot(energy.binary, aes(coal.growth)) + geom_bar()

## Two on same plot
ggplot(usa.energy[32:56,], aes(year)) +
  geom_line(aes(y = solar.growth), color = "#68382C", size = 1.25) +
  geom_line(aes(y = wind.growth), color = "#000080", size = 1.25) +
  scale_x_continuous(breaks = c(seq(1960, 2015, 5), 2015)) +
  scale_y_continuous(sec.axis = sec_axis(~.*1,name = "Motorgas consumption")) +
  ggtitle("Fossil Fuel Consumption")
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "gray50", size = 0.5),
        panel.grid.major.x = element_blank())