## Model Building

################################################################################

## Load libraries
library(lme4)
library(sjPlot)

################################################################################

## Read in data
states.energy <- read.csv("~/Energy_Analysis/data/usa_states_energy.csv")
states.energy$State <- as.factor(states.energy$State)

################################################################################

## GLM

## Partial Pooling
glm.model <- glmer(GDP.growth ~ hydro.growth + solar.growth + coal.growth + wind.growth +
                 (hydro.growth | State),
               family=binomial("logit"), data=states.energy)

## Make sjPlot for random effects
sjp.glmer(glm.model, type="re", y.offset = .4)

## Make sjPlot for fixed effects
sjp.glmer(glm.model, type="fe", y.offset = .4)

## Make sjPlot for random effects
sjp.glmer(glm.model, type="eff", y.offset = .4)

################################################################################

## LM

## Partial Pooling
contin.model <- lmer(GDP.growth ~ hydro.growth + solar.growth + coal.growth + wind.growth +
                    (hydro.growth | State), data=states.energy)

## Make sjPlot for random effects
plot_model(contin.model, type="re", y.offset=.4)
