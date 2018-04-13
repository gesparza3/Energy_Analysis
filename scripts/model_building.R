## Model Building

################################################################################

## Load libraries
library(lme4)
library(sjPlot)
library(ggplot2)
library(dplyr)

################################################################################

## Read in data
states.energy <- read.csv("~/Energy_Analysis/data/usa_states_energy.csv")
states.energy$State <- as.factor(states.energy$State)
states.energy$GDP.growth <- as.factor(states.energy$GDP.growth)

################################################################################

## GLM

## Partial Pooling
glm.model <- glmer(GDP.growth ~ -1 + motor_gas.growth + (1 | State),
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
contin.model <- lmer(log(GDP) ~ hydro.growth + (hydro.growth | State), 
                     data=states.energy)

## Make sjPlot for random effects
plot_model(contin.model, sort.est="(Intercept)", type="re", y.offset=.4)

## Make sjPlot for fixed effects
sjp.glmer(contin.model, type="fe", y.offset = .4)

################################################################################

## Plot GDP
ggplot(states.energy, aes(x=GDP.growth, fill=State)) + geom_bar() +
  coord_flip() + theme(legend.position = 'none')

## Plot GDP.growth
ggplot(states.energy, aes(State, fill=GDP.growth)) + geom_bar() + coord_flip()

## Plot hydro.growth
newer <- filter(states.energy, states.energy$State == "MI")
ggplot(newer, aes(hydro.growth)) + geom_histogram(bins=10)

## Plot Coal.growth
ggplot(states.energy, aes(x=hydro.growth, fill=State)) +
  geom_density() + facet_wrap(~State) + theme(legend.position = 'none')

################################################################################

## Fixed Effect model
model.glm <- glm(GDP.growth ~ coal.growth + State, family="binomial",
                 states.energy)
anova(model.glm)
################################################################################

theta <- getME(glm.model,"theta")
## diagonal elements are identifiable because they are fitted
##  with a lower bound of zero ...
diag.element <- getME(glm.model,"lower")==0
any(theta[diag.element]<1e-5)
