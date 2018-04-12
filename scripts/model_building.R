## Model Building

################################################################################

## Load libraries
library(lme4)
library(sjPlot)
library(ggplot2)

################################################################################

## Read in data
states.energy <- read.csv("~/Energy_Analysis/data/usa_states_energy.csv")
states.energy$State <- as.factor(states.energy$State)
states.energy$GDP.growth <- as.factor(states.energy$GDP.growth)

################################################################################

## GLM

## Partial Pooling
glm.model <- glmer(GDP.growth ~ hydro.growth + (1 | State),
               family=binomial("logit"), data=states.energy)

## Make sjPlot for random effects
plot_model(glm.model, type="re", y.offset = .4)

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
ggplot(states.energy, aes(x=GDP, fill=State)) + geom_bar() +
  coord_flip() + theme(legend.position = 'none')

## Plot GDP.growth
ggplot(states.energy, aes(State, fill=GDP.growth)) + geom_bar() + coord_flip()

## Plot hydro.growth
ggplot(states.energy, aes(log(hydro.growth))) + geom_histogram()

## 
ggplot(states.energy, aes(x=State, y=log(coal.growth), fill=State)) + 
  geom_boxplot() + coord_flip() + theme(legend.position = 'none')