## Model Building

################################################################################

## Load libraries
library(lme4)
library(sjPlot)
library(ggplot2)
library(dplyr)
library(blme)

################################################################################

## Read in data
states.energy <- read.csv("~/Energy_Analysis/data/usa_states_energy.csv")
states.energy$State <- as.factor(states.energy$State)
states.energy$GDP.growth <- as.factor(states.energy$GDP.growth)
states.energy$renewable <- rowSums(states.energy[, c(6, 7, 10, 11)])
states.energy$non_renewable <- rowSums((states.energy[, c(4, 5, 8, 9)]))

################################################################################

## GLM

## Partial Pooling
glm.model <- glmer(GDP.growth ~ -1 + hydro.growth + (1 | State),
               family=binomial("logit"), data=states.energy)

## Make sjPlot for random effects
sjp.glmer(glm.model, type="re", y.offset = .4)

## Make sjPlot for fixed effects
sjp.glmer(glm.model, type="fe", y.offset = .4)

## Make sjPlot for random effects
sjp.glmer(glm.model, type="eff", y.offset = .4)

################################################################################


summary(lm(GDP.total~HYTCB, data=states.energy[states.energy$State == "WA",]))

## LM

## Partial Pooling
contin.model <- lmer(log(GDP.total) ~ -1 + CLTCB + HYTCB + (1 | State), data=states.energy)

## Make sjPlot for random effects
plot_model(contin.model, sort.est="(Intercept)", type="re", y.offset=.4)

## Make sjPlot for fixed effects
sjp.glmer(contin.model, type="fe", y.offset = .4)

################################################################################

## Plot GDP
ggplot(states.energy, aes(x=GDP.total, fill=State)) + geom_bar() +
  coord_flip() + theme(legend.position = 'none')

## Plot GDP.growth
ggplot(states.energy, aes(State, fill=GDP.growth)) + geom_bar() + coord_flip()

## Plot hydro.growth
ggplot(states.energy, aes(HYTCB, log(GDP.total), color=State)) +
    geom_point(size = 5) +
    geom_smooth(aes(color=State), se=FALSE)


newer <- filter(states.energy, states.energy$State == "MO")

ggplot(states.energy, aes(log(hydro.growth), GDP)) + geom_point() +
    geom_smooth(aes(color=State), se=FALSE)

## Plot Coal.growth
ggplot(states.energy, aes(x=hydro.growth, fill=State)) +
  geom_density() + facet_wrap(~State) + theme(legend.position = 'none')

