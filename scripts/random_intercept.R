## Model Building with Random Intercepts
################################################################################

## Load libraries
library(lme4)
library(sjPlot)

################################################################################

## Read data
states.energy <- read.csv("~/Energy_Analysis/data/usa_states_energy.csv")
usa.energy <- read.csv("~/Energy_Analysis/data/usa_energy.csv")

################################################################################

## Add US GDP variables

################################################################################

## Model
summary(lm(GDP.total~HYTCB, data=states.energy[states.energy$State == "WA",]))

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

## Data selected by state
newer <- filter(states.energy, states.energy$State == "MO")

ggplot(states.energy, aes(log(hydro.growth), GDP)) + geom_point() +
    geom_smooth(aes(color=State), se=FALSE)

## Plot Coal.growth
ggplot(states.energy, aes(x=hydro.growth, fill=State)) +
  geom_density() + facet_wrap(~State) + theme(legend.position = 'none')
