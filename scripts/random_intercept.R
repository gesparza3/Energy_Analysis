## Model Building with Random Intercepts
################################################################################

## Load libraries
library(lme4)
library(sjPlot)
library(dplyr)
library(ggplot2)

################################################################################

## Read data
states.energy <- read.csv("~/Energy_Analysis/data/usa_states_energy.csv")
usa.energy <- read.csv("~/Energy_Analysis/data/usa_energy.csv")

################################################################################

states.energy <- states.energy %>% select(c(year, State, CLTCB, FFTCB, GETCB, HYTCB,
                                          MGTCB, NGTCB, SOTCB, WYTCB, GDP,
                                          GDP.total))
names(states.energy)[11] <- "GDP.state"
states.energy$GDP.total.growth <- ifelse(usa.energy[38:56, 22] > 1.03,
                                         1,0)

names(states.energy)
#  [1] "year"      "State"     "CLTCB"     "FFTCB"     "GETCB"    
#  [6] "HYTCB"     "MGTCB"     "NGTCB"     "SOTCB"     "WYTCB"    
# [11] "GDP.state" "GDP"      

################################################################################

## Create renewable vs non renewable
states.energy$renewable <- rowSums(states.energy %>% 
                                   select(HYTCB, SOTCB, WYTCB, GETCB))
states.energy$non_renewable <- rowSums(states.energy %>% 
                                   select(CLTCB, FFTCB, MGTCB, NGTCB))

## Model
# summary(lm(GDP.total~HYTCB, data=states.energy[states.energy$State == "WA",]))

## Take the log of each variable
for (i in 1:8){
    states.energy[, i+2] <- log(states.energy[,i+2] + 1)
}
states.energy$GDP.total <- log(states.energy$GDP.total)
states.energy$GDP.state <- log(states.energy$GDP.state)
states.energy$non_renewable <- log(states.energy$non_renewable)
states.energy$renewable <- log(states.energy$renewable)

## Partial Pooling
contin.model <- lmer(GDP.state ~ -1 + renewable + non_renewable + (1 | State), 
                     data=states.energy)

contin.model1 <- lmer(GDP.state ~ -1 + CLTCB + FFTCB + MGTCB + NGTCB+
                      GETCB + HYTCB + SOTCB + WYTCB + (1 | State), 
                     data=states.energy)


## Make sjPlot for random effects
plot_model(contin.model1, sort.est="(Intercept)", type="re", y.offset=.4)

## Make sjPlot for fixed effects
plot_model(contin.model, type="est", y.offset = .4)

sjt.lm(contin.model1, show.r2 = FALSE) 

knitr::kable(contin.model1)

################################################################################

## Plot 
ggplot(states.energy) +
  geom_density(aes(x=CLTCB), fill="black", alpha=.5) +
  geom_density(aes(x=FFTCB), fill="brown", alpha=.5) + 
  geom_density(aes(x=GETCB), fill="green", alpha=.5) +
  geom_density(aes(x=HYTCB), fill="blue", alpha=.5) +
  geom_density(aes(x=MGTCB), fill="gray", alpha=.5) +
  geom_density(aes(x=NGTCB), fill="yellow", alpha=.5) +
  geom_density(aes(x=SOTCB), fill="orange", alpha=.5) +
  geom_density(aes(x=WYTCB), fill="light blue", alpha=.5) +
  facet_wrap(~State)

################################################################################

# Factor Analysis
library(corrplot)
library(psych)

states.dat <- states.energy %>% select(c(CLTCB, FFTCB, GETCB, HYTCB, MGTCB,
                                         NGTCB, SOTCB, WYTCB, GDP.state))

extract_pca <- princomp(states.dat, cor=TRUE)
var_pc <- (extract_pca$sdev)^2
qplot(x=1:length(var_pc), y=var_pc, geom=c("point", "line")) +
    xlab("PC number") + ylab("Eigenvalue")

pc.model.oblimin <- principal(states.dat, nfactors=2, rotate="oblimin",
                              scores=TRUE)
print(pc.model.oblimin)

load <- pc.model.oblimin$loadings
plot(load, type="n")
text(load, labels=rownames(load))

ggplot(states.energy, aes(HTYCB, GDP.state)) + 
    geom_point()
