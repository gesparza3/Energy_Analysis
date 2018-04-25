################################################################################

## Load libraries
library(dplyr)
library(tidyr)

################################################################################

## Read data
og.usa <- read.csv("~/Energy_Analysis/data/usa_states_energy.csv")

################################################################################

## Transform to wide
usa.energy.wide <- og.usa[,c("State", "year", "HYTCB")]
usa.energy.wide <- spread(usa.energy.wide, year, HYTCB)

################################################################################

## Create correlation matrix
W <- cor(usa.energy.wide[,c(2:20)])
corrplot::corrplot(cor(usa.energy.wide[c(1:19), c(2:20)]), method="number", number.cex=.5)

#diag(cor(usa.energy[c(1:19), c(2:20)])) <- #950?

W <- diag(950)

#corrplot::corrplot(W)

for (i in 1:49)
{
    W[c(1:19)+i*19,c(1:20)+i*19] <- cor(usa.energy.wide[c(1:19), c(2:20)])
    i = i + 19
}

################################################################################

## Write to csv
write.csv(usa.energy.wide, "~/Energy_Analysis/data/usa.energy.wide.csv")
