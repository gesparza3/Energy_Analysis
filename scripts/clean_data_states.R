## GDP data from Bureau of Economic Analysis

###############################################################################

# Read libraries
library(tidyr)
library(dplyr)

###############################################################################

## read in data for the U.S. and GDP
energy.data <- read.csv("~/Energy_Analysis/data/use_all_btu.csv")

###############################################################################

## Clean GDP
gdp1 <- read.csv("~/Energy_Analysis/data/gdpstate_naics_all_R.csv")
 
## Select total GDP
gdp1 <- gdp1[gdp1$Description %in% "All industry total",]

## Select only the states and years
gdp1 <- gdp1[c(2:8,10:52),]
gdp1 <- gdp1[c(2,9:27)]

## Convert to long
gdp <- gdp1 %>% gather(GeoName, year, X1997:X2015)
names(gdp) <- c("State", "Year", "GDP")
gdp$Year <- gsub("X", "", gdp$Year)
gdp$Year <- as.numeric(gdp$Year)
gdp$GDP <- as.numeric(gdp$GDP)

###############################################################################

## Total consumption:
## CLTCB - Coal
## HYTCB - Hydro
## NGTCB - Natural Gas
## SOTCB - Solar
## WYTCB - Wind
## GETCB - Geothermal
## MGTCB - Motor gas
## FFTCB - Fossil Fuel

## Select only the rows with the data we are concerned with
energy.types <- c("CLTCB", "HYTCB", "NGTCB", "SOTCB", "WYTCB", "GETCB", "CLTCB",
                  "MGTCB", "FFTCB")
usa.energy.pre_clean <- energy.data[energy.data$MSN %in% energy.types,]

## Transform wide data to long data
usa.energy.long <- usa.energy.pre_clean %>% gather(year, energy_consumed, X1960:X2015)

## Transform long to wide
usa.energy.wide <- spread(usa.energy.long, MSN, energy_consumed)

###############################################################################

## Create % growth variables
usa.energy <- usa.energy.wide
## Coal rate of growth
usa.energy <- mutate(usa.energy, coal.growth = (round(CLTCB/lag(CLTCB, 1), 2)))
## Fossil fuel rate of growth
usa.energy <- mutate(usa.energy, fossil_fuel.growth = (round(FFTCB/lag(FFTCB, 1), 2)))
## Geothermal rate of growth
usa.energy <- mutate(usa.energy, geothermal.growth = (round(GETCB/lag(GETCB, 1), 2)))
## Hydro rate of growth
usa.energy <- mutate(usa.energy, hydro.growth = (round(HYTCB/lag(HYTCB, 1), 2)))
## Motorgas rate of growth
usa.energy <- mutate(usa.energy, motor_gas.growth = (round(MGTCB/lag(MGTCB, 1), 2)))
## Natural Gas rate of growth
usa.energy <- mutate(usa.energy, natural_gas.growth = (round(NGTCB/lag(NGTCB, 1), 2)))
## Solar rate of growth
usa.energy <- mutate(usa.energy, solar.growth = (round(SOTCB/lag(SOTCB, 1), 2)))
## Wind rate of growth
usa.energy <- mutate(usa.energy, wind.growth = (round(WYTCB/lag(WYTCB, 1), 2)))

## Tranform year variable
usa.energy$year <- gsub("X", "", usa.energy$year)
usa.energy$year <- as.numeric(usa.energy$year)

## Select 1997-2015
usa.energy <- usa.energy[usa.energy$year %in% c(1997:2015),]

## Remove US and Data Status
usa.energy <- filter(usa.energy, State != "US")
usa.energy <- filter(usa.energy, State != "DC")
usa.energy <- usa.energy[,2:19]

## Add GDP
usa.energy$GDP <- gdp[,3]
usa.energy <- mutate(usa.energy, GDP.growth = (round(GDP/lag(GDP, 1), 2)))

## Make State a factor

###############################################################################

## Replace NA's with 0's
usa.energy[is.na(usa.energy)] <- 0
usa.energy[sapply(usa.energy, simplify = 'matrix', is.infinite)] <- 0
usa.energy[,20] <- as.data.frame(ifelse(usa.energy[,20] > 1.03, 1, 0))

## Write to csv
write.csv(usa.energy, "~/Energy_Analysis/data/usa_states_energy.csv")
