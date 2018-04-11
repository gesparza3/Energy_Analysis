###############################################################################

# Read libraries
library(tidyr)
library(dplyr)

###############################################################################

## read in data for the U.S. and GDP
energy.data <- read.csv("/home/gesparza/Downloads/use_US.csv")
gdp.data <- read.csv("/home/gesparza/Dropbox/MATH456/Energy/GDP.csv")

###############################################################################

## Combine quaters to make it yearly
year <- 1
gdp <- data.frame(Year=numeric(), GDPI=numeric(), stringsAsFactors=FALSE) 
for(i in 1:71)
{
  gdp[i,1] <- i + 1946
  gdp[i,2] <- sum(gdp.data[year:year+4,2])
  year <- year + 4
}

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
## GDP rate of growth
usa.energy$GDPI <- gdp[14:69,2]
usa.energy <- mutate(usa.energy, GDP.growth = (round(GDPI/lag(GDPI, 1), 2)))

## Tranform year variable
usa.energy$year <- gsub("X", "", usa.energy$year)
usa.energy$year <- as.numeric(usa.energy$year)

###############################################################################

## Replace NA's with 0's
usa.energy[is.na(usa.energy)] <- 0
usa.energy <- usa.energy[is.infinite(usa.energy[,8]) == TRUE] <- 0
usa.energy <- usa.energy[is.infinite(usa.energy[,9]) == TRUE] <- 0

## Write to csv
write.csv(usa.energy, "~/Energy_Analysis/data/usa_energy.csv")
