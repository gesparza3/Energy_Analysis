## read in data for the U.S. and GDP
energy.data <- read.csv("/home/gesparza/Downloads/use_US.csv")
gdp.data <- read.csv("/home/gesparza/Dropbox/MATH456/Energy/GDP.csv")
gdp <- as.vector(gdp.data$GPDI, mode = "numeric")
gdp
## Select only the rows with the data we are concerned with
energy.types <- c("CLTCB", "HYTCB", "NGTCB", "SOTCB", "WYTCB", "GETCB", "CLTCB",
                 "MGTCB", "FFTCB")

## Total consumption:
## CLTCB - Coal
## HYTCB - Hydro
## NGTCB - Natural Gas
## SOTCB - Solar
## WYTCB - Wind
## GETCB - Geothermal
## MGTCB - Motor gas
## FFTCB - Fossil Fuel

usa.energy.pre_clean <- energy.data[energy.data$MSN %in% energy.types,]

## Transform wide data to long data
library(tidyr)
usa.energy.long <- usa.energy.pre_clean %>% gather(year, energy_consumed, X1960:X2015)
tail(usa.energy.long)
head(usa.energy.long)

## Transform long to wide

usa.energy.wide <- spread(usa.energy.long, MSN, energy_consumed)
head(usa.energy.wide)

## Create % growth variables
library(dplyr)
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
usa.energy$GDPI <- gdp
usa.energy <- mutate(usa.energy, GDP.growth = (round(GDPI/lag(GDPI, 1), 2)))
## Tranform year variable
usa.energy$year <- gsub("X", "", usa.energy$year)
usa.energy$year <- as.numeric(usa.energy$year)
head(usa.energy)

## Plot some data
library(ggplot2)
## Two on same plot
ggplot(usa.energy[32:56,], aes(year)) +
  geom_line(aes(y = solar.growth), color = "#68382C", size = 1.25) +
  geom_line(aes(y = wind.growth), color = "#000080", size = 1.25) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "gray50", size = 0.5),
        panel.grid.major.x = element_blank()) + 
  scale_x_continuous(breaks = c(seq(1960, 2015, 5), 2015)) +
  scale_y_continuous(sec.axis = sec_axis(~.*1,name = "Motorgas consumption")) +
  ggtitle("Fossil Fuel Consumption")

## First attempt
ggplot(usa.energy, aes(year, solar.growth, color = "darkred", group = 1)) + 
  geom_point(size = 3) +
  ##geom_line(color = "darkred") +
  ##geom_point(aes(year, coal.growth, group = 1), color = "darkblue", size = 3) +
##  geom_line(color = "darkblue") +
  theme(axis.text.x = element_text(angle = 80, hjust = 1)) +
  geom_hline(yintercept = 1.00)
