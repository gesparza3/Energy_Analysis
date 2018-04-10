library(tidyr)
library(dplyr)
library(glmulti)
library(ggplot2)
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

## Make usa.energy binary
energy.binary <- select(usa.energy, c("year", "coal.growth", "fossil_fuel.growth", 
                                      "geothermal.growth", "hydro.growth", 
                                      "motor_gas.growth", "natural_gas.growth", 
                                      "solar.growth", "wind.growth", "GDP.growth"))

## Convert variables
energy.binary[,2:9] <- as.data.frame(ifelse(energy.binary[,2:9] > 1, 1, 0))
energy.binary[,2:9] <- lapply(energy.binary[,2:9], as.factor)
energy.binary[,10] <- as.data.frame(ifelse(energy.binary[,10] > mean(usa.energy$GDP.growth), 1, 0))

###############################################################################

## Glmulti function
best.mods <- glmulti::glmulti(GDP.growth ~ geothermal.growth + hydro.growth + 
                                solar.growth + wind.growth + coal.growth + 
                                fossil_fuel.growth + geothermal.growth + 
                                hydro.growth + motor_gas.growth + 
                                natural_gas.growth, data = energy.binary, 
                              level = 1, method = "h", crit = "bic", 
                              confsetsize = 5, plotty = F, report = F, 
                              fitfunction = "glm", family = binomial)
best.mods@formulas

## Run the model
mod <- glm(GDP.growth ~ motor_gas.growth, data = energy.binary, family = "binomial")
summary(mod)
MKmisc::HLgof.test(fit = fitted(mod.1), obs = mod.1$y)

###############################################################################

## Bar Chart for Coal
ggplot(energy.binary, aes(coal.growth)) + geom_bar()

## Two on same plot
ggplot(usa.energy[32:56,], aes(year)) +
  geom_line(aes(y = solar.growth), color = "#68382C", size = 1.25) +
  geom_line(aes(y = wind.growth), color = "#000080", size = 1.25) +
  scale_x_continuous(breaks = c(seq(1960, 2015, 5), 2015)) +
  scale_y_continuous(sec.axis = sec_axis(~.*1,name = "Motorgas consumption")) +
  ggtitle("Fossil Fuel Consumption")
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "gray50", size = 0.5),
        panel.grid.major.x = element_blank())
 
