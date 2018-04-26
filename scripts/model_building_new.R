## New attempts post-random-intercepts

################################################################################

## Load libraries
library(dplyr)
library(ggplot2)

################################################################################

## Pick top states from each category
usa.energy <- read.csv("~/Energy_Analysis/data/usa_states_energy.csv")
country.energy <- read.csv("~/Energy_Analysis/data/usa_energy.csv")

usa.energy$GDP.total <- country.energy[38:56, 21]

head(usa.energy[usa.energy$State == "OR",])

OR.data <- usa.energy[usa.energy$State == "OR", c(4:11, 20)]

#Select Oregon
OR.mod <- lm(GDP.t ~ CLTCB, data=OR.data)
summary(OR.mod)

states <- levels(usa.energy$State)

model_results <- data.frame(adj_r_sqrd=numeric(),
                            inter.coeff=numeric(),
                            coeff=numeric(),
                            State=character(),
                            stringsAsFactors=FALSE)

## LM
for (i in c(1:7, 9:24, 26:50))
{
    i
    st.data <- usa.energy[usa.energy$State == states[i],c(4:11,22)]
    mod <- lm(log(GDP.total) ~ log(HYTCB), data=st.data)
    model_results[i,1] <- summary(mod)$adj.r.squared
    model_results[i,2] <- summary(mod)$coefficients[1,4]
    # model_results[i,3] <- summary(mod)$coefficients[2,4]
    model_results[i,3] <- ifelse(nrow(summary(mod)$coefficients) < 2, 0, summary(mod)$coefficients[2,4])
    model_results[i,4] <- states[i]

}

model_results <- data.frame(adj_r_sqrd=numeric(),
                            inter.coeff=numeric(),
                            coeff=numeric(),
                            State=character(),
                            stringsAsFactors=FALSE)


## GLM
for (i in c(1:7, 9:24, 26:50))
{
    i
    st.data <- usa.energy[usa.energy$State == states[i],c(4:22)]
    mod <- glm(GDP.total ~ HYTCB, data=st.data, family="binomial")
    # model_results[i,1] <- summary(mod)$adj.r.squared
    model_results[i,2] <- summary(mod)$coefficients[1,4]
    model_results[i,3] <- summary(mod)$coefficients[2,4]
    # model_results[i,3] <- ifelse(nrow(summary(mod)$coefficients) < 2, 0, summary(mod)$coefficients[2,4])
    model_results[i,4] <- states[i]

}



st.data <- usa.energy[usa.energy$State == states[8],c(11:21)]
model_results[i,1] <- summary(mod)$adj.r.squared
model_results[1,1] <- summary(mod)$adj.r.squared
model_results[1,2] <- summary(mod)$coefficients[1,4]
model_results[1,3] <- summary(mod)$coefficients[2,4]
model_results[1,4] <- states[1]

# Make density plot

ggplot(usa.energy, aes(log(HYTCB), fill=State)) + geom_density(alpha=.5)


write.csv(usa.energy, "~/Energy_Analysis/data/usa_states_energy.csv")

