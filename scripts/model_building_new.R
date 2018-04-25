## New attempts post-random-intercepts

################################################################################

## Load libraries
library(dplyr)
library(ggplot2)

################################################################################

## Pick top states from each category
usa.energy <- read.csv("~/Energy_Analysis/data/usa_states_energy.csv")

head(usa.energy[usa.energy$State == "OR",])

OR.data <- usa.energy[usa.energy$State == "OR", c(4:11, 20)]

#Select Oregon
OR.mod <- lm(GDP ~ CLTCB, data=OR.data)
summary(OR.mod)

states <- levels(usa.energy$State)

model_results <- data.frame(adj_r_sqrd=numeric(),
                            inter.coeff=numeric(),
                            coeff=numeric(),
                            State=character(),
                            stringsAsFactors=FALSE)

for (i in 1:50)
{
    st.data <- usa.energy[usa.energy$State == states[i],c(4:11,20)]
    mod <- lm(GDP ~ HYTCB, data=st.data)
    model_results[i,1] <- summary(mod)$adj.r.squared
    model_results[i,2] <- summary(mod)$coefficients[1,4]
    model_results[i,3] <- summary(mod)$coefficients[2,4]
    model_results[i,4] <- states[i]

}

st.data <- usa.energy[usa.energy$State == states[1],c(4:11,20)]
    mod <- lm(GDP ~ HYTCB, data=st.data)
    model_results[1,1] <- summary(mod)$adj.r.squared
    model_results[1,2] <- summary(mod)$coefficients[1,4]
    model_results[1,3] <- summary(mod)$coefficients[2,4]
    model_results[1,4] <- states[1]

write.csv(model_results,"~/Energy_Analysis/data/results.csv")
