# Copyright (C) 2023 Macarena Picazo Mora
# Examining the effect of the Refugee Crisis. The relationship between attitudes toward Immigration and public Euroscepticism in EU member states

library(tidyverse)
library(haven)
library(ggplot2)
library(gridExtra)
library(svglite)
library(car)

data <- read_csv("./data/ESS6e02_5/ESS6e02_5.csv")


# Convert categorical variables into factors
data$gender <- as.factor(data$gndr)
data$country <- as.factor(data$cntry)
data$year <- as.factor(data$inwyys)
data$age <- data$agea
data$education <- data$education
data$immigration_attitude <- data$imwbcnt

data$eu_integration <- data$euftf


# education
# income
# urbanization
# political_interest
# democracy_satisfaction
# unemployment
# political_ideology


# Model 1: Baseline model
model1 <- lm(eu_integration ~ immigration_attitude + gender + age + education + income + urbanization + political_interest + democracy_satisfaction + unemployment + political_ideology, data = data)

summary(model1)

# Model 2: Adding country dummies
model2 <- lm(eu_integration ~ immigration_attitude + gender + age + education + income + urbanization + political_interest + democracy_satisfaction + unemployment + political_ideology + country, data = data)

summary(model2)

# Create interaction terms
data$immigration_year_interaction <- data$immigration_attitude * data$year

# Model 3: Adding time dummies and interaction term, without country dummies
model3 <- lm(eu_integration ~ immigration_attitude + gender + age + education + income + urbanization + political_interest + democracy_satisfaction + unemployment + political_ideology + year + immigration_year_interaction, data = data)

summary(model3)

# Model 4: Adding time dummies and interaction term, with country dummies
model4 <- lm(eu_integration ~ immigration_attitude + gender + age + education + income + urbanization + political_interest + democracy_satisfaction + unemployment + political_ideology + country + year + immigration_year_interaction, data = data)

summary(model4)
