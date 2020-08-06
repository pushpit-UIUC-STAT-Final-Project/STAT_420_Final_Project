library(gridExtra)
library(reshape2)
library(tidyverse)
library(car)

who = read.csv("LifeExpectancyData.csv")
View(who)
nrow(who)

# Data Cleanup
who = who %>% 
  drop_na() %>% 
  filter(!is.na(Life.expectancy), !is.na(Adult.Mortality), !is.na(infant.deaths))

nrow(who)

# Model 1
model1 = lm(Life.expectancy ~ Schooling + Income.composition.of.resources 
            + Total.expenditure + percentage.expenditure + BMI + Alcohol, data=who)

summary(model1)

# Model 2
model2 = step(model1, data = who, direction = "both", test="F")
summary(model2)

# Model 3
model3 = lm(Life.expectancy ~ Schooling + Income.composition.of.resources 
            + Total.expenditure + percentage.expenditure 
            + BMI + Alcohol + GDP, data = who )

anova(model3, model2)

# Model 4
model4 = step(model3, data=who, direction = "both", test="F")
summary(model4)

plot(model4)
