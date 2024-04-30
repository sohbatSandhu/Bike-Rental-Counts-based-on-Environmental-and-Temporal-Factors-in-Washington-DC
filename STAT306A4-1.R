# STAT 306 GROUP PROJECT
## Group A4: Gina Choi, Harnoor Shinh, 
##           Jasper Law, Sohbat Sandhu

## Install Packages if necessary
# install.packages("corrplot")
# install.packages("GGally")
# install.packages("leaps")
# install.packages("tidyverse")
# install.packages("car")

# Load Useful Libraries
library(dplyr)
library(corrplot)
library(GGally)
library(leaps)
library(tidyverse)
library(car)

#load dataset
bike <- read.csv("~/Downloads/day.csv")

# Calculate correlation matrix for EDA
bike= subset(bike, select = -c(instant,dteday))
correlation_matrix <- cor(bike)
corr_plot <- corrplot(correlation_matrix, method = "color", order = "hclust", 
                      addCoef.col = 1, tl.cex = 1, number.cex = 1, tl.srt = 35)

#convert categorical variables to factors
bike$season <- factor(bike$season,levels=c(1,2,3,4),labels=c("Winter","Spring",
                                                             "Summer","Fall"))
bike$yr <- factor(bike$yr,levels=c(0,1),labels=c(2011,2012))
bike$mnth <- factor(bike$mnth,
                    levels=c(1:12),
                    labels=c("Jan","Feb", "Mar", "Apr", "May", "Jun", "Jul", 
                             "Aug", "Sep", "Oct", "Nov", "Dec"))
bike$holiday <- factor(bike$holiday,levels=c(0,1),
                       labels=c("NonHoliday","Holiday"))
bike$weekday <- factor(bike$weekday,
                       levels=c(0,1,2,3,4,5,6),
                       labels=c("Sun","Mon", "Tues", "Weds", 
                                "Thurs", "Fri", "Sat"))
bike$workingday <- factor(bike$workingday,
                          levels=c(1,0),
                          labels=c("WorkingDay", "WeekendOrHoliday"))
bike$weathersit <- factor(bike$weathersit,
                          levels=c(1,2,3),
                          labels=c("Clear/PartlyCloudy", 
                                   "Misty/Cloudy", "LightSnow/Rain/Thunderstorm"))

# casual and registered variables have to be removed as we get a perfit fitting
# model as casual + registered = cnt
Overall = subset(bike, select = -c(registered,casual))
head(Overall, 2)

#Plotting for Overall
eda1 = boxplot(Overall$cnt ~ Overall$workingday, 
        main = "Type of Day vs. Total Bikes Rented in a Day",
        xlab = "Type of Weather",
        ylab = "Total Bikes Rented in a Day")
eda2 = boxplot(Overall$cnt ~ Overall$weathersit, 
        main = "Type of Weather vs. Total Bikes Rented in a Day",
        xlab = "Type of Day",
        ylab = "Total Bikes Rented in a Day")
eda3 = boxplot(Overall$cnt ~ Overall$season, 
        main = "Season vs. Total Bikes Rented in a Day",
        xlab = "Seasons",
        ylab = "Total Bikes Rented in a Day")
eda4 = boxplot(Overall$cnt ~ Overall$weekday, 
        main = "Day of the Week vs. Total Bikes Rented in a Day",
        xlab = "Day of the Week",
        ylab = "Total Bikes Rented in a Day")
eda5 = boxplot(Overall$cnt ~ Overall$mnth, 
        main = "Month vs. Total Bikes Rented in a Day",
        xlab = "Month",
        ylab = "Total Bikes Rented in a Day")
eda6 = boxplot(Overall$cnt ~ Overall$holiday, 
        main = "Holiday vs. Total Bikes Rented in a Day",
        xlab = "Type of Day",
        ylab = "Total Bikes Rented in a Day")
eda7 = boxplot(Overall$cnt ~ Overall$yr, 
        main = "Year vs. Bikes Rented in a Day",
        xlab = "Year",
        ylab = "Total Bikes Rented in a Day")

# As mnth and weekday variables do not show actual variation and as their information
# is also accounted for in the holiday variable
Overall <- subset(Overall, select = -c(mnth,weekday))
head(Overall, 3)

# perform VIF on all the continuous variables, since we find the VIFs of
# temp and atemp to be greater than 10, we observe multicollinearity we remove 
# temp (as atemp is more relevent to our analysis) and perform VIF again
var1 <- Overall |> select(c(temp,atemp, hum, windspeed, cnt))
MLR_1 <- lm(cnt ~ ., data = var1)
vif1 <- vif(MLR_1)
vif1

# Multi-collinearity issue resolved
var2 <- Overall |> select(c(atemp, hum, windspeed, cnt))
MLR_2 <- lm(cnt ~ ., data = var2)
vif2 <- vif(MLR_2)
vif2

Overall = subset(Overall, select = -c(temp))

# Split Data to train/test split for model selection and apply Validation 
# Set Approach for Cross-validation
set.seed(3679)
ID = sample.int(n = nrow(Overall), 
                 size = floor(0.7*nrow(Overall)), replace = F)
training_set = Overall[ID, ]
testing_set = Overall[-ID, ]

# Apply exhaustive stepwise selection algorithm and summarize results
interactionFormula = cnt ~ yr + workingday + weathersit + season*holiday + atemp*hum + 
  atemp*windspeed + hum*windspeed + atemp*season
exhaustive_sel = regsubsets(interactionFormula, data=training_set,
                            method="exhaustive", nvmax = 20)
ex_summary = summary(exhaustive_sel)

# Display model metrics
ex_summary_table = data.frame("n_input_variables" = 1:20,
                              "BIC" = ex_summary$bic,
                              "AdjR2" = ex_summary$adjr2,
                              "Cp" = ex_summary$cp)
ex_summary_table

# Select variables for best performance by accessing Cp and adjusted R^2 values
# Cp approximately equals p, decrease model complexity and 
selected_var <- names(coef(exhaustive_sel, 13))[-1]
selected_var 

# create subset of the training data from the selected variables
encoded_data <- model.matrix(~ . - 1 + as.factor(yr) + as.factor(weathersit) + as.factor(season) 
                             + as.factor(workingday) + season*holiday + atemp*hum + atemp*windspeed 
                             + hum*windspeed + atemp*season, 
                             data = training_set)
df_train <- as.data.frame(encoded_data)
training_subset <- df_train %>% select(all_of(selected_var),cnt)

# create subset of the testing data from the selected variables
encoded_test <- model.matrix(~ . - 1 + as.factor(yr) + as.factor(weathersit) + as.factor(season) 
                             + as.factor(workingday) + season*holiday + atemp*hum + atemp*windspeed 
                             + hum*windspeed + atemp*season, data = testing_set)
df_test <- as.data.frame(encoded_test)
testing_subset <- df_test %>% select(all_of(selected_var),cnt)


#Fit our reduced model
reduced_MLR <- lm(cnt ~ ., training_subset)
summary(reduced_MLR)

# Plot graphs to check for violations in linear model assumptions
options(repr.plot.width=20, repr.plot.height=20)

# Assess Linearity Assumption
assumption1 = plot(y = reduced_MLR$fitted.values, x = training_subset$cnt,
                   ylab = "Fitted Values", xlab = "Actual values", 
                   abline(a = 0, b = 1, col = "red"))

# Assess Homoscedasticity or Constancy of variance assumption
assumption2 = plot(reduced_MLR, 3)

# Function to calculate RMSE
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Predict testing data using the reduced model
reduced_MLR_predictions <- predict(reduced_MLR, testing_subset)

# Predict testing data using the full model
full_MLR <- lm(interactionFormula, training_set)
full_MLR_predictions <- predict(full_MLR, testing_set)

# Compare results of RMSE of the reduced and full model to see if the reduced model is better
options(digits = 6)
results <- rbind(tibble(
  Model = "Full Model",
  RMSE = rmse(testing_set$cnt, full_MLR_predictions)),
  tibble(
    Model = "Reduced Model using Exhaustive Selection",
    RMSE = rmse(testing_subset$cnt, reduced_MLR_predictions)))
results

