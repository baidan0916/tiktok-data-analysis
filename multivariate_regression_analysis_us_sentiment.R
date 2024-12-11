# Load required libraries
library(readxl)
library(stats)
library(base)
library(dplyr)
library(zoo)
library(quanteda)
library(lmtest)
library(jtools)
library(kableExtra)

# Load dependent variable: monthly positive sentiment percentages
sentiment_data <- read.csv("D:/Surface 文件备份/Study/【博士】埃塞克斯/PhD_Dissertation/Drafts/TikTok Data/monthly_sentiment_us_dv.csv")

# Load independent variables
# Independent variable: covid death cases (last day of the month)
covid_deaths <- read_excel("D:/Surface 文件备份/Study/【博士】埃塞克斯/PhD_Dissertation/Drafts/Secondary Data/Covid Death Cases/covid_death_us.xlsx") %>%
  group_by(year = as.numeric(format(date, "%Y")), month = as.numeric(format(date, "%m"))) %>%
  filter(date == max(date)) %>%
  summarize(total_deaths = last(total_cases))

# Independent variable: workplace closures (sum per month)
workplace_closures <- read_excel("D:/Surface 文件备份/Study/【博士】埃塞克斯/PhD_Dissertation/Drafts/Secondary Data/Workplace Closures/workplace_closure_us.xlsx") %>%
  group_by(year = as.numeric(format(date, "%Y")), month = as.numeric(format(date, "%m"))) %>%
  summarize(workplace_closing_total = sum(workplace_closing))

# Independent variable: school closures (sum per month)
school_closures <- read_excel("D:/Surface 文件备份/Study/【博士】埃塞克斯/PhD_Dissertation/Drafts/Secondary Data/School Closures/school_closure_us.xlsx") %>%
  group_by(year = as.numeric(format(date, "%Y")), month = as.numeric(format(date, "%m"))) %>%
  summarize(school_closing_total = sum(school_closing))

# Independent variable: public transport closures (sum per month)
public_transport <- read_excel("D:/Surface 文件备份/Study/【博士】埃塞克斯/PhD_Dissertation/Drafts/Secondary Data/Public Transport/public_transport_us.xlsx") %>%
  group_by(year = as.numeric(format(date, "%Y")), month = as.numeric(format(date, "%m"))) %>%
  summarize(public_transport_closing_total = sum(public_transport_closing))

# Independent variable: economic decline (monthly probability)
economic_decline <- read_excel("D:/Surface 文件备份/Study/【博士】埃塞克斯/PhD_Dissertation/Drafts/Secondary Data/Economic Decline/recession_us.xlsx") %>%
  group_by(year = as.numeric(format(date, "%Y")), month = as.numeric(format(date, "%m"))) %>%
  summarize(economic_decline_prob = mean(resseion_probability))

# Combine dependent and independent variables
combined_data <- sentiment_data %>%
  filter(!is.na(positive)) %>%
  mutate(year = as.numeric(format(ymd(Month), "%Y")),
         month = as.numeric(format(ymd(Month), "%m"))) %>%
  left_join(covid_deaths, by = c("year", "month")) %>%
  left_join(workplace_closures, by = c("year", "month")) %>%
  left_join(school_closures, by = c("year", "month")) %>%
  left_join(public_transport, by = c("year", "month")) %>%
  left_join(economic_decline, by = c("year", "month"))

# Remove rows with NA values
cleaned_data <- combined_data %>%
  filter(!is.na(positive) & !is.na(total_deaths) & !is.na(workplace_closing_total) &
           !is.na(school_closing_total) & !is.na(public_transport_closing_total) &
           !is.na(economic_decline_prob))

# Build the multivariate regression model
model_positive <- lm(positive ~ total_deaths + workplace_closing_total + 
                       school_closing_total + public_transport_closing_total + 
                       economic_decline_prob, data = cleaned_data)

# Summarize the model
summary(model_positive)
summ(model_positive)


########################################################################################################################################
model_test <- lm(trust ~ total_deaths + workplace_closing_total + 
                       school_closing_total + public_transport_closing_total + 
                       economic_decline_prob, data = cleaned_data)
summary(model_test)

model_fear_us<- lm(fear ~ total_deaths + public_transport_closing_total, data = cleaned_data)
summary(model_fear_us)
summ(model_fear_us)

model_anger_us<- lm(fear ~ total_deaths + public_transport_closing_total, data = cleaned_data)
summary(model_anger_us)
summ(model_anger_us)

model_anticipation_us<- lm(anticipation ~ total_deaths + public_transport_closing_total, data = cleaned_data)
summary(model_anticipation_us)
summ(model_anticipation_us)

model_joy_us<- lm(joy ~ total_deaths + public_transport_closing_total, data = cleaned_data)
summary(model_joy_us)
summ(model_joy_us)

model_sadness_us<- lm(sadness ~ total_deaths + public_transport_closing_total, data = cleaned_data)
summary(model_sadness_us)
summ(model_sadness_us)

model_surprise_us<- lm(surprise ~ total_deaths + public_transport_closing_total, data = cleaned_data)
summary(model_surprise_us)
summ(model_surprise_us)

model_trust_us<- lm(trust ~ total_deaths + public_transport_closing_total, data = cleaned_data)
summary(model_trust_us)
summ(model_trust_us)

########################################################################################################################################################################
# now load library leaps to do selection
library(leaps)

# use regsubsets() to fit various models where the number of variables is up to "nvmax"
#    By default, regsubset() performs exhaustive search. 
#    "nvmax" is the maximum number of predictors that can be used in model. 
#    "nvmax" is no more than total number of predictors. 
myfit.regsub<-regsubsets(positive ~ total_deaths + workplace_closing_total + 
                           school_closing_total + public_transport_closing_total + 
                           economic_decline_prob, data = cleaned_data, nvmax=5)
myfit.summary<-summary(myfit.regsub)

names(myfit.summary)

par(mfrow=c(2,2))


# plot rss
plot(myfit.summary$rss, xlab="Variable number", ylab="RSS", type="l")
which.min(myfit.summary$rss)# rss is the smallest when variable number is 5
points(5, myfit.summary$rss[5], col="red", cex=2, pch=5)
#RSS (Residual Sum of Squares):
#What it is: The sum of the squared differences between the observed values and the predicted values from the model.
#Purpose: It measures the total variation in the data that is not explained by the model.
#Interpretation: A smaller RSS indicates that the model fits the data better, meaning it explains more of the variability in the response variable.

# plot adjusted R-square and maximum point
plot(myfit.summary$adjr2, xlab="Variable number", ylab="Adjust R-squared", type="l")
which.max(myfit.summary$adjr2) # adjr2 is the largest when variable number is 1. 
points(1, myfit.summary$adjr2[1], col="red", cex=2, pch=5)

# plot cp and minimum point
plot(myfit.summary$cp, xlab="Variable number", ylab="Cp", type="l")
which.min(myfit.summary$cp) # cp is the smallest when variable number is 1. 
points(1, myfit.summary$cp[1], col="red", cex=2, pch=5)
#Cp (Mallow's Cp):
#What it is: A criterion used to compare regression models with different numbers of predictors.
#Purpose: It balances the trade-off between the goodness-of-fit and model complexity.
#Interpretation: A smaller Cp value indicates a better-fitting model. Ideally, the Cp value should be close to the number of predictors plus one (p+1) for an unbiased model.

# plot BIC and minimum point
plot(myfit.summary$bic, xlab="Variable number", ylab="BIC", type="l")
which.min(myfit.summary$bic) # BIC is the smallest when variable number is 1. 
points(1, myfit.summary$bic[1], col="red", cex=2, pch=5)
#BIC (Bayesian Information Criterion):
#What it is: A criterion for model selection based on likelihood and model complexity.
#Purpose: Like Cp, it balances fit and simplicity, but it penalizes model complexity more heavily than Cp.
#Interpretation: A lower BIC value indicates a better model. It is particularly useful for large datasets or when avoiding overfitting.


# Get the coefficient of the "best" model
coef(myfit.regsub, 1)

model_positive_us <- lm(positive ~ school_closing_total, data = cleaned_data)
summ(model_positive_us)

########################################################################################################################################################################
########################################################################################################################################################################
# Diagnostics and Pretty Outputs
# Variance Inflation Factor (VIF) to check multicollinearity
vif_values <- vif(model_positive)
vif_table <- kable(as.data.frame(vif_values), caption = "Variance Inflation Factor (VIF) Values") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))
print(vif_table)

# Breusch-Pagan test for homoscedasticity
bp_test <- bptest(model_positive)
bp_table <- kable(data.frame(
  Statistic = bp_test$statistic,
  `Degrees of Freedom` = bp_test$parameter,
  `P-Value` = bp_test$p.value
), caption = "Breusch-Pagan Test for Homoscedasticity") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))
print(bp_table)

# Shapiro-Wilk test for normality of residuals
shapiro_test <- shapiro.test(residuals(model_positive))
shapiro_table <- kable(data.frame(
  `W-Statistic` = shapiro_test$statistic,
  `P-Value` = shapiro_test$p.value
), caption = "Shapiro-Wilk Test for Normality") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))
print(shapiro_table)

# Durbin-Watson test for autocorrelation
dw_test <- dwtest(model_positive)
dw_table <- kable(data.frame(
  `DW Statistic` = dw_test$statistic,
  `P-Value` = dw_test$p.value
), caption = "Durbin-Watson Test for Autocorrelation") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))
print(dw_table)
