# Set working directory
setwd("/Users/Emilie/Dropbox/Skole/UC3M/Predictive modeling/Practice Workbook/Chapter 2/Data")

# Install packages
install.packages('openxlsx') 
install.packages('xtable')

# Library
library(openxlsx)
library(xtable)

options(xtable.floating = FALSE)
options(xtable.timestamp = "")

# -------------------------------------------------------------------------------------------
# CHAPTER 2: MULTIPLE LINEAR REGRESSION
# -------------------------------------------------------------------------------------------


# -------------------------------------------------------------------------------------------
# EXERCISE 1
# -------------------------------------------------------------------------------------------

# Read data from xlsx
cost = read.xlsx('cost.xlsx')

# Defining the regression variables
y = cost$cost;
x1 = cost$machinery;
x2 = cost$raw;
x3 = cost$energy;
x4 = cost$salary;

# -------------------------------------------------------------------------------------------
# (a) Make an initial exploratory plot of the data.
# -------------------------------------------------------------------------------------------
# 1. Multiple linear regression

# Scatter plot matrix
dev.off();
plot(cost, col = 'navy', pch = 16, cex = 1.1, cex.labels = 1.3, font.labels = 2, gap = 0.15,
     main = 'Scatter plot matrix: Linear regression'); 

# Fit of full multiple linear regression model
model.1 = lm(cost ~ ., data = cost); 
fit.1 = fitted(model.1);

# Test linear fit
summary(model.1);

# Residual plot for linear regression model 
dev.off();
residualPlots(model.1, type = 'rstudent', tests = FALSE, quadratic = FALSE, ask = FALSE, 
              col = 'navy', pch = 16, cex = 1.5, main = 'Residual plots: Linear regression'); 

# -------------------------------------------------------------------------------------------
# 2. Log transformation in energy and raw costs

# Scatter plot matrix
dev.off();
plot(log(cost), col = 'navy', pch = 16, cex = 1.1, cex.labels = 1.3, font.labels = 2, 
     gap = 0.15, labels = paste0('log(',colnames(cost),')'), 
     main = 'Scatter plot matrix: Log-log regression'); 

# Fit of log linear regression model
model.2 = lm(log(cost) ~ log(x1) + log(x2) + log(x3) + log(x4), data = cost); 
fit.2 = fitted(model.2);

# Test linear fit
summary(model.2);

# Residual plot for multiplicative model 
dev.off();
residualPlots(model.2, type = 'rstudent', tests = FALSE, quadratic = FALSE, ask = FALSE, 
              col = 'navy', pch = 16, cex = 1.2, main = 'Residual plots: Log-log regression'); 

# -------------------------------------------------------------------------------------------
# (d) After the decision in part (c):
# -------------------------------------------------------------------------------------------
# i. Analyze the individual significance of all the regressors.

# Look at the p-values for each variable
summary(model.2); 

# -------------------------------------------------------------------------------------------
# ii. Obtain 95% confidence intervals for all the slope parameters.

# Confidence interval for the slope parameter x
confint(model.2, 'log(x1)', level = 0.95);
confint(model.2, 'log(x2)', level = 0.95);
confint(model.2, 'log(x3)', level = 0.95);
confint(model.2, 'log(x4)', level = 0.95);

# -------------------------------------------------------------------------------------------
# iii. What would be the predicted cost for the values x1(machinery) = 80; x2(raw) =3
# 2000; x3(energy) = 1000; and x4(salary) = 25.

# Confidence and prediction intervals for specified regressor values;
exp(predict(model.2, data.frame(x1=80, x2=2000, x3=1000, x4=25), interval = 'confidence', 
            level = 0.95));

# -------------------------------------------------------------------------------------------
# EXERCISE 2
# -------------------------------------------------------------------------------------------

# Read data from xlsx
cigarettecons = read.xlsx('cigarettecons.xlsx')

# View variable names
str(cigarettecons);

# -------------------------------------------------------------------------------------------
# 1. Multiple linear regression

# Initial explanatory plot: Scatter plot matrix
dev.off();
plot(cigarettecons[,2:8], col = 'navy', pch = 16, cex = 1.1, cex.labels = 1.3, font.labels = 2, 
     gap = 0.15, main = 'Scatter plot matrix: Linear regression'); 

# Fit of full multiple linear regression model
model.1 = lm(Sales ~ Age + HS + Income + Black + Female + Price, data = cigarettecons); 
fit.1 = fitted(model.1);

# Test linear fit
summary(model.1);

# Residual plot for linear regression model 
dev.off();
residualPlots(model.1, type = 'rstudent', tests = FALSE, quadratic = FALSE, ask = FALSE, 
              col = 'navy', pch = 19, cex = 1.5, main = 'Residual plots: Linear regression'); 

# -------------------------------------------------------------------------------------------
# 2. Log transformation

# Scatter plot matrix of log transformation
dev.off();
plot(log(cigarettecons[,2:8]), col = 'navy', pch = 16, cex = 1.1, cex.labels = 1.2, 
     font.labels = 2, gap = 0.15, labels = paste0('log(',colnames(cigarettecons[,2:8]),')'), 
     main = 'Scatter plot matrix: Log-log regression');

# Fit of log linear regression model
model.2 = lm(log(Sales) ~ log(Age) + log(HS) + log(Income) + log(Black) + log(Female) 
             + log(Price), data = cigarettecons); 
fit.2 = fitted(model.2);

# Test log linear fit
summary(model.2);

# Residual plot for log linear regression model 
dev.off();
residualPlots(model.2, type = 'rstudent', tests = FALSE, quadratic = FALSE, ask = FALSE, 
              col = 'navy', pch = 19, cex = 1.5, main = 'Residual plots'); 

# -------------------------------------------------------------------------------------------
# (a) Test the hypothesis that the variable Female is not needed in the regression relating 
# Sales to the six regressor variables.
# -------------------------------------------------------------------------------------------
# 1. Reduced linear regression model

# Reduced linear regression model, excluding female
model.3 = lm(Sales ~ Age + HS + Income + Black + Price, data = cigarettecons);
fit.3 = fitted(model.3);

# Test linear fit excluding female
summary(model.3);

# Residual plot for reduced linear regression model 
dev.off();
residualPlots(model.3, type = 'rstudent', tests = FALSE, quadratic = FALSE, ask = FALSE, 
              col = 'navy', pch = 19, cex = 1.5, main = 'Residual plots'); 

# Compare the models to test hypothesis of female excluded
anova(model.3,model.1);

# Alternatively
lht(model.1, 'Female = 0');

# -------------------------------------------------------------------------------------------
# 2. Reduced log-log regression model

# Reduced log-log regression model, excluding female
model.4 = lm(log(Sales) ~ log(Age) + log(HS) + log(Income) + log(Black) + log(Price), 
             data = cigarettecons); 
fit.4 = fitted(model.4);

# Test log-log fit, excluding female
summary(model.4);

# Residual plot for reduced log-log regression model 
dev.off();
residualPlots(model.4, type = 'rstudent', tests = FALSE, quadratic = FALSE, ask = FALSE, 
              col = 'navy', pch = 19, cex = 1.5, main = 'Residual plots'); 

# Compare the models to test hypothesis of female excluded
anova(model.4,model.2);

# -------------------------------------------------------------------------------------------
# (b) Test the hypothesis that Female and HS are not needed in the above regression equation.
# -------------------------------------------------------------------------------------------
# 1. Reduced linear regression model

# Reduced linear regression model, excluding female and HS
model.5 = lm(Sales ~ Age + Income + Black + Price, data = cigarettecons);
fit.5 = fitted(model.5);

# Test hypothesis of Femare and HS excluded
summary(model.5);

# Residual plot for reduced linear regression model 
dev.off();
residualPlots(model.5, type = 'rstudent', tests = FALSE, quadratic = FALSE, ask = FALSE, 
              col = 'navy', pch = 19, cex = 1.5, main = 'Residual plots'); 

# Compare the models to test hypothesis of female and HS excluded
anova(model.5,model.1);
anova(model.5,model.3);

# -------------------------------------------------------------------------------------------
# 2. Reduced log-log regression model

# Reduced log-log regression model, excluding female and HS
model.6 = lm(log(Sales) ~ log(Age) + log(Income) + log(Black) + log(Price), 
             data = cigarettecons);
fit.6 = fitted(model.6);

# Test hypothesis of Femare and HS excluded
summary(model.6);

# Residual plot for reduced log linear regression model 
dev.off();
residualPlots(model.6, type = 'rstudent', tests = FALSE, quadratic = FALSE, ask = FALSE, 
              col = 'navy', pch = 19, cex = 1.5, main = 'Residual plots'); 

# Compare the models to test hypothesis of female and HS excluded
anova(model.6,model.2);
anova(model.6,model.4);

# -------------------------------------------------------------------------------------------
# (c) Obtain the 95% confidence interval for the true regression coefficient of the variable 
# Income.

confint(model.1, 'Income', level = 0.95);

# -------------------------------------------------------------------------------------------
# (d) What percentage of the total variation in Sales can be accounted for when Income is 
# removed? Explain.

# Reduced linear regression model excluding Income
model.7 = update(model.1, ~ . -Income);
fit.7 = fitted(model.7);

# Total variation given by R-squared
summary(model.7);

# -------------------------------------------------------------------------------------------
# (e) What percentage of the total variation in Sales can be accounted for by the three 
# variables Price, Age and Income? Explain.

# Reduced linear regression model excluding HS, black and female
model.8 = update(model.1, ~ . - HS - Black - Female);
fit.8 = fitted(model.8);

# Total variation given by R-squared
summary(model.8);

# -------------------------------------------------------------------------------------------
# (f) What percentage of the total variation in Sales can be accounted for by the 
# variable Income, when Sales is regressed only on this predictor? Explain.

# Simple linear regression model including Income
model.9 = lm(Sales ~ Income, data = cigarettecons);
fit.9 = fitted(model.9);

# Total variation given by R-squared
summary(model.9);

# -------------------------------------------------------------------------------------------
# EXERCISE 3
# -------------------------------------------------------------------------------------------

# Load data
state.x77; 

# Investigate data set
class(state.x77);
str(state.x77);

# Get information on state.x77 data
help(state.x77)

# Form a data frame with the data
state = data.frame(state.x77); 

# -------------------------------------------------------------------------------------------
# Initial explanatory plot

# Scatter plot matrix
dev.off();
plot(state, col = 'navy', pch = 16, cex = 1.1, cex.labels = 1.3, font.labels = 2, gap = 0.15,
     main = 'Scatter plot matrix: Multiple linear regression'); 

# -------------------------------------------------------------------------------------------
# (a) Fit a multiple linear regression model to explain the response variable Life Expectancy 
# as a function of all the regressors

# 1. Full multiple linear regression model

# Fit of multiple linear regression model including all regressor
model.1 = lm(Life.Exp ~ ., data = state);
fit.1 = fitted(model.1);

# Print the regression function
formula(model.1);

# Test linear fit
summary(model.1);

# Residual plots
dev.off();
library(car);
residualPlots(model.1, type = 'rstudent', tests = FALSE, quadratic = FALSE, ask = FALSE, 
              col = 'navy', pch = 19, cex = 1.5, 
              main = 'Residual plots: Multiple linear regression'); 
residualPlots(model.1, type = 'rstudent', tests = FALSE, quadratic = FALSE, ask = FALSE, 
              col = 'navy', terms = ~ 1, pch = 19, cex = 1.5, main = 'Linear fit'); 


# -------------------------------------------------------------------------------------------
# (b) Establish that Life expectancy is essentially a linear function of the predictors
# Population, Murder, HS Graduates, and Frost.

# Reduced multiple linear regression model

# Fit of reduced model
model.2 = lm(Life.Exp ~ Population + Murder + HS.Grad + Frost, data = state);
fit.2 = fitted(model.2)

# Print the regression function
formula(model.2);

# Test reduced fit
summary(model.2);

# Residual plots reduced model
dev.off();
residualPlots(model.2, type = 'rstudent', tests = FALSE, quadratic = FALSE, ask = FALSE, 
              col = 'navy', pch = 19, cex = 1.5, main = 'Residual plots'); 
residualPlots(model.2, type = 'rstudent', tests = FALSE, quadratic = FALSE, ask = FALSE, 
              col = 'navy', terms = ~ 1, pch = 19, cex = 1.5, main = 'Linear fit'); 

# Compare models
anova(model.2,model.1);

# -------------------------------------------------------------------------------------------
# (c) Population and Area are size type variables. Are they better reexpressed in logs?

# Log transformated regression model in population and area

# Create scatter plot matrix for log(Population) and log(Area)
state.M = transform(state, Population = log(Population), Area = log(Area));
colnames(state.M)[1] = 'log(Population)'; colnames(state.M)[8] = 'log(Area)'; 
plot(state.M, pch = 16, col = 'navy', cex = 1.1, 
     main = 'Scatter plot matrix: Linear-log regression', font.labels = 2); 

# Fit of log transformation
model.3 = lm(Life.Exp ~ log(Population) + Income + Illiteracy + Murder + HS.Grad + Frost 
             + log(Area), data = state);
fit.3 = fitted(model.3);

# Print the regression function
formula(model.3);

# Test the fit
summary(model.3);

# Residual plots
dev.off();
residualPlots(model.3, type = 'rstudent', tests = FALSE, quadratic = FALSE, ask = FALSE, 
              col = 'navy', pch = 16, cex = 1.5, main = 'Residual plots: Linear-log regression'); 
residualPlots(model.3, type = 'rstudent', tests = FALSE, quadratic = FALSE, ask = FALSE, 
              col = 'navy', terms = ~ 1, pch = 19, cex = 1.5, main = 'Log linear fit'); 

# The reduced regression model, excluding income, illiteracy and area, keeping log(Population)

# Fit of log transformation
model.4 = lm(Life.Exp ~ log(Population) + Murder + HS.Grad + Frost, data = state);
fit.4 = fitted(model.4);

# Print the regression function
formula(model.4);

# Test the fit
summary(model.4);

# Residual plots
dev.off();
residualPlots(model.4, type = 'rstudent', tests = FALSE, quadratic = FALSE, ask = FALSE, 
              col = 'navy', pch = 16, cex = 1.5, main = 'Residual plots: Linear-log regression'); 
residualPlots(model.4, type = 'rstudent', tests = FALSE, quadratic = FALSE, ask = FALSE, 
              col = 'navy', terms = ~ 1, pch = 19, cex = 1.5, main = 'Log linear fit'); 

# Compare the models
anova(model.4,model.3);

# -------------------------------------------------------------------------------------------
# Analyze the individual significance of all the regressors.
summary(model.4); 

# -------------------------------------------------------------------------------------------
# Obtain 95% confidence intervals for all the slope parameters

# Confidence interval for the slope parameter x
confint(model.4, 'log(Population)', level = 0.95);
confint(model.4, 'Murder', level = 0.95);
confint(model.4, 'HS.Grad', level = 0.95);
confint(model.4, 'Frost', level = 0.95);

# -------------------------------------------------------------------------------------------