# Set working directory
setwd("/Users/Emilie/Dropbox/Skole/UC3M/Predictive modeling/Practice Workbook/Chapter 1/Data")

# Install packages
install.packages("openxlsx")

# Library
library("openxlsx")


# -------------------------------------------------------------------------------------------
# CHAPTER 1: SIMPLE LINEAR REGRESSION
# -------------------------------------------------------------------------------------------

# -------------------------------------------------------------------------------------------
# EXERCISE 1
# -------------------------------------------------------------------------------------------

# Read data from xlsx
poverty = read.xlsx('poverty.xlsx')

# Defining the regression variables
Birth.rate = poverty$Brth15to17; # response variable, Y
Poverty.rate = poverty$PovPct; # regressor, X

# -------------------------------------------------------------------------------------------
# (a) Specify the regression equation, indicating whether or not a transformation is needed.

# Initial explanatory plot
dev.off();
with(poverty,plot(Poverty.rate,Birth.rate, pch = 19, 
                  main = 'Scatter plot: Simple linear regression', 
                  col = 'navy', lwd = 1, xlab = 'Poverty rate (%)', ylab = 'Birth rate (%)')); 

# Fit of simple linear regression model
model.1 = lm(Birth.rate ~ Poverty.rate, data = poverty); 
fit.1 = fitted(model.1);

# Test linear fit
summary(model.1);

# Residual plots
library(car);

# Residual plots for the regressor and fitted values
dev.off();
residualPlots(model.1, type = 'rstudent', tests = FALSE, quadratic = FALSE, ask = FALSE, 
              col = 'navy', pch = 16, cex = 1.5, main = 'Residual plots'); 

# Scatter plot with fitted line
dev.off();
with(poverty,plot(Poverty.rate,Birth.rate, pch = 19, 
                  main = 'Fitted model: Simple linear regression',
                  col = 'navy', lwd = 1, xlab = 'Poverty rate (%)', ylab = 'Birth rate (%)')); 
lines(Poverty.rate,fit.1,col='blue',lwd = 1.5); 

# -------------------------------------------------------------------------------------------
# (b) Construct a 95% confidence interval for the slope parameter.

# Confidence interval for the slope parameter x
confint(model.1, 'Poverty.rate', level = 0.95);

# -------------------------------------------------------------------------------------------
# (c) Is the poverty rate significant for assessing the teenage birth rate?

# Look at the p-value for the slope parameter
summary(model.1); 

# -------------------------------------------------------------------------------------------
# (d) Quantify the effect of an increase of the poverty on the birth rate.

(betas = coef(model.1));

# -------------------------------------------------------------------------------------------
# (e) What would be the estimated birth rate for a state with a poverty percentage of 15%?

# Confidence and prediction intervals for specified regressor values
predict(model.1, data.frame(Poverty.rate = 15), se.fit = TRUE, interval = 'confidence', 
        level = 0.95);

# -------------------------------------------------------------------------------------------
# EXERCISE 2
# -------------------------------------------------------------------------------------------

# Read data from xlsx
hometax = read.xlsx('hometax.xlsx')

# -------------------------------------------------------------------------------------------
# (a) Specify the regression equation, indicating whether or not a transformation is needed.
# -------------------------------------------------------------------------------------------
# 1. Simple linear regression

# Scatter plot
dev.off();
with(hometax,plot(Price,Tax, pch = 19,main = 'Scatter plot: Simple linear regression', 
                  col = 'navy', lwd = 1, xlab = 'Price (x1000), X', ylab = 'Tax, Y'));  

# Fit of simple linear regression model
model.1 = lm(Tax ~ Price, data = hometax); 
fit.1 = fitted(model.1);

# Test linear fit
summary(model.1);

# Residual plot for simple linear regression
dev.off();
residualPlots(model.1, type = 'rstudent', tests = FALSE, quadratic = FALSE, ask = FALSE, 
              col = 'navy', pch = 16, cex = 1.5, main = 'Residual plots'); 

# Fitted simple linear regression model
dev.off();
with(hometax,plot(Price,Tax, pch = 19,main = 'Fitted model: Simple linear regression', 
                  col = 'navy', lwd = 1, xlab = 'Price (x1000), X', ylab = 'Tax, Y'));  
lines(hometax$Price,fit.1,col='blue',lwd = 1.5); 

# -------------------------------------------------------------------------------------------
# 2. Linear-log regression, tax ~ log(price)

# Scatter plot
dev.off();
with(hometax,plot(log(Price),Tax, pch = 19,main = 'Scatter plot: Linear-log regression', 
                  col = 'navy', lwd = 1, xlab = 'log (Price), log (X) (x1000)',
                  ylab = 'Tax, Y'));  

# Fit of log linear regression model
model.2 = lm(Tax ~ log(Price), data = hometax); 
fit.2 = fitted(model.2);

# Test log linear fit
summary(model.2);

# Residual plot log transformation in regressor
dev.off();
residualPlots(model.2, type = 'rstudent', tests = FALSE, quadratic = FALSE, ask = FALSE, 
              col = 'navy', pch = 16, cex = 1.5, main = 'Residual plots'); 

# -------------------------------------------------------------------------------------------
# 3. Log-log regression model, log(tax) ~ log(price)

# Scatter plot
dev.off();
with(hometax,plot(log(Price),log(Tax), pch = 19,main = 'Scatter plot: Log-log regression', 
                  col = 'navy', lwd = 1, xlab = 'log (Price), log (X) (x1000)', 
                  ylab = 'log (Tax), log (Y)'));  

# Fit of multiplicative regression model
model.3 = lm(log(Tax) ~ log(Price), data = hometax); 
fit.3 = fitted(model.3);

# Test log linear fit
summary(model.3);

# Residual plot multiplicative model
dev.off();
residualPlots(model.3, type = 'rstudent', tests = FALSE, quadratic = FALSE, ask = FALSE, 
              col = 'navy', pch = 16, cex = 1.5, main = 'Residual plots'); 

# Scatter plot with fitted line
dev.off();
with(hometax,plot(log(Price),log(Tax), pch = 19, main = 'Fitted model: Log-log regression',
                  col = 'navy', lwd = 1, xlab = 'log (Price), log (X)', 
                  ylab = 'log (Tax), log (Y)')); 
lines(log(hometax$Price),fit.3,col='blue',lwd = 1.5); 

# -------------------------------------------------------------------------------------------
# (b) Construct a 95% confidence interval for the slope parameter.

# Confidence interval for the slope parameter x
confint(model.3, 'log(Price)', level = 0.95);

# -------------------------------------------------------------------------------------------
# (c)  Is the price of a house significant for assessing the amount of taxes paid?

summary(model.3); 

# -------------------------------------------------------------------------------------------
# (d) Quantify also the effect of an increase of the price on taxes.

(betas = coef(model.3));

# -------------------------------------------------------------------------------------------
# (e) How much would be the estimated taxes for a house with price 100(??1000)$?

# Confidence and prediction intervals for specified regressor values
exp(predict(model.3, data.frame(Price = 100), interval = 'confidence', level = 0.95));

# When looking at the scatter plot this prediction seem realistic
dev.off();
with(hometax,plot(Price,Tax, pch = 16, main = 'Simple linear regression',col = 'navy', lwd = 1, 
                  xlab = 'Price', ylab = 'Tax')); 
lines(hometax$Price,fit.1,col='blue',lwd = 1.5); 

# -------------------------------------------------------------------------------------------
# EXERCISE 3
# -------------------------------------------------------------------------------------------

# Read data from xlsx
advertising = read.xlsx('advertising.xlsx')

# -------------------------------------------------------------------------------------------
# (a) Specify the regression equation, indicating whether or not a transformation is needed.

# 1. Simple linear regression

# Initial explanatory plot
dev.off();
with(advertising,plot(Pages,Revenue, pch = 19,main = 'Scatter plot: Simple linear regression', 
                      col = 'navy', lwd = 1, xlab = 'Pages, X (x100)', 
                      ylab = 'Revenue, Y ($ million)'));  

# Fit of simple linear regression model
model.1 = lm(Revenue ~ Pages, data = advertising); 
fit.1 = fitted(model.1);

# Test linear fit
summary(model.1);

# Residual plot for simple linear regression
library(car);

dev.off();
residualPlots(model.1, type = 'rstudent', tests = FALSE, quadratic = FALSE, ask = FALSE, 
              col = 'navy', pch = 16, cex = 1.5, main = 'Residual plots'); 

# -------------------------------------------------------------------------------------------
# 2. Log transformaion in response variable, log(Revenue) ~ Pages
# Initial explanatory plot
dev.off();
with(advertising,plot(Pages,log(Revenue), pch = 19,main = 'Scatter plot: Log-linear regression', 
                      col = 'navy', lwd = 1, xlab = 'Pages, X (x100)', 
                      ylab = 'log (Revenue), log (Y) ($ million)'));  

# Fit of log transformation
model.2 = lm(log(Revenue) ~ Pages, data = advertising); 
fit.2 = fitted(model.2);

# Test log linear fit
summary(model.2);

# Residual plot for log linear model 
dev.off();
residualPlots(model.2, type = 'rstudent', tests = FALSE, quadratic = FALSE, ask = FALSE, 
              col = 'navy', pch = 16, cex = 1.5, main = 'Residual plots'); 

# -------------------------------------------------------------------------------------------
# 3. Log-log regression, log(Revenue) ~ log(Pages)

# Scatter plot
dev.off();
with(advertising,plot(log(Pages),log(Revenue), pch = 19,main = 'Scatter plot: Log-log regression', 
                      col = 'navy', lwd = 1, xlab = 'log (Pages), log (X) (x100)', 
                      ylab = 'log (Revenue), log (Y) ($ million)'));  

# Fit of simple linear regression model
model.3 = lm(log(Revenue) ~ log(Pages), data = advertising); 
fit.3 = fitted(model.3);

# Test linear fit
summary(model.3);

# Residual plot for log-log model 
dev.off();
residualPlots(model.3, type = 'rstudent', tests = FALSE, quadratic = FALSE, ask = FALSE, 
              col = 'navy', pch = 16, cex = 1.5, main = 'Residual plots'); 

# -------------------------------------------------------------------------------------------
# Outliers

# Detect the outliers
with(advertising,range(Pages));
with(advertising,range(log(Pages)));

# Remove outliers
(outliers = subset(advertising, log(Pages) == 0 | log(Pages) > 4)); 
(z = as.numeric(rownames(outliers))); 
(advertising.new = advertising[-z,]);

# -------------------------------------------------------------------------------------------
# 4. Log transformaion in both variables, log(Revenue) ~ log(Pages) excluding outliers

# Scatter plot
dev.off();
with(advertising.new,plot(log(Pages),log(Revenue), pch = 19,
                          main = 'Scatter plot: Log-log regression excluding outliers', 
                          col = 'navy', lwd = 1, xlab = 'Pages, log(X) (x100)', 
                          ylab = 'Revenue, log(Y) ($ million)'));  

# Fit of log-log regression model
model.4 = lm(log(Revenue) ~ log(Pages), data = advertising.new); 
fit.4 = fitted(model.4);

# Test linear fit
summary(model.4);

# Residual plot for log-log model excluding outliers
dev.off();
residualPlots(model.4, type = 'rstudent', tests = FALSE, quadratic = FALSE, ask = FALSE, 
              col = 'navy', pch = 16, cex = 1.5, main = 'Residual plots'); 

# The fitted log-log model, excluding the outliers
dev.off();
with(advertising.new,plot(log(Pages),log(Revenue), pch = 19,
      main = 'Fitted model: Log-log regression excluding outliers', col = 'navy', 
      lwd = 1, xlab = 'log (Pages), log (X) (x100)', 
      ylab = 'log (Revenue), log (Y) ($ million)'));  
lines(log(advertising.new$Pages),fit.4,col='blue',lwd = 1.5); 

# -------------------------------------------------------------------------------------------
# (b) Construct a 95% confidence interval for the slope parameter.

# Confidence interval for the slope parameter x
confint(model.4, 'log(Pages)', level = 0.95);

# -------------------------------------------------------------------------------------------
# (c)  Is the number of pages significant for assessing the revenue?

summary(model.4); 

# -------------------------------------------------------------------------------------------
# (d) Quantify also the effect of an increase of the number of pages on revenue

(betas = coef(model.4));

# -------------------------------------------------------------------------------------------

