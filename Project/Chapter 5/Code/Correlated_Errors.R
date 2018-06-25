# Set working directory
setwd("/Users/Emilie/Dropbox/Skole/UC3M/Predictive modeling/Practice Workbook/Chapter 5/Data")

# Install packages
install.packages('openxlsx')

# Library
library('openxlsx')

# -------------------------------------------------------------------------------------------
# CHAPTER 4: CORRELATED ERRORS
# -------------------------------------------------------------------------------------------


# -------------------------------------------------------------------------------------------
# EXERCISE 1
# -------------------------------------------------------------------------------------------

# Read data from xlsx
icecream = read.xlsx('icecream.xlsx')

# View variable names
str(icecream);

# Defining the regression variables
y = icecream$cons;
x1 = icecream$price;
x2 = icecream$income;
x3 = icecream$temp;
x4 = icecream$time;

# (a) Run a OLS regression in (10) of y on x1 : price; x2 : income; and x3 : temperature;
# including an intercept term. Compute the OLS residuals et. Make a time sequence plot of the standardized et. 
# Can you see any type of correlation pattern?

# Linear regression model using OLS
full = lm(y ~ x1 + x2 + x3, data = icecream);

# Fit the model 
fit.full = fitted(full);

# Test hypothesis of linear relationship
summary(full);

# Compute residuals
residuals = resid(full);residuals

# Time sequence plot
library(car)

dev.off();

# Stardardized residuals
full.stdres = ts(residuals)
m=acf(full.stdres)
full.stdres_1=full.stdres[2:length(full.stdres)]
full.stdres = full.stdres[1:length(full.stdres)-1]

small = lm(full.stdres~ 0 +full.stdres_1)
ro = small$coefficients
difference = full.stdres-ro*full.stdres_1
sigma = sd(difference)
qqnorm(difference); qqline(rnorm(10000),mean = 0, sd= sigma, col = 2)




# Residual plot using stardardized residuals
plot(x4,full.stdres, ylab='Residuals', xlab='Time', main='Time sequence plot of residuals') 
abline(0, 0)

# (b) Estimate the parameter using the lag-1 autocorrelation coefficient
acf(residuals, lag.max = 20, plot = 1);
    
# (c) Obtain the Durbin-Watson statistic for the icecream.xlsx data set, and investigate
# its significance.
library(lmtest)

dwtest(full);


# (d) Perform a Cochrane-Orcutt iteration for the icecream.xlsx data using STATGRAPHICS, and
# interpret the final result.
install.packages('orcutt')
library('orcutt')

cochrane.orcutt(full);

