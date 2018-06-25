# Set working directory
setwd("/Users/Emilie/Dropbox/Skole/UC3M/Predictive modeling/Practice Workbook/Chapter 4/Data")

# Install packages
install.packages('openxlsx')

# Library
library('openxlsx')

# -------------------------------------------------------------------------------------------
# CHAPTER 4: WEIGHTED LEAST SQUARES
# -------------------------------------------------------------------------------------------


# -------------------------------------------------------------------------------------------
# EXERCISE 1
# -------------------------------------------------------------------------------------------

# Read data from xlsx
labour = read.xlsx('labour_2.xlsx')

# View variable names
str(labour);

# Defining the regression variables
y = labour$labour;
x1 = labour$wage;
x2 = labour$output;
x3 = labour$capital;
x4 = labour$cstd;
x5 = labour$cvar;
x6 = labour$weight;

# (a) Fit a linear model using OLS. Investigate the significance of all the regressors.

# Linear regression model using OLS
full = lm(y ~ x1 + x2 + 
            x3, data = labour);

# Fit the model 
fit.full = fitted(full);

# Test hypothesis of linear relationship including wage, output and capital
summary(full);

# (b) Make an initial assessment of the presence of heteroscedasticity by forming the scatter
# plot matrix of the variables as they come, or after taking logs. Heterogeneity in variation
# arises naturally, due to the use of firms with changing number of workers.

# Scatter plot
plot(labour);

# (c) More formally, store the OLS residuals ei, i = 1, . . . , n, and construct an auxiliary
# regression of ei^2 on all the regressor variables. The Breusch - Pagan test statistic
# nR^2, that is asymptotically distributed as a χ^2 random variable with p = 3 degrees of
# freedom, strongly suggests the presence of heteroscedasticity of unknown form. The
# associated p−value is very close to zero.

residuals = resid(full);
r.squared = (residuals)^2;

# Residual plot
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(full)

## Breusch-Pagan
lmtest::bptest(full, data=labour, studentize=FALSE);
car::ncvTest(full);

# (d) From part (b), taking logs seems a possible initial remedial action. In other words, fit
# log(y) = β0 + β1log(x1) + β2log(x2) + β3log(x3) + ε ,where β0 = log(a). Can you see any anomaly?

# Log transformation of linear regression model
full.log = lm(log(y) ~ log(x1) + log(x2) + 
            log(x3), data = labour);

# Fit the model
fit.full.log = fitted(full.log);

# Test hypothesis of log transformation
summary(full.log);


# Residual plot
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(full.log)


# (e) To solve for the problems of part (d), a possibility is to compute the so-called White
# standard errors, and recompute the associated partial t−statistics. These are the
# squared roots of the associated diagonal elements of the matrix.
install.packages('het.test')

library('het.test')

whites.htest(full.log);

library(vars)
model1 <- VAR(dataset, p = 1)
whites.htest(model1)


# (f) According to the above, heteroscedasticity still plays a role in model (1) after taking
# logs. Do this for this data set, taking the vector z = (log(x1), log(x2), log(x3))0.

# (g) After the conclusions in (f), the weighted last squares (WLS) objective function
# a) Fit a regression with a constant, specifying the vector of weights.
# b) Run OLS with no intercept, and new data points obtained after dividing each data
# row by the square root of the quantity exp(αzi).Is capital now significant?
