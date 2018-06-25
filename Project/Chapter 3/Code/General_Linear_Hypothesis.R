# Set working directory
setwd("/Users/Emilie/Dropbox/Skole/UC3M/Predictive modeling/Practice Workbook/Chapter 3/Data")

# Install packages
install.packages('openxlsx')

# Library
library('openxlsx')

# -------------------------------------------------------------------------------------------
# CHAPTER 3: GENERAL LINEAR HYPOTHESIS
# -------------------------------------------------------------------------------------------


# -------------------------------------------------------------------------------------------
# EXERCISE 1
# -------------------------------------------------------------------------------------------

# Read data from xlsx
profit = read.xlsx('profit.xlsx')

# View variable names
str(profit);

# Scatter plot
dev.off();
plot(profit, col = 'navy', pch = 16, cex = 1.1, cex.labels = 1.3, font.labels = 2, gap = 0.15,
     main = 'Scatter plot matrix: Multiple linear regression'); 

# -------------------------------------------------------------------------------------------
# (a) Fit a multiple regression with all the regressor variables.

# Full linear regression model
full = lm(Profit ~ Purchases + Sales + Hours + Market.share, data = profit);
fit.full = fitted(full);

# Test hypothesis of all regressors included
summary(full);

# -------------------------------------------------------------------------------------------
# (b) From the previous analysis, it seems that the profit is, as expected, mostly determined
# by the purchases and sales. Formalize this statement as an application of the GLH, by
# testing whether the hours of operation and the market share have something to say
# about the profit, once purchases and sales have been included in the model.

# Reduced linear regression model
reduced = lm(Profit ~ Purchases + Sales, data = profit);
fit.reduced = fitted(reduced);

# Test hypothesis of hours and market share excluded
summary(reduced);

# Compare the models
anova = anova(reduced,full);anova
str(anova);

# Determine the RSS associated to the reduced model that includes only
# purchases and sales.
RSS.reduced = with(anova,RSS[1]);RSS.reduced

# -------------------------------------------------------------------------------------------
# EXERCISE 2
# -------------------------------------------------------------------------------------------

palette(c('navy','dark cyan'));
z = with(profit,Special.offer);

# Scatter plot matrix, including special offer = 0,1
plot(profit, main = 'Scatter plot matrix: Special.offer = 0,1', font.labels = 2, gap = .15, 
     cex.labels = 1.15, col = 1 + z, cex = 1.1, pch = 16);

# Scatter plot matrix, for special offer = 0
plot(subset(profit, Special.offer == 0), cex = 1.1, pch = 16, col = 'navy',
     main = 'Scatter plot matrix: Special.offer = 0', font.labels = 2, gap = .15, 
     cex.labels = 1.15);

# Scatter plot matrix, for special offer = 1
plot(subset(profit, Special.offer == 1), cex = 1.1, pch = 16, col = 'dark cyan',
     main = 'Scatter plot matrix: Special.offer = 1', font.labels = 2, gap = .15, 
     cex.labels = 1.15);

with(profit, table(Special.offer));


# (a) Find the values of the statistics RSS0, RSS1, RSS, and RSS(dummy).

# Reduced regression model including special offer
reduced.SO = lm(Profit ~ Purchases + Sales + Special.offer + 
                  Special.offer:(Purchases + Sales), data = profit);
fit.reduced.SO = fitted(reduced.SO);

xtable(summary(reduced.SO));

anova.reduced.SO = anova(reduced.SO);anova.reduced.SO
str(anova.reduced.SO);

# Determine RSS for model including special offer
RSS = anova.reduced.SO$'Sum Sq'[6];RSS

# -------------------------------------------------------------------------------------------
# Reduced regression model special offer = 0

# Reduced linear regression model, SO = 0
reduced.SO.0 = lm(Profit ~ Purchases + Sales, subset = Special.offer == 0, data = profit);
fit.reduced.SO.0 = fitted(reduced.SO.0);

xtable(summary(reduced.SO.0));

anova.reduced.SO.0 = xtable(anova(reduced.SO.0));anova.reduced.SO.0
str(anova.reduced.SO.0);

# Determine RSS0 for model when special offer = 0
RSS0 = anova.reduced.SO.0$'Sum Sq'[3];RSS0

# -------------------------------------------------------------------------------------------
# Reduced linear regression model, SO = 1
reduced.SO.1 = lm(Profit ~ Purchases + Sales, subset = Special.offer == 1, data = profit);
fit.reduced.SO.1 = fitted(reduced.SO.1);

xtable(summary(reduced.SO.1));

anova.reduced.SO.1 = xtable(anova(reduced.SO.1));anova.reduced.SO.1
str(anova.reduced.SO.1);

# Determine RSS1 for model when special offer = 1
RSS1 = anova.reduced.SO.1$'Sum Sq'[3];RSS1


# -------------------------------------------------------------------------------------------
# Reduced linear regression model including special offer, same slope but different intercept
# parameters

reduced.dummy = lm(Profit ~ Purchases + Sales + Special.offer, data = profit);
fit.reduced.dummy = fitted(reduced.dummy);

xtable(summary(reduced.dummy));

anova.reduced.dummy = xtable(anova(reduced.dummy));anova.reduced.dummy
str(anova.reduced.dummy);

# Determine RSS.dummy for model with same slope but different intercept
RSS.dummy = anova.reduced.dummy$'Sum Sq'[4];RSS.dummy

RSS.table = cbind(RSS.reduced, RSS, RSS0, RSS1, RSS0 + RSS1, RSS.dummy);RSS.table
RSS.table = xtable(cbind(RSS.reduced, RSS, RSS0, RSS1, RSS0 + RSS1, RSS.dummy));RSS.table

# -------------------------------------------------------------------------------------------
# (b) Test with the GLH whether the slope parameters for the cases = 0 and = 1 coincide.
# Comments. You will need a F-statistic that depends on RSS and RSS(dummy).

# H0: Same slope parameters
anova(reduced.dummy, reduced.SO);

# H0: Same intercept parameters
# H0: Dummy = 0
anova(reduced, reduced.dummy);


# Relation between F and t in dummy model
summary(reduced.dummy)$coeff

t = as.matrix(data.frame(summary(reduced.dummy)$coeff)[,3])[2];t

cbind(t*t, anova(reduced, reduced.dummy)$F[2])

# -------------------------------------------------------------------------------------------
# (c) From the results in (b), is the dummy model adequate?

anova(reduced, reduced.dummy);

# -------------------------------------------------------------------------------------------
# (d) Test the significance of the dummy parameter using RSS(reduced) and RSS(dummy).
# Does this confirm the analysis of Exercise 1?


# Final decision is on the reduced model with
#         n = n0 + n1 = 80 + 30 = 
#            110 observations
