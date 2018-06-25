# Set working directory
setwd("/Users/Emilie/Dropbox/Skole/UC3M/Predictive modeling/Data1")

# Install packages
install.packages("openxlsx")

library("openxlsx")

list.files()
crude.oil = read.xlsx('crudeoil.xlsx')


with(crude.oil,plot(Year,log(Barrels), pch = 16,   
                   main = 'Barrel production',
                   col = 'royalblue', lwd = 1.5));  

str(crude.oil)

f = log(Barrels) ~ Year;
ls();
model.oil = lm(f, data=crude.oil)
coef(model.oil)

#######################
# read data 
#######################
setwd("/Users/Emilie/Dropbox/Skole/UC3M/Predictive modeling/Data2")

used.car = read.table('usedcar.txt');
colnames(used.car) = c('price','years');
class(used.car);
str(used.car);
n = dim(used.car)[1];n
View(used.car);

##################################
# plot data 
################################## 

dev.off()

with(used.car,plot(years,price, pch = 16,   
                   main = 'Used car data',
                   col = 'royalblue', lwd = 1.5));  


attr(with(model, coefficients), "names")

intercept = betas[1];
slope = betas[2];

abline(a = intercept, b = slope, lty = '1373', 
       col = 'orangered',
       lwd = 1.5);

# fitted values
fit = with(model,fitted.values);fit

# the true values
with(used.car, price);
with(used.car, years);


####################################
# interpolate several lines visually 
#################################### 

intercept = 12;

slope = -1.;

abline(a = intercept, b = slope, lty = '1373', 
       col = 'mediumseagreen',
       lwd = 1.5);

############################
# Remove significance stars;
############################

options(show.signif.stars = F); 

#############################################################
# Fit of simple linear regression model  
# For writing '~', do 'AltGr + 4 + Space'; 
#############################################################

f = price ~ years; # f: formula

# or simply f = price ~. ;

model = lm(f, data = used.car);  

summary(model); 

str(model); 

str(summary(model));  

#######################
# Standard ANOVA table;
#######################

f = price ~ 1.;

one.sample = lm(f,used.car); 

anova(one.sample,model);

###############
# Model matrix;
###############

model.matrix(model)[,]; # [,] for extraction;

############################################
# Coefficients, fitted values and residuals;
############################################

betas = coef(model);

fit = fitted(model); 

res = residuals(model); 

# standardize residuals
stres = rstandard(model); 

# standarize with student t distribution
studres = rstudent(model); 


par(mfrow = c(2,2));

plot(model,which = c(1:2),
     add.smooth = FALSE, 
     label.id = NULL,
     sub.caption = '', 
     caption = 'residual plots');

library(car);


residualPlots(model, 
              type = 'rstudent',
              tests = F, 
              quadratic = NULL,
              fitted = F); 

#################################################
# Confidence intervals for individual parameters;
#################################################

confint(model, 'years', level = 0.90);

#####################################################################
# Confidence and prediction intervals for specified regressor values;
#####################################################################

predict(model, data.frame(years = 5), 
        se.fit = TRUE, interval = 'confidence', 
        level = 0.95);

