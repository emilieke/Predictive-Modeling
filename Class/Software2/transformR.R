#######################
# read data 
#######################

graphics.off();

library(MASS)

library(openxlsx)

cat('',sep = '\n'); (movie.buzz.0 = read.xlsx('moviebuzz.xlsx')); 

movie.buzz = transform(movie.buzz.0, 
Box = Box/1E6);

##################################
# plot data 
################################## 

Sys.setenv(LANGUAGE = 'en'); # menus in English

windows(); # take the plot outside RStudio 

#################################################
# X11() for Unix; quartz() for Mac
#  
# http://www.statmethods.net/graphs/creating.html
#################################################

with(movie.buzz, plot(Fandango,Box, 
pch = 16,   
main = 'Movie buzz data', 
col = 'royalblue', cex = 1.5));  

####################################
# box-cox power transformation
#################################### 

f.0 = Box ~ Fandango; 

windows();

A = boxcox(lm(f.0, data = movie.buzz), plotit = T); 

######################################
# extract relevant 95% part of (wrong)
# log-likelihood 
###################################### 

A = with(A,cbind(x,y)); 

round(A[A[,2]> -105,], d = 4);

######################
# excludes a = 0 (log)
######################

#############################################
# take power 1/4 (0.2626)
############################################# 

a = 1/4; 

(f.a = update(f.0, (.)^a ~ .));

model.a = lm(f.a, data = movie.buzz);

coef.a = with(summary(model.a), coefficients);

cat('', sep = '\n'); format(round(coef.a, d = 4));

#############################################
# correct but uninterpretable  
############################################# 

windows();

with(movie.buzz, plot(log(Box),Box^a, 
pch = 16,   
main = 'Movie buzz data', 
col = 'royalblue', cex = 1.5)); 

with(movie.buzz, abline(lm(Box^a ~ log(Box)), lty = '1373')); 

#############################################
# take logs  
############################################# 
 
(f.log = update(f.0, log(.) ~ .)); 

model.log = lm(f.log, data = movie.buzz);

coef.log = with(summary(model.log), coefficients);

cat('', sep = '\n'); format(round(coef.log, d = 4)); cat('', sep = '\n');
#############################################
# for every .1 of increase of Fandango index,  
# Box increments 40% of 1 (x 1000000$)
# __________________________________________
# Even if F = 0,
# Box = exp(.8) > 2 (x 1000000$) 
############################################# 

dev.set(2);

a = coef.log;

with(movie.buzz, lines(sort(Fandango), exp(a[1] + a[2]*sort(Fandango)),
lty = '1373', col = 'brown', lwd = 3.5));

