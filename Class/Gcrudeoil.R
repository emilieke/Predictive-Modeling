# Set working directory
setwd("/Users/Emilie/Dropbox/Skole/UC3M/Predictive modeling/class")


library(PBSmodelling);

clearRcon(os = 'windows');
######################################### 
# [1] clear console and customize options
#########################################

rm(list = ls());

graphics.off();

loadRconsole(
  'C:/Users/santiago/Documents/Rconsole5.txt'
);

########################################
# [2] data 
########################################

oil = read.table(
  'crudeoil.txt', 
  h = T);

head(oil, n = 10); 

class(oil); 

str(oil); 

attributes(oil); 

attach(oil); 

########################################
# [3] scatter plot 
########################################

xrange = range(Year); 

yrange = range(Barrels);

windows(); 

Sys.sleep(5);
dev.off();
########################################
# [4] set up display
######################################## 

par(font.lab = 2, col.lab = 'brown4', 
    font.main = 2, col.main = 'darkorchid4', 
    lwd = 2., col = 'black');

plot(xrange,yrange, type = 'n', 
     xlab = 'Year', ylab = 'Barrels',  
     main = 'Barrels vs. Year'); 

Sys.sleep(5);

########################################
# [5] plot the points
######################################## 

par(cex = 1.5, pch = 15, col = 'seagreen');

points(Year,Barrels);  

########################################
# [6] add a legend
######################################## 

par(lwd = 2.5, col = 'darkorchid4', 
    bg = 'ghostwhite', cex = 1.);

legend(1900, 15000, 
       col = 'seagreen', pch = 15, pt.cex = 1., 
       'oil data', bty = 'o', text.font = 2,
       text.width = strwidth(c('oil data*'),
                             units = 'user')); 

Sys.sleep(5);

########################################
# [7] ls line
########################################

f = Barrels ~ Year;

model.1 = lm(f);

fit = fitted(model.1); 

par(lty = 1, lwd = 4.5, col = 'grey');

lines(Year, fit); 

Sys.sleep(5);

########################################
# [8] order two and three polynomials
########################################

f = Barrels ~ Year + I(Year^2);

model.2 = lm(f);

fit = fitted(model.2); 

par(lty = 2, lwd = 4.5, col = 'red'); 

lines(Year, fit); 

Sys.sleep(5);

f = Barrels ~ Year + I(Year^2) + I(Year^3);

model.3 = lm(f);

fit = fitted(model.3);

par(lty = '1373', lwd = 4.5, 
    col = 'deepskyblue3'); 

lines(Year, fit); 

Sys.sleep(10);

########################################
# [9] modified legend
########################################

par(bg = 'ghostwhite', lwd = 2.5, 
    lty = 1); 

legend(1900,16000, 
       xjust = 0, 
       cex = 0.9,  
       pch = c(15,NA,NA,NA), 
       pt.cex = 1.1, 
       lty = c(0,1,2,4), 
       lwd = c(0,3.5,3.5,3.5),
       bty = 'o', 
       c('oil data', 'linear', 'quadratic','cubic') , 
       col = c('seagreen','grey','red','deepskyblue3'), 
       text.font = 2,
       text.col = 'darkorchid4',
       text.width = strwidth(c('quadratic'),  
                             units = 'user'),
       seg.len = 1.2); 

Sys.sleep(5); 

########################################
# [10] log(Barrels) ~ Year; 
########################################

f = log(Barrels) ~ Year; 

model.log = lm(f); 

windows();

par(pch = 16, lwd = 2);

plot(Year,log(Barrels), cex = 1.2, 
     col = 'maroon4'); 

abline(model.log, col = 'deepskyblue3', 
       lwd = 3.5, lty = '2222'); 

par(pch = 16, bg = 'beige');

legend(1880, 10, 'log(Barrels) ~ Year',
       text.col = 'navy', pch = 16, 
       pt.cex = 1.1,
       col = 'maroon4',
       text.font = 2);

Sys.sleep(10); 

########################################
# [11] residual plots
########################################

library(car);

windows();

par(bg = 'honeydew1', mfrow = c(2,2), lwd = 1., 
    font.lab = 1, cex.lab = 1,
    col.lab = 'black', oma = c(2,2,3,2),
    mar = c(4.5, 4.5, 1.5, 1.5), pch = 1, 
    cex = .8,
    font.main = 1,
    cex.main = 1.2); 

residualPlot(model.1, 
             main = 'Linear', 
             variable = 'Year',
             type = 'rstudent');

mtext('residual plots', outer = T, cex = 1.3, 
      col = 'black', line = 0.75, font = 2);

box('inner', col = 'black',
    lty = '1373'); 

residualPlot(model.2, 
             main = 'Quadratic', 
             variable = 'Year',
             type = 'rstudent');

residualPlot(model.3, 
             main = 'Cubic', 
             variable = 'Year',
             type = 'rstudent');

residualPlot(model.log, 
             main = 'log(Barrels) ~ Year', 
             variable = 'Year',
             type = 'rstudent'); 

legend('bottomleft', legend = 'correlated errors', 
       bty = 'n', text.col = 'navy', text.font = 2);

Sys.sleep(5); 

detach(oil);

search();

ls();
