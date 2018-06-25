# Set working directory
setwd("/Users/Emilie/Dropbox/Skole/UC3M/Predictive modeling/class")

install.packages('PBSmodelling')

library(PBSmodelling);

clearRcon(os = 'windows');
######################################### 
# [1] clear console and customize options
#########################################

rm(list = ls());

graphics.off();

loadRconsole('G:/Rconsole5.txt');

########################################
# [2] data 
########################################

sal = read.table(
  'salary.txt', h = T);  

########################################
# [3] remove outlier at row 33
########################################  

sal = sal[-33, ];

rownames(sal) = c(1:45);

str(sal); 

head(sal,n = 10); 

#########################################
# [4] change column names/add class
# use within(sal,{*}) or transform(sal,*)
######################################### 

sal = transform(sal, S = S/1E3);

names(sal)[1:2] = c('salx1000','years');

sal = within(sal, {class = as.factor(E + 3*M)});

sal = transform(sal, E = as.factor(E));

sal = transform(sal, M = as.factor(M));

names(sal)[3:4] = c('educ','man.1'); 

str(sal); 

head(sal, n = 10);  

Sys.sleep(10);

########################################
# [5] check data  
######################################## 

objects(); 

objects(sal);  

Sys.sleep(10);

########################################
# [6] initial scatter plot 
########################################

attach(sal);  

xrange = range(years);

yrange = range(salx1000);

windows();

plot(c(min(years)-1,max(years)), 
     c(min(salx1000), max(salx1000)+3), 
     type = 'n', 
     xlab = 'years', 
     ylab = 'salx1000'); 

points(years,salx1000);

Sys.sleep(10);

########################################
# [7] scatter plot by class
########################################

nclass = max(as.integer(class)); 

colors = rainbow(nclass);

linetype = c(1:nclass);

plotchar = c(15:20);  

########################################
# [8] set up display
########################################

windows();

par(lwd = 2., 
    font.lab = 2, cex.lab = 1.1,
    font.main = 2, cex.main = 1.4, 
    col.main = 'deepskyblue');

plot(c(min(years)-1,max(years)), 
     c(min(salx1000), max(salx1000)+3), 
     type = 'n', 
     xlab = 'years', 
     ylab = 'salx1000', 
     main = 'Salary example');  

Sys.sleep(5); 

########################################
# [9] add lines with color by class
########################################

for (i in 1:nclass)
  
{
  par(cex = 1.5, 
      col = colors[i], 
      lty = linetype[i],
      pch = plotchar[i], 
      lwd = 3.5);
  
  sal. = subset(sal, class == as.character(i)); 
  
  with(sal.,
       lines(years, salx1000, type = 'b'));
  
  rm(sal.); 
}

########################################
# [10] add a legend
######################################## 

par(col = 'black',  
    bg = 'ghostwhite', 
    lty = 1,
    lwd = 1); 

legend(min(years)-1., max(salx1000)+3.,
       h = T,
       xjust = 0,
       cex = 0.5,
       pt.cex = 0.9,
       1:nclass,  
       col = colors, 
       pch = plotchar, 
       lty = linetype, 
       lwd = rep(2.,6),
       title = 'class'); 

Sys.sleep(10);

########################################
# [11] ls in full 12-parameter model
######################################## 

f = salx1000 ~ class + class:years - 1; 

model.exp = lm(f,sal); 

coef.exp = round(matrix(coef(model.exp), 
                        nr = 6, nc = 2), d = 4);

colnames(coef.exp) = c('cons', 'years'); 

rownames(coef.exp) = 
  c('class = 1', 'class = 2', 'class = 3',
    'class = 4', 'class = 5', 'class = 6'); 

########################################
# [12] ls in each class
########################################

f = salx1000 ~ years;

for (i in 1:nclass)
  
{ 
  model.class = lm(f,sal, 
                   class == as.character(i));
  
  coef = coef(model.class); 
  coef = round(coef, d = 4);
  
  print(
    paste('class =', i, 
          'cons =', coef[1], 
          'years =', coef[2])
  );  
}

coef.exp;

Sys.sleep(10);

########################################
# [13] ls in reduced 7-parameter model
########################################

f = salx1000 ~ class + years - 1; 

model.red = lm(f, sal);  

coef.red = coef(model.red);

coef.red = round(
  cbind(coef.red[1:6], 
        coef.red[7]*matrix(1,6,1)),
  d = 4); 

colnames(coef.red) = c('cons','years'); 

rownames(coef.red) = 
  c('class = 1', 'class = 2', 
    'class = 3', 'class = 4', 
    'class = 5', 'class = 6'); 

coef.exp;

coef.red; 

Sys.sleep(10); 

#################################
# [14] Comparison of models
#################################

hip = anova(model.red,model.exp); 

str(hip);  

round(with(hip,RSS), d = 4); 

########################################
# [15] ls in additive 5-parameter model
########################################   

f = salx1000 ~ educ + man.1 + years - 1.; 

model.ad = lm(f, sal);  

coef.ad = c(coef(model.ad)[1:3], 
            coef(model.ad)[1:3] + coef(model.ad)[4],
            coef(model.ad)[5]*matrix(1,1,6));

coef.ad = round(matrix(coef.ad, nr = 6, 
                       nc = 2), d = 4); 

colnames(coef.ad) = c('cons','years'); 

rownames(coef.ad) = 
  c('class = 1', 'class = 2', 
    'class = 3', 'class = 4', 
    'class = 5', 'class = 6'); 

coef.red;

coef.ad; 

Sys.sleep(10); 

#################################
# [16] Comparison of models
#################################

hip = anova(model.ad,model.red); 

str(hip); 

round(with(hip,RSS), d = 4); 

#################################
# [17] Comparison of models
#################################

f = salx1000 ~ years;

model = lm(f,sal); 

coef = round(coef(model), d = 4);

coef;

coef.red; 

Sys.sleep(10); 

hip = anova(model,model.red); 

round(with(hip,RSS), d = 4); 

#################################
# [18] comparison of design 
#      matrices
#################################

methods(class = lm);

formula(model);

tail(model.matrix(model), 5);

Sys.sleep(15); 

formula(model.exp);

tail(model.matrix(model.exp), 5);

tail(data.frame(class,years),5);

Sys.sleep(15); 

formula(model.red);

tail(model.matrix(model.red), 5);

tail(data.frame(class,years),5);

Sys.sleep(15);  

formula(model.ad);

tail(model.matrix(model.ad), 5);

tail(data.frame(class,years),5); 

Sys.sleep(15); 

detach(sal);

search();

ls();
