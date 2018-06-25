############################
# LS by example
############################

help(attitude); 

windows(); plot(attitude, pch = 19, 
col = 'royalblue2', cex = 1.3, gap = .15,
font.labels = 2, cex.labels = 1.1, 
main = 'attitude survey data');

cat('', sep = '\n'); (at = attitude); # data

cat('', sep = '\n'); (n = nrow(at)); # sample size

cat('', sep = '\n'); (Y = at[,1]); # nx1 response vector

cat('', sep = '\n'); (p = ncol(at[,2:7])); # of regressors

cat('', sep = '\n'); (X1 = at[,2:7]); # nxp matrix of regressors

cat('', sep = '\n'); (X = cbind(1,X1)); # design matrix (by recycling)

cat('', sep = '\n'); (ones.n = X[,1]); # nx1 column of ones 

##############################
# X is of full rank by columns
##############################

library(Matrix); 

cat('', sep = '\n'); (m = rankMatrix(X)[1]); # m = p + 1

#########################################################
# _____________________ LS criterion ____________________
#
# fitted model:
#
#                      Y = fit + res  
#
# fit = X*beta.h: orthogonal projection of Y onto C(X)
#
# res = Y - fit - C(X) (-: Alt + 193)
#
# normal equations: (X'*X)*beta.h = X'*Y
#########################################################

X = data.matrix(X); # convert X to numerical object

Y = data.matrix(Y); # same with Y

################################################
# crossprod(A,B) = A' %*% B; %*%: matrix product
# crossprod(A) = A' %*% A; solve(A) = inv(A); 
################################################

beta.h = crossprod(solve(crossprod(X)), crossprod(X,Y)); 

colnames(beta.h) = 'coef'; 

cat('', sep = '\n'); round(beta.h, d = 4); # LS estimates

fit = X %*% beta.h; res = Y - fit; 

cat('', sep = '\n'); as.vector(Y); # responses

cat('', sep = '\n'); as.vector(round(fit, d = 4)); # fitted values

cat('', sep = '\n'); as.vector(round(res, d = 4)); # residuals

cat('', sep = '\n'); round(crossprod(X,res)); # Orthogonality condition

#################
# Sums of squares
#################

ssq = function(x){sum(x^2)}

cat('', sep = '\n'); (TSS.0 = ssq(Y)); cat('', sep = '\n');  # meaning?
cat('', sep = '\n'); (RGSS.0 = ssq(fit)); cat('', sep = '\n');  # meaning?
cat('', sep = '\n'); (RSS = ssq(res)); cat('', sep = '\n'); # measure of error  
# TSS.0 = RGSS.0 + RSS: Pythagorean Theorem

##################
# centered version
##################

# b.0: intercept; b: slope parameters

b.0 = beta.h[1]; b = beta.h[-1];

# fitted model: Y = fit + res = 1*b.0 + X1 %*% b + res 

# From fitted model: b.0 = mean(Y) - colMeans(X1) %*% b;

#############################################
# __________ centered fitted model __________  
#
#             Yc = Xc %*% b + res 
##############################################

# Yc: centered responses: Y - 1*mean(Y)

Yc = scale(Y, scale = FALSE)[,]; 

# Xc: centered regressor matrix: X1 - 1 %*% colMeans(X1)

Xc = scale(X1, scale = FALSE)[,]; 

cat('', sep = '\n'); as.vector(round(Yc, d = 4)); # centered responses

cat('', sep = '\n'); as.vector(round(Xc %*% b, d = 4)); # summand Xc %*% b

cat('', sep = '\n'); as.vector(round(res, d = 4)); # residuals

cat('', sep = '\n'); round(crossprod(Xc,res)); # Orthogonality condition

cat('', sep = '\n'); (TSS = ssq(Yc)); cat('', sep = '\n'); # Total sum of squares
cat('', sep = '\n'); (RGSS = ssq(Xc %*% b)); cat('', sep = '\n'); # Regression sum of squares
cat('', sep = '\n'); (RSS = ssq(res)); cat('', sep = '\n'); # Residual sum of squares  

# ANOVA decomposition: TSS = RGSS + RSS (Pythagorean Theorem)

############## 
# Consequences
##############

cat('', sep = '\n'); (sigma2.h = RSS/(n-m)); cat('', sep = '\n'); # (unbiased) LS estimator of error variance
# R2: squared cosine of angle between Yc and Xc %*% b: measure of quality of fit
cat('', sep = '\n');(R2 = RGSS/TSS); cat('', sep = '\n');
# R2: coefficient of determination: 1 - (RSS/TSS): grows with # of regressors p

########################## 
# Properties of estimators
##########################

# E(beta.h) = beta; Var(beta.h) = sigma^2*(solve(crossprod(X)));  

s.errors = sqrt(sigma2.h)*sqrt(diag(solve(crossprod(X)))); 

cat('', sep = '\n'); round(s.errors, d = 4); cat('', sep = '\n'); # s.errors of beta.h

t.ratios = beta.h/s.errors; 

cat('', sep = '\n'); round(as.vector(t.ratios), d = 4); cat('', sep = '\n'); # t-statistics

# under normality, and assuming H: slope = 0,
# all t- statistics are Student's t with n-m degrees of freedom

############# 
# ANOVA table
############# 

cat('', sep = '\n'); (df.total = n-1); # degrees of freedom of TSS;

cat('', sep = '\n'); (df.reg = p); # degrees of freedom of RGSS;

cat('', sep = '\n'); (df.res = n-m); # degrees of freedom of RSS

cat('', sep = '\n'); (F = (RGSS/df.reg)/(RSS/df.res)); cat('', sep = '\n'); # F - ratio 

# under normality, and assuming H: no regression,
# F-ratio is F with df.reg and df.res degrees of freedom 

################################################################
# all elements above can be obtained with functions lm and anova
################################################################

options(show.signif.stars = FALSE);

f = rating ~ .; model.full = lm(f,at); 

f.0 = rating ~ 1.; model.0 = lm(f.0,at);

#####################################################################
coef = with(summary(model.full), coefficients);
cat('', sep = '\n'); format(round(coef, d = 4)); cat('', sep = '\n');
#####################################################################

cat('', sep = '\n'); round(as.vector(beta.h), d = 4);

cat('', sep = '\n'); round(as.vector(s.errors), d = 4);

cat('', sep = '\n'); round(as.vector(t.ratios), d = 4);

############################################## 
# str(model.full); # str(summary(model.full)); 
##############################################

cat('', sep = '\n');  with(model.full, fitted.values); cat('', sep = '\n'); as.vector(fit)

cat('', sep = '\n');  with(model.full, residuals); cat('', sep = '\n'); as.vector(res)

######################## 
# methods(class = 'lm');
########################

cat('', sep = '\n'); model.matrix(model.full)[,]; cat('', sep = '\n'); X;

round(vcov(model.full), d = 4); cat('', sep = '\n'); round(sigma2.h*solve(crossprod(X)), d = 4);

############################ 

aov = anova(model.0, model.full); 

###########
# str(aov);
###########

cat('', sep = '\n'); with(aov,RSS); cat('', sep = '\n'); c(TSS,RSS);

cat('', sep = '\n'); aov$'Sum of Sq'[2]; cat('', sep = '\n'); RGSS;

cat('', sep = '\n'); with(aov,Res.Df); cat('', sep = '\n'); c(df.total,df.res);

cat('', sep = '\n'); with(aov,Df)[2]; cat('', sep = '\n'); df.reg;

cat('', sep = '\n'); with(aov,F)[2]; cat('', sep = '\n'); F;

#######################
# presentation of table
#######################

cat('', sep = '\n'); aov

attr(aov, 'heading') = NULL; 

rownames(aov) = c('total','error');  

cat('', sep = '\n'); (aov = rbind(aov[2,], aov[1,])); 
