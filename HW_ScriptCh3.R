# Empirical Modeling HW1
##MAJ Matthew Ferguson
##

setwd("~/Desktop/EmpModel/")

## Problem 3,1
p3_1 <- read.csv("TableB_1.csv", strip.white = TRUE, header=TRUE, sep = ",")

model <- lm(y~x2+x7+x8, data = p3_1)
summary(model)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.0370 -0.7129 -0.2043  1.1101  3.7049 
# 
# Coefficients:
#                Estimate Std. Error t value Pr(>|t|)    
#  (Intercept) -1.808372   7.900859   -0.229 0.820899    
#   x2           0.003598   0.000695   5.177 2.66e-05 ***
#   x7           0.193960   0.088233   2.198 0.037815 *  
#   x8          -0.004816   0.001277  -3.771 0.000938 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.706 on 24 degrees of freedom
# Multiple R-squared:  0.7863,	Adjusted R-squared:  0.7596 
# F-statistic: 29.44 on 3 and 24 DF,  p-value: 3.273e-08

### Since we see that our model is significant (p<3.273e-08)
### we can exmaine individual variables with:
anova(model)
# Analysis of Variance Table
# 
# Response: y
#             Df  Sum Sq Mean Sq F value    Pr(>F)    
#   x2         1  76.193  76.193  26.172 3.100e-05 ***
#   x7         1 139.501 139.501  47.918 3.698e-07 ***
#   x8         1  41.400  41.400  14.221 0.0009378 ***
#   Residuals 24  69.870   2.911                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
### and we can see that all model components are significant.  The t-test
### statistics for the null hypotheses are:
#       t-stat  Pr(>|t|)    
#   x2   5.177 2.66e-05
#   x7   2.198 0.037815 
#   x8  -3.771 0.000938 
### Since these test statistics are more extreme than our critical value, 
### as evidenced by the p-values, there is significant statistical 
### evidence in our observed data to support the conclusion that 
### the partial derivatives of these three variables are non-zero.

### The R^2 and adjusted R^2 are given by:
# Multiple R-squared:  0.7863,	Adjusted R-squared:  0.7596

### The summary(model) command gives t-tests off of a type III sum
### of squares, but the partial-F statistics from anova are from type I.  Therefore, 
### if we want t-statistics from a type I sum of squares, we should take the square-root
### of the F-statistics given by anova(), and likewise square the t's from summary.lm() 
### to get an F-stat from a type III sum of squares.

###/-------------------------------------/

### Problem 3.2: Show numerically that r^2 = R^2

### Since R-squared: 0.9584
summary(model)$r.squared
# or (1434.18+14.96+165.84)/(1434.18+14.96+165.84+70.02)
cor(model$fitted.values, p3_1$y)^2
abs(summary(model)$r.squared - cor(model$fitted.values, p3_1$y)^2)
### Since this difference is fairly small, and fairly close to machine precision, 
### we can assert that these two values are numerically the same (which is what
### we would expect analytically)

###/-------------------------------------/

### Problem 3.3: 

### Part a: Construct a 95% CI on Beta_7.
confint(model)
### This constructs a Bonferroni Joint Confidence Interval around all
### parameters simultaneously, with endpoints for x7 of (0.011855322,  0.376065098)

### Part b: 
predict(model, data.frame(x2=2300, x7=56.0, x8 = 2100), interval='conf', level=.95)
### This establishes a 95% CI on the mean number of games won for the given
### values.  The CI is (6.436203, 7.996645)

###/-------------------------------------/

### Problem 3.4: 

model2 <- lm(y~x7+x8, data = p3_1)
summary(model2)
### The model is statistically significant.
#R-squared:  0.5477,	Adjusted R-squared:  0.5115 
### These values are lower for the reduced model than the "full" model with three
### regressors.
confint(model2)
#x7          -0.19716429  0.293906022
predict(model2, data.frame(x2=2300, x7=56.0, x8 = 2100), interval='conf', level=.95)
#5.828643 8.023842

### These are wider/longer CI's.

### Omission of an 'important' or significant regressor can increase the amount of variance
### attributed to error, which will increase the standard errors of your estimates and
### potentially lead to a failure to reject a null hypothesis (that the Beta is zero) when
### in fact it is non-zero.  This would be a type II error.

###/-------------------------------------/

### Problem 3.27: Attached, done by hand.

             