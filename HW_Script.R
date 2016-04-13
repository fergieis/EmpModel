# Empirical Modeling HW1
##MAJ Matthew Ferguson
##

setwd("~/Desktop/EmpModel/")

## Problem 2.7
### First of all, there is a typo on the chart in the book ("Purily" vs. "Purity").
### Problem set-up: I've adjusted the datafiles to a csv format
p2_7 <- read.csv("Problem2_7.csv", strip.white = TRUE, header=TRUE, sep = ",")

###Part a: Fit a simple linear regression to the data
### Construct a linear model of purity as a function of hydro
model <- lm(purity~hydro, data=p2_7)

###/----------------------------------------/

###Part b: Test H_0: Beta_1 =0
### Let H_a be Beta_1 != 0

summary(model)
# Call:
#   lm(formula = purity ~ hydro, data = p2_7)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -4.6724 -3.2113 -0.0626  2.5783  7.3037 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     77.863      4.199  18.544 3.54e-13 ***
#   hydro         11.801      3.485   3.386  0.00329 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3.597 on 18 degrees of freedom
# Multiple R-squared:  0.3891,	Adjusted R-squared:  0.3552 
# F-statistic: 11.47 on 1 and 18 DF,  p-value: 0.003291


### The probability of observing a more extreme value than our test statistic, t
### (or the "Pr(>|t|)" value) is only 3.29E-3, implying that it is unlikely we 
### observed such as extreme value by chance.  Based upon our sample date, we should
### then reject H_0 in favor of H_a.

###/----------------------------------------/

### Part c: Calculate R^2

### The second to the last line of the summary(model) command provides
# Multiple R-squared:  0.3891,	Adjusted R-squared:  0.3552
### To access directly:
summary(model)$r.squared
# [1] 0.3891224
###The unadjusted correlation coefficient is then roughly .39

###/----------------------------------------/

### Part d: Find a 95% CI on the slope
### If we desire a simultaneous estimate of slope and intercept 
### (using the Bonferroni method) we could use:
confint(model, level=.975)
#               1.25 %  98.75 %
# (Intercept) 67.596979 88.12959
# hydro        3.279893 20.32216

### Since the intercept is significant, I would recommend this.
### However, we can manually calculate the 95% CI 
### of just the slope with:
model.coefs <- coef(summary(model))
alpha=.05
n<-length(p2_7$hydro)

slope.est <- model.coefs[2]
slope.stderr <- model.coefs[4]

lower <- slope.est - qt(1 - alpha/2, n-2) * slope.stderr
upper <- slope.est + qt(1 - alpha/2, n-2) * slope.stderr

CI <- c(lower, upper)
print(CI, digits=3)
#[1]  4.48 19.12
### these are the lower and upper bounds of the 95% CI of the slope

###/----------------------------------------/

### Part e: Find a 95% conf interval on purity 
### when the hydrocarbon percentage is 1.00.
predict(model, data.frame(hydro=1), interval='conf', level=.95)
# fit        lwr      upr
# 1 89.66431 87.51017 91.81845
### Therefore the CI is (87.5, 91.8).

###/----------------------------------------/
###/----------------------------------------/
## Problem 2.8
### Problem set-up: I've adjusted the datafiles to a csv format
### and this uses the same data as Problem 2.7.
r <- sqrt(summary(model)$r.squared)
### or cor(p2_7$purity, p2_7$hydro), but that might
### not demonstrate my understanding of the material

t_nought <- r * sqrt(n-2) / (sqrt(1-r^2))
#3.386119
qt(1-alpha/2,n-2)
#2.100922
### Since our test statistic is more extreme than our critical
### value, based upon this data we would reject the null hypothesis 
### that rho=0 in favor of the alternative hypothesis that rho != 0.
library(psych)
r.con(r,20)
###This constructs a 95% CI about rho, with endpoints:
#[1] 0.2503961 0.8356439
### Why psych package? Why is a Scheffe CI in a Peruvian agricultural analysis tool? 
### "Into the valley of Death, rode the six hundred" -Tennyson

###/----------------------------------------/
###/----------------------------------------/
### Problem 2.10

p2_10 <- read.csv("Problem2_10.csv", strip.white = TRUE, header=TRUE, sep = ",")

model2 <- lm(sys.bp~weight, data=p2_10)
# Call:
#   lm(formula = sys.bp ~ weight, data = p2_10)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -17.182  -6.485  -2.519   8.926  12.143 
# 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 69.10437   12.91013   5.353 1.71e-05 ***
#   weight    0.41942    0.07015    5.979 3.59e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 8.681 on 24 degrees of freedom
# Multiple R-squared:  0.5983,	Adjusted R-squared:  0.5815 
# F-statistic: 35.74 on 1 and 24 DF,  p-value: 3.591e-06

### Therefore the regression model is significant, with estimated 
### coefficients of:

#> model2$coefficients
#(Intercept)      weight 
# 69.1043728   0.4194152 

### Taking the easy route...
model2.r <- cor(p2_10$weight, p2_10$sys.bp)
n <- 26

### Basically copy and paste from previous
model2.t_nought <- model2.r * sqrt(n-2) / (sqrt(1-model2.r^2))
#5.177657
#### Since this is also more extreme than our test statistic of qt(1-alpha/2,n-2), 
### or 2.063899, we can reject the null hypothesis in favor of the alternative
### that rho != 0.

(atanh(model2.r)-atanh(.6))*sqrt(n-3)
#1.610495
### Since this is less that our critcal value of Z, at the given significance level
### we fail to reject the null hypothesis that rho =.6.

r.con(model2.r, n)
### this provides a 95% confidence on rho, or (0.5513214, 0.8932215)

###/----------------------------------------/
###/----------------------------------------/

### Problem 2.11

model3 <- lm(sys.bp~0+weight, data=p2_10)
summary(model3)
# Call:
#   lm(formula = sys.bp ~ 0 + weight, data = p2_10)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -26.245  -0.902   6.170  10.254  16.838 
# 
# Coefficients:
#         Estimate Std. Error t value Pr(>|t|)    
# weight  0.79164    0.01343   58.97   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 12.6 on 25 degrees of freedom
# Multiple R-squared:  0.9929,	Adjusted R-squared:  0.9926 
# F-statistic:  3477 on 1 and 25 DF,  p-value: < 2.2e-16

### The estimate for the slope increased (i.e. steeper) roughly by a factor of 2, 
### but so did the standard error.  Since the intercept is significant, and we are
### increasing the error in our slope estimate in the reduced model, I would prefer
### the full model that includes the intercept.

###/-------------------------------------/

### Problem 2.26: Attached, completed by hand.
