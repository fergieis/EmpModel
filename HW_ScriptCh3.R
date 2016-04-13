# Empirical Modeling HW1
##MAJ Matthew Ferguson
##

setwd("~/Desktop/EmpModel/")

## Problem 3,1
p3_1 <- read.csv("TableB_1.csv", strip.white = TRUE, header=TRUE, sep = ",")

model <- lm(y~0+x2+x7+x8, data = p3_1)
summary(model)
# Call:
#   lm(formula = y ~ 0 + x2 + x7 + x8, data = p3_1)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.9707 -0.6953 -0.2601  1.0570  3.7442 
# 
# Coefficients:
#       Estimate   Std. Error t value Pr(>|t|)    
#   x2  0.0035213  0.0005969   5.899  3.73e-06 ***
#   x7  0.1747501  0.0266986   6.545  7.41e-07 ***
#   x8 -0.0050642  0.0006581  -7.695  4.74e-08 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.674 on 25 degrees of freedom
# Multiple R-squared:  0.9584,	Adjusted R-squared:  0.9535 
# F-statistic: 192.2 on 3 and 25 DF,  p-value: < 2.2e-16

anova(model)
# Analysis of Variance Table
# 
# Response: y
# Df  Sum Sq Mean Sq  F value    Pr(>F)    
# x2         1 1434.18 1434.18 512.0408 < 2.2e-16 ***
# x7         1   14.96   14.96   5.3423   0.02934 *  
# x8         1  165.84  165.84  59.2092  4.74e-08 ***
# Residuals 25   70.02    2.80    

