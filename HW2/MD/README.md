
# Homework 2
## OPER 679 - Empirical Modeling
###  MAJ Matthew Ferguson
This document produced using a jupyter notebook.
The version of the notebook server is 4.1.0-354a863 and is running on:

Python 2.7.11 |Anaconda 2.4.1 (64-bit)| (default, Dec  6 2015, 18:08:32) 
[GCC 4.4.7 20120313 (Red Hat 4.4.7-1)]

Current Kernel Information:

R version 3.2.2 (2015-08-14)using an R kernel, R version 3.2.2 (2015-08-14).


#### Textbook Problem 4.1
Consider the simple regression model fit to the National Football League team performance data in Problem 2.1


```R
A <- read.csv("HW-2Sp-16.csv", strip.white = TRUE, header=TRUE, sep = ",")
head(A)

```




<table>
<thead><tr><th></th><th scope=col>y</th><th scope=col>x1</th><th scope=col>x2</th><th scope=col>x3</th><th scope=col>x4</th><th scope=col>x5</th><th scope=col>x6</th><th scope=col>x7</th><th scope=col>x8</th><th scope=col>x9</th></tr></thead>
<tbody>
	<tr><th scope=row>1</th><td>10</td><td>2113</td><td>1985</td><td>38.9</td><td>64.7</td><td>4</td><td>868</td><td>59.7</td><td>2205</td><td>1917</td></tr>
	<tr><th scope=row>2</th><td>11</td><td>2003</td><td>2855</td><td>38.8</td><td>61.3</td><td>3</td><td>615</td><td>55</td><td>2096</td><td>1575</td></tr>
	<tr><th scope=row>3</th><td>11</td><td>2957</td><td>1737</td><td>40.1</td><td>60</td><td>14</td><td>914</td><td>65.6</td><td>1847</td><td>2175</td></tr>
	<tr><th scope=row>4</th><td>13</td><td>2285</td><td>2905</td><td>41.6</td><td>45.3</td><td>-4</td><td>957</td><td>61.4</td><td>1903</td><td>2476</td></tr>
	<tr><th scope=row>5</th><td>10</td><td>2971</td><td>1666</td><td>39.2</td><td>53.8</td><td>15</td><td>836</td><td>66.1</td><td>1457</td><td>1866</td></tr>
	<tr><th scope=row>6</th><td>11</td><td>2309</td><td>2927</td><td>39.7</td><td>74.1</td><td>8</td><td>786</td><td>61</td><td>1848</td><td>2339</td></tr>
</tbody>
</table>




a) Construct a normal probability plot of the residuals.  Does there seem to be any problem with the normality assumption?


```R
library(MASS)
model <- lm(y ~ x8, data=A)
residuals <- studres(model)
qqnorm(residuals, ylab="R-Studentized Residuals", xlab="Normal Scores", main="y~x8 Normal Probability Plot") 
qqline(residuals) 
```


![svg](output_4_0.svg)


By the "fat pencil" test, there do not appear to be significant deviations from normality.  There is some indication of being "light-tailed" if you use a particularly thin pencil, but the rule of thumb is a "fat pencil".

b)  Construct and interpret a plot of the residuals versus the predicted response.



```R
plot(residuals~model$fitted.values)
paste("Correlation of fitted values to the residuals:", toString(cor(residuals,model$fitted.values)))
```




'Correlation of fitted values to the residuals: -0.00822232826654008'




![svg](output_6_1.svg)


There appears to be no correlation between the fitted values and the residuals.  If a stronger correlation were present, that would imply that there is additional variability in the model that could be explained by the regressor.  We could then proceed to evaluate some non-linear term of the regressor or an interaction term in an attempt to explain this additional variability.

c)  Plot the residuals versus the team passing yardage, x<sub>2</sub>.   Does this plot indicate that the model will be improved by adding x<sub>2</sub> to the model?

Repeat for x<sub>7</sub>.


```R
plot(residuals~A$x2)
paste("Correlation of x2 and the residuals:", toString(cor(residuals,A$x2)))

plot(residuals~A$x7)
paste("Correlation of x7 and the residuals:", toString(cor(residuals,A$x7)))

```




'Correlation of x2 and the residuals: 0.664607506893894'




![svg](output_8_1.svg)





'Correlation of x7 and the residuals: 0.0536055882481425'




![svg](output_8_3.svg)


Since x<sub>2</sub> appears to have a correlation with the model residuals, there is additional variability in the model that can be explained by adding x<sub>2</sub> as a regressor. 

x<sub>7</sub> has a slight correlation, but is practically zero.  In this case, the addition of x<sub>7</sub> would likely not contribute much towards explaining the variance in our model.  I would prefer to add x<sub>2</sub> before adding x<sub>7</sub> since x<sub>2</sub> has a greater correlation.  The addition of x<sub>2</sub>  to the model would also be predicated on the juxtaposition of the parsimony of the expanded model to its marginal ability to explain variance.  This juxtaposition can be evaluated by contrasting the adjusted R<sup>2</sup>  of the expanded model and the adjusted R<sup>2</sup> of the current model.  I would then see if there was a significant enough change to subsequently add x<sub>2</sub>.  

#### Textbook problem 4.2
Consider the multiple regression model fit to the National Football League team performance data in Problem 3.1. 

a) Construct a normal probabilty plot of the residuals.


```R
model <- lm(y~x2+x7+x8, data = A)
residuals <- studres(model)
qqnorm(residuals, ylab="R-Studentized Residuals", xlab="Normal Scores", main="y~x2+x7+x8 Normal Probability Plot") 
qqline(residuals) 
```


![svg](output_11_0.svg)


There does not appear to be any problem with the normality assumption. While there is some minor deviation from the line, the plot passes the "fat pencil" heuristic.

b) Construct and interpret a plot of the residuals versus the predicted response.

First, I will copy and paste the code from question 4.1 b).



```R
plot(residuals~model$fitted.values)
paste("Correlation of fitted values to the residuals:", toString(cor(residuals,model$fitted.values)))
```




'Correlation of fitted values to the residuals: 0.00233994212215379'




![svg](output_13_1.svg)


It is as expected, since there is no correlation or pattern between the fitted model values and the residuals.

c) Construct plots of the residuals versus each of the regressor variables. Do these plots imply that the regressor is correctly specified?

We can examine these plots as the first row of a scatterplot matrix.


```R
pairs(~residuals+x2+x7+x8, data=A, main="4.2 c. Residuals vs Regressors")

```


![svg](output_15_0.svg)


From here, our only possible concern might be possible heteroscedacity in the second and third (residual ~ x<sub>7</sub> and residual~x<sub>8</sub> ) plots.  In each case, we may wish to perform a transformation on the response variable to ensure better fit.  Or we can throw up our hands, say "close enough for government work" and grab a beer.  It's Friday night, after all.

d) Do partial regression/avp plots. (below).
The only one that has something funky (a technical term, of course) going on is x<sub>7</sub>.  There is alot of deviation, but then again some winning teams run the ball alot.  Some losing teams also run ball alot. Perhaps there's an interaction that cannot be determined by this plot between regressors.  Likely there is an underlying relationship between the percentage of running plays and the total running yards achieved.  Further analysis will likely be required to ensure that we are ignoring some interaction between running play percentage and rushing yardage.


```R
library(car)
avPlots(model, ~x2+x7+x8)
```


![svg](output_17_0.svg)


e) Compute the studentized and the R-studentized residuals for this model.  What information is conveyed by these scaled residuals?

This answer is almost trivial, but the values in <i> residuals </i> are the R-studentized residuals.  A simple function call to rstandard yields the Studentized residuals.  These are plotted below against thier index/observation order.  These scaled residuals account (or attempt to account) for the non-constant variance of uncorrected residuals.



```R
plot(residuals, main="Residuals", col="red", ylim=c(0,3), ylab="Residual")
par(new=TRUE, pch=20)
plot(rstandard(model), main="", col="blue", ylim=c(0,3), ylab="")
legend(10,2.9,c("R-Student Residuals","Studentized Residuals"), col=c("red", "blue"), pch=c(1, 20))
```


![svg](output_19_0.svg)



```R
library(car)
model2 <- lm(y ~  x2+x7+x8, contrasts = c("contr.sum", "contr.poly"), 
    data = A)
Anova(model2, type = 3)
pairs(A)
```




<table>
<thead><tr><th></th><th scope=col>Sum Sq</th><th scope=col>Df</th><th scope=col>F value</th><th scope=col>Pr(>F)</th></tr></thead>
<tbody>
	<tr><th scope=row>(Intercept)</th><td>0.1525129</td><td>1</td><td>0.05238741</td><td>0.820899</td></tr>
	<tr><th scope=row>x2</th><td>78.02809</td><td>1</td><td>26.80226</td><td>2.655723e-05</td></tr>
	<tr><th scope=row>x7</th><td>14.06819</td><td>1</td><td>4.832354</td><td>0.03781516</td></tr>
	<tr><th scope=row>x8</th><td>41.40006</td><td>1</td><td>14.22072</td><td>0.0009377699</td></tr>
	<tr><th scope=row>Residuals</th><td>69.87</td><td>24</td><td>NA</td><td>NA</td></tr>
</tbody>
</table>







<table>
<thead><tr><th></th><th scope=col>y</th><th scope=col>x1</th><th scope=col>x2</th><th scope=col>x3</th><th scope=col>x4</th><th scope=col>x5</th><th scope=col>x6</th><th scope=col>x7</th><th scope=col>x8</th><th scope=col>x9</th></tr></thead>
<tbody>
	<tr><th scope=row>y</th><td> 1.00000000</td><td> 0.59323604</td><td> 0.48273470</td><td>-0.08081247</td><td> 0.25847477</td><td> 0.51320624</td><td> 0.22403447</td><td> 0.54534104</td><td>-0.73802730</td><td>-0.30374811</td></tr>
	<tr><th scope=row>x1</th><td> 0.59323604</td><td> 1.00000000</td><td>-0.03674736</td><td> 0.21247123</td><td> 0.07029904</td><td> 0.59998017</td><td> 0.25297272</td><td> 0.83728269</td><td>-0.65854627</td><td>-0.11055739</td></tr>
	<tr><th scope=row>x2</th><td> 0.48273470</td><td>-0.03674736</td><td> 1.00000000</td><td>-0.06881516</td><td> 0.30151583</td><td> 0.13499515</td><td>-0.19283713</td><td>-0.19691540</td><td>-0.05104783</td><td> 0.14598149</td></tr>
	<tr><th scope=row>x3</th><td>-0.080812472</td><td> 0.212471227</td><td>-0.068815157</td><td> 1.000000000</td><td>-0.413095614</td><td> 0.115098074</td><td>-0.003115748</td><td> 0.162511469</td><td> 0.290438108</td><td> 0.088195595</td></tr>
	<tr><th scope=row>x4</th><td> 0.25847477</td><td> 0.07029904</td><td> 0.30151583</td><td>-0.41309561</td><td> 1.00000000</td><td> 0.14902865</td><td>-0.12818435</td><td>-0.10100316</td><td>-0.16402353</td><td> 0.05913611</td></tr>
	<tr><th scope=row>x5</th><td> 0.51320624</td><td> 0.59998017</td><td> 0.13499515</td><td> 0.11509807</td><td> 0.14902865</td><td> 1.00000000</td><td> 0.25891534</td><td> 0.60956318</td><td>-0.47004608</td><td>-0.09028906</td></tr>
	<tr><th scope=row>x6</th><td> 0.224034472</td><td> 0.252972716</td><td>-0.192837129</td><td>-0.003115748</td><td>-0.128184348</td><td> 0.258915336</td><td> 1.000000000</td><td> 0.367077900</td><td>-0.352493271</td><td>-0.172756078</td></tr>
	<tr><th scope=row>x7</th><td> 0.5453410</td><td> 0.8372827</td><td>-0.1969154</td><td> 0.1625115</td><td>-0.1010032</td><td> 0.6095632</td><td> 0.3670779</td><td> 1.0000000</td><td>-0.6850457</td><td>-0.2033178</td></tr>
	<tr><th scope=row>x8</th><td>-0.73802730</td><td>-0.65854627</td><td>-0.05104783</td><td> 0.29043811</td><td>-0.16402353</td><td>-0.47004608</td><td>-0.35249327</td><td>-0.68504573</td><td> 1.00000000</td><td> 0.41746519</td></tr>
	<tr><th scope=row>x9</th><td>-0.30374811</td><td>-0.11055739</td><td> 0.14598149</td><td> 0.08819559</td><td> 0.05913611</td><td>-0.09028906</td><td>-0.17275608</td><td>-0.20331784</td><td> 0.41746519</td><td> 1.00000000</td></tr>
</tbody>
</table>





    Error in eval(expr, envir, enclos): could not find function "pca"




![svg](output_20_3.svg)



```R
model3 <- lm(y ~  0+x2+x7+x8, data = A) #repeatedly changed to examine poly(xn,2) and full model on down...
summary(model3)
r3 <- studres(model3)

```




    
    Call:
    lm(formula = y ~ 0 + x2 + x7 + x8, data = A)
    
    Residuals:
        Min      1Q  Median      3Q     Max 
    -2.9707 -0.6953 -0.2601  1.0570  3.7442 
    
    Coefficients:
         Estimate Std. Error t value Pr(>|t|)    
    x2  0.0035213  0.0005969   5.899 3.73e-06 ***
    x7  0.1747501  0.0266986   6.545 7.41e-07 ***
    x8 -0.0050642  0.0006581  -7.695 4.74e-08 ***
    ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
    
    Residual standard error: 1.674 on 25 degrees of freedom
    Multiple R-squared:  0.9584,	Adjusted R-squared:  0.9535 
    F-statistic: 192.2 on 3 and 25 DF,  p-value: < 2.2e-16





```R
##### par(mfrow=c(1,1))
plot(residuals~A$y)

#Possible additional regressors for the dataset
plot(residuals~A$x1)
plot(residuals~A$x3)
plot(residuals~A$x4)
plot(residuals~A$x5)
plot(residuals~A$x6)
plot(residuals~A$x9)

#Possible additional regressors for a polynomial fit
plot(residuals~A$x2)
plot(residuals~A$x7)
plot(residuals~A$x8)

```


![svg](output_22_0.svg)



![svg](output_22_1.svg)



![svg](output_22_2.svg)



![svg](output_22_3.svg)



![svg](output_22_4.svg)



![svg](output_22_5.svg)



![svg](output_22_6.svg)



![svg](output_22_7.svg)



![svg](output_22_8.svg)



![svg](output_22_9.svg)


After examination of each of the residuals, there do not appear to be any other significant factors that we would wish to add as regressors.  Just for curiosity, I did explore some possible polynomial regressors (mostly to explore how they are accomplished in R using the poly() function) but none of these regressors were significant.  There is some mild heteroscedasticity observed in the residuals with respect to some of the remaining data, identified as a mild "vee" shape from changes in variance over changes in the possible additional regressor ("x").  However, no linear or polynomial relationship expressed.  The observed heteroscedasticity might be due to an unobserved factor that has a contributing relationship with these variables.  Just to confirm the linear model was well constructed, I lastly constructed a full regression model with all nine variables and singly removed the regressors with the highest t-statistic.  This process resulted in the same three-regressor model with x<sub>2</sub>, x<sub>7</sub> and x<sub>8</sub>.
