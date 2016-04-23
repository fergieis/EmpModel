
A <- read.csv("HW-2Sp-16.csv", strip.white = TRUE, header=TRUE, sep = ",")
head(A)

library(MASS)
model <- lm(y ~ x8, data=A)
residuals <- studres(model)
qqnorm(residuals, ylab="R-Studentized Residuals", xlab="Normal Scores", main="y~x8 Normal Probability Plot") 
qqline(residuals) 

plot(residuals~model$fitted.values)
paste("Correlation of fitted values to the residuals:", toString(cor(residuals,model$fitted.values)))

plot(residuals~A$x2)
paste("Correlation of x2 and the residuals:", toString(cor(residuals,A$x2)))

plot(residuals~A$x7)
paste("Correlation of x7 and the residuals:", toString(cor(residuals,A$x7)))


model <- lm(y~x2+x7+x8, data = A)
residuals <- studres(model)
qqnorm(residuals, ylab="R-Studentized Residuals", xlab="Normal Scores", main="y~x2+x7+x8 Normal Probability Plot") 
qqline(residuals) 

plot(residuals~model$fitted.values)
paste("Correlation of fitted values to the residuals:", toString(cor(residuals,model$fitted.values)))

pairs(~residuals+x2+x7+x8, data=A, main="4.2 c. Residuals vs Regressors")


library(car)
avPlots(model, ~x2+x7+x8)

plot(residuals, main="Residuals", col="red", ylim=c(0,3), ylab="Residual")
par(new=TRUE, pch=20)
plot(rstandard(model), main="", col="blue", ylim=c(0,3), ylab="")
legend(10,2.9,c("R-Student Residuals","Studentized Residuals"), col=c("red", "blue"), pch=c(1, 20))

library(car)
model2 <- lm(y ~  x2+x7+x8, contrasts = c("contr.sum", "contr.poly"), 
    data = A)

Anova(model2, type = 3)
pairs(A)

library(corrplot)
corrplot(A)
