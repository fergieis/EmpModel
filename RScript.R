
A <- read.csv("ModelData.csv")

m <- lm(X2015OBP~G + HR+R+SS+BB+PHI, data=A)
inf <- influence.measures(m)

library(car)
qqplot(m)
outlierTest(m)