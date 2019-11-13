# load packages and data --------------------------------------------------

library(tidyverse)
library(dslabs)
library(rpart)

library(randomForest)

glimpse(iris)

iris_fit_full <- rpart(Species ~ . , data = iris)
plot(iris_fit_full)
text(iris_fit_full)

fit <- randomForest(Species ~ . , data = iris) #yeah gotta read up on random forest
plot(fit)

set.seed(4530)
forest <- randomForest(Species ~ . , data = iris, localImp = TRUE)
forest