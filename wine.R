
# objectifs ---------------------------------------------------------------

#combiner 2 jeux de données portant sur vin
#construire un arbre de décision pour classifier le vin
#construire un "random forest" pour classifier le vin
#interpréter le "random forest"

# téléchargement des "packages" et des jeux de données --------------------

library(tidyverse)
library(dslabs)
library(rpart)

library(randomForest)
library(randomForestExplainer)

wine_red <- read.csv("winequality-red.csv", sep = ";") %>%
  mutate(wine.type = "red")
glimpse(wine_red)

wine_white <- read.csv("winequality-white.csv", sep = ";") %>% 
  mutate(wine.type = "white")
glimpse(wine_white)

wine <- rbind(wine_red, wine_white) %>% select(-quality) #mesure subjective
#wine <- merge(wine_red, wine_white, by.x = colnames(), by.y = colnames())
wine$wine.type <- as.factor(wine$wine.type)
glimpse(wine)


# Arbre de décision -------------------------------------------------------

wine_fit_full <- rpart(wine.type ~ . , data = wine)
plot(wine_fit_full)
text(wine_fit_full)

# Random Forest -----------------------------------------------------------

set.seed(4530)
forest <- randomForest(wine.type ~ . , data = wine, localImp = TRUE, ntree = 1000)
forest

fit <- randomForest(wine.type ~ . , data = wine, ntree = 1000) 
plot(fit)

# Random Forest Explainer -------------------------------------------------

explain_forest(forest, interactions = TRUE, data = wine)

#Bibliographie
#https://stackoverflow.com/questions/20081256/combine-two-data-frames-with-the-same-column-names

