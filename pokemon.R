

# load packages and data --------------------------------------------------

library(tidyverse)
library(dslabs)
pokemon <- read.csv("pokemon.csv")

#objectif: classify whether legendary and/or mega OR NOT

# look at the data --------------------------------------------------------

glimpse(pokemon)

# praparing data ----------------------------------------------------------

pokemon_prep <-  mutate(pokemon, LegendaryMega = ifelse(Legendary == "TRUE" ,"TRUE", 
                                          ifelse(str_detect(Name, "Meganium"), "FALSE",
                                                        ifelse(str_detect(Name, "Mega"), "TRUE", "FALSE"))))

#making sure that everything works out
pokemon_prep %>% 
  select(-Type.1, -Type.2, -Generation) %>% 
  #filter(str_detect(Name, "Mega"))
  filter(!Legendary == TRUE)

pokemon_prep

#lets graph stuff

pokemon_g <- select(pokemon_prep, -X., -Name, -Type.1, -Type.2, -Legendary)
pokemon_g$LegendaryMega <- as.factor(pokemon_g$LegendaryMega)
glimpse(pokemon_g)

pokemon_g %>% 
  gather(stats, value,-LegendaryMega) %>% 
  ggplot(aes(x = stats, y = value, fill = LegendaryMega)) + 
  # geom_point() + 
  facet_wrap(~stats, scale = "free") + 
  geom_boxplot()


# the regression tree time ------------------------------------------------

library(rpart)

pokemon_tree <- pokemon_g
#set.seed(9999)

#kill_these <- sample(1:550, 25, replace = FALSE)
#pokemon_tree$HP[kill_these] <- NA_real_

#pokemon_tree$attack
pokemon_fit_full <- rpart(LegendaryMega ~ . , data = select(pokemon_tree, -Generation)) #read more on r part
olive_fit_full <- rpart(region ~ . , data = select(olive_ruined, - area))

#https://sebastiansauer.github.io/dplyr_filter/

plot(pokemon_fit_full)
text(pokemon_fit_full)

pokemon_fit_full


# random forest -----------------------------------------------------------

set.seed(4530)
forest <- randomForest(LegendaryMega ~ . , data = pokemon_g, localImp = TRUE)
forest

# Bibliographie -----------------------------------------------------------

#https://rstudio-pubs-static.s3.amazonaws.com/116317_e6922e81e72e4e3f83995485ce686c14.html#/
