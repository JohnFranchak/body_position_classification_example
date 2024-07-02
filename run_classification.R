library(tidyverse)
library(here)
library(randomForest)
library(cvms)
library(caret)

load(here("synced_data", "mot_features.RData"))

#CODE FACTORS
slide$code = ifelse(slide$code == "sr", "ss", slide$code)
slide$code <- factor(slide$code, levels = c("hs", "l","p","ss","u"), labels = c("Held", "Supine","Prone","Sitting","Upright"))
slide <- slide %>% filter(code_prop > .75) %>% drop_na(code) %>% select(-code_prop)

#SPLIT INTO TRAINING AND TESTING, DROP TIME VARIABLE AND NA FEATURES
slide <- slide %>% arrange(code, time)
training <- slide %>% group_by(code) %>% slice_head(prop = .6) %>% ungroup %>% select(-time) 
testing <- slide %>% group_by(code) %>% slice_tail(prop = .4) %>% ungroup %>% select(-time) 

not_all_na <- function(x) any(!is.na(x))
training <- training %>% select_if(not_all_na)

# FIT THE MODEL FROM TRAINING, PREDICT LABELS FROM TESTING
rfmodel <- randomForest(code ~ ., data = training, localImp = TRUE, proximity = FALSE, ntree = 550, mtry = 44)
predictions <- predict(rfmodel, testing, type = "class")

# GET CONFUSION MATRIX
u <- union(predictions, testing$code)
res <- confusion_matrix(factor(testing$code, u),factor(predictions, u))
print(res$`Overall Accuracy`)
print(res$`Table`)
print(res$`Kappa`)

# VISUALIZE DISAGREEMENTS
agree = tibble(actual = factor(testing$code, u), prediction = factor(predictions, u))
agree$disagree <- factor(agree$actual != agree$prediction, levels = c(TRUE, FALSE))
agree$agree <- factor(agree$actual != agree$prediction, levels = c(TRUE, FALSE))

agree$time <- slide %>% group_by(code) %>% slice_tail(prop = .4) %>% pull(time)
agree$time <- (agree$time - agree$time[1])/60

ggplot(agree) + 
  geom_tile(aes(x = time, y = 1, fill = prediction)) +
  geom_raster(data = filter(agree, disagree == TRUE), aes(x = time, y = 0)) + 
  geom_tile(aes(x = time, y = -1, fill = actual)) + 
  scale_y_continuous(breaks = c(-1, 1), labels = c("Code","Prediction")) +
  scale_x_continuous(breaks = seq(0,90,5)) +
  ylab("") + theme(legend.position = "top") 

