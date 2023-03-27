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

#SPLIT INTO TRAINING TESTING, THEN RECLASSIFY AND DROP CLASSES
slide <- slide %>% arrange(code, time)

training <- slide %>% group_by(code) %>% slice_head(prop = .6) %>% ungroup %>% select(-time) 
testing <- slide %>% group_by(code) %>% slice_tail(prop = .4) %>% ungroup %>% select(-time) 

not_all_na <- function(x) any(!is.na(x))
training <- training %>% select_if(not_all_na)

rfmodel <- randomForest(code ~ ., data = training, localImp = TRUE, proximity = FALSE, ntree = 550, mtry = 44)
predictions <- predict(rfmodel, testing, type = "class")
u <- union(predictions, testing$code)
res <- confusion_matrix(factor(testing$code, u),factor(predictions, u))

print(res$`Balanced Accuracy`)
print(res$`Table`)
print(res$`Kappa`)


