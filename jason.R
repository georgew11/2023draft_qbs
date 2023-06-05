#set working directory
setwd("/Users/georgewhite/Documents/draft23")

#R Version 4.1.1
#load packages
library(dplyr)#--------v 1.0.10
library(ggplot2)#------v 3.3.6
library(cfbfastR)#-----v 1.9.0
library(tidyr)#--------v 1.2.1
library(readxl)#-------v 1.4.1
library(corrplot)#-----v 0.92
library(nnet)#---------v 7.3.18
library(neuralnet)#----v 1.44.2
library(car)#----------v 3.1.1
library(rsq)#----------v 2.5
library(ggrepel)#------v 0.9.2
library(GGally)#-------v 2.1.2
library(caret)#--------v 6.0.93
library(vip)#----------v 0.3.2
library(tidymodels)  # for the tune package, along with the rest of tidymodels
library(ggrepel)

# Helper packages
library(rpart.plot)

library(tidyverse)
library(digest)
library(lares)
library(caret)
library(ranger)
library(rsq)
library(rvest)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(psych)
library(lmtest)
library(nortest)
library(corrr)
library(corrplot)
library(car)

#read data
# xfl <- read.csv("all_qbs.csv", row.names = FALSE)
# xfl <- xfl[-1]
# 
# #move columns
# xfl <- xfl %>% 
#   relocate(player, level, av_game, seasons, games, value, .after = athlete_id)
# 
# #avg columns
# xfl <- xfl %>% 
#   mutate(avg_PPA_all = total_PPA_all / countable_plays,
#          avg_PPA_pass = total_PPA_pass / passing_att,
#          avg_PPA_rush = total_PPA_rush / rushing_car)
# 
# 
# xfl_2 <- merge(xfl, rus[, c("season", "athlete_id")], by = "athlete_id")
# xfl_2 <- xfl_2 %>% 
#   relocate(season, .after = player)
# 
# write.csv(xfl_2, file = "final_qb.csv")
xfl_2 <- read.csv("final_qb.csv")
xfl_3 <- xfl_2 %>% 
  filter(is.na(level) == FALSE | season == 2022)

xfl_3$level <- ifelse(is.na(xfl_3$level ), "NCAA", xfl_3$level)

xfl_final <- subset(xfl_3, !duplicated(xfl_3$athlete_id))

#graph
xfl_final %>% 
  ggplot(aes(x = avg_PPA_rush, y = avg_PPA_pass, col = level)) +
  geom_point(alpha = .9) +
  geom_label_repel(aes(label=ifelse(avg_PPA_rush>.55 | 
                                      avg_PPA_pass > .55 | 
                                      player == "Bryce Young" |
                                      player == "Will Levis" |
                                      player == "Hendon Hooker" |
                                      player == "Drake Maye",
                                    player,'')),hjust=-.1,vjust=0, max.overlaps = 20)
  
draft_qbs <- c("Bryce Young", "Will Levis", 
               "C.J. Stroud", "Anthony Richardson",
               "Tanner McKee", "Hendon Hooker",
               "Caleb Williams", "Drake Maye",
               "Jayden Daniels", "Spencer Rattler",
               "Quinn Ewers", "Stetson Bennett",
               "Bo Nix", "Michael Pratt",
               "KJ Jefferson", "Sam Howell",
               "Jaren Hall", "Clayton Tune",
               "Aidan O'Connell", "Jake Haener",
               "Max Duggan", "Dorian Thompson-Robinson",
               "Malik Cunningham", "Myles Brennan",
               "Adrian Martinez", "Todd Centeio",
               "Tanner Morgan", "Sean Clifford")

pros <- xfl_final %>% 
  filter(season != 2022 | player %in% draft_qbs)


pros$level <- factor(pros$level, levels = c("HOF", "Pro_Bowl", "Franchise",
                                            "Backup", "Bad", "TBD", "NCAA"))


pros %>% 
  ggplot(aes(x = avg_PPA_rush, y = avg_PPA_pass, col = level)) +
  geom_point(alpha = .9) +
  geom_label_repel(aes(label=ifelse(avg_PPA_rush>.55 | 
                                      avg_PPA_pass > .55 | 
                                      player %in% draft_qbs,
                                    player,'')),hjust=-.1,vjust=0, max.overlaps = 20)


pros %>% 
  ggplot(aes(x = avg_PPA_rush, y = td_int, col = level)) +
  geom_point(alpha = .9) +
  geom_label_repel(aes(label=ifelse(avg_PPA_rush>.55 | 
                                      avg_PPA_pass > .55 | 
                                      player %in% draft_qbs,
                                    player,'')),hjust=-.1,vjust=0, max.overlaps = 20)


pros %>% 
  ggplot(aes(x = usg_overall, y = avg_PPA_pass, col = level, size = td_int)) +
  geom_point(alpha = .9) +
  geom_label_repel(aes(label=ifelse(avg_PPA_rush>.55 | 
                                      avg_PPA_pass > .55 | 
                                      player %in% draft_qbs,
                                    player,'')),hjust=-.1,vjust=0, max.overlaps = 20)
realqb <- c("Bryce Young", "Will Levis", 
            "C.J. Stroud", "Anthony Richardson",
            "Hendon Hooker",
            "Caleb Williams", "Drake Maye",
            "Quinn Ewers", "Stetson Bennett",
            "Sam Howell")

pros %>% 
  ggplot(aes(x = usg_overall, y = avg_PPA_pass, col = level, size = td_int)) +
  geom_point(alpha = .9) +
  geom_label_repel(aes(label=ifelse(avg_PPA_rush>.55 | 
                                      avg_PPA_pass > .55 | 
                                      player %in% realqb |
                                      level == "Pro_Bowl" |
                                      level == "HOF",
                                    player,'')),hjust=-.1,vjust=0, max.overlaps = 20)


pros %>% 
  ggplot(aes(x = passing_pct, y = avg_PPA_rush, col = level, size = td_int)) +
  geom_point(alpha = .9) +
  geom_label_repel(aes(label=ifelse(avg_PPA_rush>.55 | 
                                      avg_PPA_pass > .55 | 
                                      player %in% realqb |
                                      level == "Pro_Bowl" |
                                      level == "HOF",
                                    player,'')),hjust=-.1,vjust=0, max.overlaps = 20)

pros <- pros %>% 
  mutate(yards_per_carry = rushing_yds / rushing_car)

install.packages("viridis")  # Install
library("viridis")  




devtools::install_github("jaredhuling/jcolors")

library(jcolors)

pros %>% 
  ggplot(aes(x = passing_pct, y = passing_ypa, col = level)) +
  geom_point(alpha = .9) +
  geom_label_repel(aes(label=ifelse(avg_PPA_rush>.55 | 
                                      avg_PPA_pass > .55 | 
                                      player %in% realqb |
                                      level == "Pro_Bowl" |
                                      level == "HOF",
                                    player,'')),hjust=-.1,vjust=0, 
                   max.overlaps = 20) +
  scale_color_jcolors(palette = "pal6") 

pros %>% 
  ggplot(aes(x = avg_PPA_pass, y = avg_PPA_rush, col = level)) +
  geom_point(alpha = .9) +
  geom_label_repel(aes(label=ifelse(avg_PPA_rush>.55 | 
                                      avg_PPA_pass > .55 | 
                                      player %in% realqb |
                                      level == "Pro_Bowl" |
                                      level == "HOF",
                                    player,'')),hjust=-.1,vjust=0, 
                   max.overlaps = 20) +
  scale_color_jcolors(palette = "pal6") 

#LETS MAKE A TREE---------------------------------------------------------------
#split
last <- pros %>% 
  filter(level %in% c("HOF", "Pro_Bowl", "Franchise", "Backup", "Bad"))

last <- last[-1]
last$level <- droplevels(last$level, c("TBD", "NCAA"))
set.seed(123)
df_split <- initial_split(last, strata = level)
df_train <- training(df_split)
df_test  <- testing(df_split)




pro_dt3 <- train(
  level ~ .,
  data = df_train[,-c(1:3,5:21,23:26)],
  method = "rpart",
  trControl = trainControl(method = "cv", number = 10, 
                           preProcOptions = "center", "scale",),
  tuneLength = 100
)

pro_dt3
plot(pro_dt3)

predictions <- predict(pro_dt3, df_test)
df_test <- cbind(df_test, predictions)
df_test <- df_test %>% 
  rename(dt_pred_2 = predictions)
df_test <- df_test %>% 
  relocate(dt_pred_2, .after = level)

library(rattle)
plot(pro_dt3$finalModel)


pro_nnet <- train(
  level ~ .,
  data = df_train[,-c(1:3,5:21,23:26)],
  method = "pcaNNet",
  trControl = trainControl(method = "cv", number = 10, 
                           preProcOptions = "center", "scale",)
)

predictions_nnet <- predict(pro_nnet, df_test)
df_test <- cbind(df_test, predictions_nnet)
df_test <- df_test %>% 
  rename(dt_pred_nnet = predictions_nnet)
df_test <- df_test %>% 
  relocate(dt_pred_nnet, .after = dt_pred_2)


predictions_nnet <- predict(pro_nnet, df_train)
df_train <- cbind(df_train, predictions_nnet)
df_train <- df_train %>% 
  rename(dt_pred_nnet = predictions_nnet)
df_train <- df_train %>% 
  relocate(dt_pred_nnet, .after = level)

confusionMatrix(pro_nnet)

payne <- last

# Combine levels "A", "B", and "C" into "Low", and "D" and "E" into "High"
payne$level <- ifelse(payne$level == "Bad" | payne$level == "Backup", "Bad",
                      ifelse(payne$level == "Franchise", "Franchise", "Star"))

set.seed(123)
payne_split <- initial_split(payne, strata = level)
payne_train <- training(payne_split)
payne_test  <- testing(payne_split)

payne_nnet <- train(
  level ~ .,
  data = df_train[,-c(1:3,5:21,23:26)],
  method = "dnn",
  trControl = trainControl(method = "cv", number = 10, 
                           preProcOptions = "center", "scale")
)


plot(payne_nnet)

predictions_nnet_2 <- predict(payne_nnet, payne_test)
payne_test <- cbind(df_test, predictions_nnet_2)
payne_test <- payne_test %>% 
  rename(dt_pred_nnet_2 = predictions_nnet_2)
payne_test <- payne_test %>% 
  relocate(dt_pred_nnet_2, .after = level)

confusionMatrix(payne_nnet)

#NEW DF-------------------------------------------------------------------------

allen <- payne
allen <- allen[,c(1:8,22,27:45)]

set.seed(998)
inTraining <- createDataPartition(allen$level, p = .75, list = FALSE)
training <- allen[ inTraining,]
testing  <- allen[-inTraining,]

tgrid <- expand.grid(layer1 = 1:3,
                     layer2 = 0:3, layer3 = 0:3,
                     hidden_dropout = c(0, .1), 
                     visible_dropout = 0)

allen_nnet <- train(
  level ~ .,
  data = training[,-c(1:3,5:8)],
  method = "dnn",
  metric = "ROC",
  tuneGrid = tgrid,
  trControl = trainControl(method = "cv", number = 10,
                           sampling = "smote",
                           classProbs = TRUE)
)

confusionMatrix(allen_nnet)


predictions_nnet <- predict(allen_nnet, testing, type = "prob")
testing <- cbind(testing, predictions_nnet)
testing <- testing %>% 
  rename(dt_pred_nnet_2 = predictions_nnet)
testing <- testing %>% 
  relocate(Bad, Franchise, Star, .after = level)

#fda-------------------------------
tgrid_2 <- expand.grid(degree = 1:5,
                       nprune = 1:10)

allen_fda <- train(
  level ~ .,
  data = training[,-c(1:3,5:8)],
  method = "fda",
  metric = "Accuracy",
  # tuneGrid = tgrid_2,
  trControl = trainControl(method = "cv", number = 10,
                           sampling = "smote",
                           classProbs = TRUE)
)

confusionMatrix(allen_fda)

options(scipen = 100)

predictions_fda <- predict(allen_fda, testing, type = "prob")
testing <- cbind(testing, predictions_fda)
testing <- testing %>% 
  rename(dt_pred_nnet_2 = predictions_nnet)
testing <- testing %>% 
  relocate(Bad, Franchise, Star, .after = level)

predictions_fda <- predict(allen_fda, training, type = "prob")
training <- cbind(training, predictions_fda)
# training <- training %>% 
#   rename(dt_pred_nnet_2 = predictions_nnet)
training <- training %>% 
  relocate(Bad, Franchise, Star, .after = level)


predictions_fda <- predict(allen_fda, pros, type = "prob")
pros<- cbind(pros, predictions_fda)
# training <- training %>% 
#   rename(dt_pred_nnet_2 = predictions_nnet)
pros <- pros %>% 
  relocate(Bad, Franchise, Star, .after = level)

pros$Bad <- round(pros$Bad, 2)
pros$Franchise <- round(pros$Franchise, 2)
pros$Star <- round(pros$Star, 2)

pros_22 <- pros %>% 
  filter(season == 2022)

pros_22 <- pros_22[-1]


#GBM----------------------------------------------------
training <- training[,-c(5:7)]
allen_new <- train(
  level ~ .,
  data = training[,-c(1:3,5:8)],
  method = "knn",
  metric = "Accuracy",
  # tuneGrid = tgrid_2,
  trControl = trainControl(method = "cv", number = 10,
                           sampling = "smote",
                           classProbs = TRUE)
)
allen_new
confusionMatrix(allen_new)
plot(allen_new)

test_final <- testing[,-c(5:7, 34:39)]
knn_pred <- predict(allen_new, test_final, type = "prob")
test_final <- cbind(test_final, knn_pred )
test_final <- test_final %>% 
  relocate(Bad, Franchise, Star, .after = level)
test_final$Bad <- round(test_final$Bad, 2)
test_final$Franchise <- round(test_final$Franchise, 2)
test_final$Star <- round(test_final$Star, 2)

test_final$knn <- apply(test_final[5:7], 1, function(x) names(x)[which.max(x)])
test_final <- test_final %>% 
  relocate(knn, .after = level)
testing_new$knn <- test_final$knn
testing_new <- testing_new %>% 
  relocate(knn, .after = monmlp)


confusionMatrix(as.factor(testing_new$knn), as.factor(testing_new$level))
confusionMatrix(as.factor(testing_new$monmlp), as.factor(testing_new$level))
'%!in%' <- function(x,y)!('%in%'(x,y))

draft_22 <- pros_tbd %>% 
  filter(player %!in% c("Bo Nix", "Desmond Ridder", "Sam Howell",
                        "Jayden Daniels", "Drake Maye", "Caleb Williams",
                        "KJ Jefferson", "Quinn Ewers","Spencer Rattler", 
                        "Kenny Pickett") )
View(draft_22)

draft_22$good_rate <- draft_22$Franchise + draft_22$Star
draft_22 <- draft_22 %>% 
  relocate(good_rate, .after = Star)

write.csv(xfl_final, "big_qbs.csv")

pros_new <- pros[,-c(6:8)]
predictions_new <- predict(allen_new, pros_new, type = "prob")
pros_new <- cbind(pros_new, predictions_fda)
# training <- training %>% 
#   rename(dt_pred_nnet_2 = predictions_nnet)
pros_new <- pros_new %>% 
  relocate(Bad, Franchise, Star, .after = level)
pros_new$Bad <- round(pros_new$Bad, 2)
pros_new$Franchise <- round(pros_new$Franchise, 2)
pros_new$Star <- round(pros_new$Star, 2)


#SVM-----------------------------------------------------
tgrid_3 <- expand.grid(hidden1 = 1:10,
                       n.ensemble = 1:5)


allen_next <- train(
  level ~ .,
  data = training[,-c(1:3,5:8)],
  method = "monmlp",
  metric = "Accuracy",
  tuneGrid = tgrid_3,
  trControl = trainControl(method = "cv", number = 10,
                           sampling = "smote",
                           classProbs = TRUE)
)

allen_next2 <- caret::train(
  level ~ .,
  data = training[,-c(1:3,5:8)],
  method = "monmlp",
  metric = "Accuracy",
  tuneGrid = tgrid_3,
  trControl = trainControl(method = "cv", number = 10,
                           sampling = "smote",
                           classProbs = TRUE),
  
  ordered = TRUE
)




confusionMatrix(allen_next)

allen_next

pros_again <- pros[,-c(6:8)]
predictions_again <- predict(allen_new, pros_again, type = "prob")
pros_again <- cbind(pros_again, predictions_again)
# training <- training %>% 
#   rename(dt_pred_nnet_2 = predictions_nnet)
pros_again <- pros_again %>% 
  relocate(Bad, Franchise, Star, .after = level)
pros_again$Bad <- round(pros_again$Bad, 2)
pros_again$Franchise <- round(pros_again$Franchise, 2)
pros_again$Star <- round(pros_again$Star, 2)

pros_again <- pros[,-c(6:8)]
predictions_next <- predict(allen_next, pros_again, type = "prob")
predictions_next <- predictions_next %>% 
  rename("Bad_next" = "Bad", "Franchise_next" = "Franchise", "Star_next" = "Star")

pros_again <- cbind(pros_again, predictions_next)
# training <- training %>% 
#   rename(dt_pred_nnet_2 = predictions_nnet)
pros_again <- pros_again %>% 
  relocate(Bad_next, Franchise_next, Star_next, .after = level)
pros_again$Bad_next <- round(pros_again$Bad_next, 2)
pros_again$Franchise_next <- round(pros_again$Franchise_next, 2)
pros_again$Star_next <- round(pros_again$Star_next, 2)

testing_2 <- testing[,-c(5:7)]
next_tst <- predict(allen_next, testing_2)
confusionMatrix(testing_2, next_tst)

predi


testing_new <- testing_2[,-c(31:36)]
predictions_again <- predict(allen_next, testing_new, type = "prob")
testing_new <- cbind(testing_new, predictions_again)
# training <- training %>% 
#   rename(dt_pred_nnet_2 = predictions_nnet)
testing_new <- testing_new %>% 
  relocate(Bad, Franchise, Star, .after = level)
testing_new$Bad <- round(testing_new$Bad, 2)
testing_new$Franchise <- round(testing_new$Franchise, 2)
testing_new$Star <- round(testing_new$Star, 2)

testing_new$monmlp <- apply(testing_new[5:7], 1, function(x) names(x)[which.max(x)])
testing_new <- testing_new %>% 
  relocate(monmlp, .after = level)

plot(allen_next)
allen_next
predictions_next <- predict(allen_next, pros, type = "prob")
#predictions_next <- predictions_next %>% 
 # rename("Bad_next" = "Bad", "Franchise_next" = "Franchise", "Star_next" = "Star")

pros <- cbind(pros, predictions_next)
# training <- training %>% 
#   rename(dt_pred_nnet_2 = predictions_nnet)
pros <- pros %>% 
  relocate(Bad, Franchise, Star, .after = level)
pros$Bad <- round(pros$Bad, 2)
pros$Franchise <- round(pros$Franchise, 2)
pros$Star <- round(pros$Star, 2)

pros$monmlp <- apply(pros[6:8], 1, function(x) names(x)[which.max(x)])
pros <- pros %>% 
  relocate(monmlp, .after = level)

pros_tbd <- pros %>% 
  filter(level == "TBD" | level == "NCAA")

vip(allen_next)
require(clusterGeneration)
require(nnet)

gar.fun('level', allen_next, wts.only = T)

class(allen_next)
confusionMatrix(allen_next)
plot(allen_next$modelInfo)
library(iml)
x <- select(allen,-level)
y <- select(allen,level)
predictor <- Predictor$new(allen_next, data = x, y = y, type = "prob")
hp_size <- as.numeric(allen_next$bestTune) # Tuned Hyperparameter Neruon in hiddenlayer
metric = "ce" # allowed losses "ce",

imp <- FeatureImp$new(predictor, 
                      loss = metric,
                      compare = "ratio",
                      n.repetitions = 50,
                      features = variables
)
plot(imp)

imp_2 <- FeatureImp$new(predictor, 
                      loss = metric,
                      compare = "difference",
                      n.repetitions = 50,
                      features = variables
)
plot(imp_2)
library(monmlp)
library(dplyr)


gam.style(allen_next)
gam.style(matrix(70,20), weights = )

allen_next$finalModel

new_draft <- draft_22
newdata <- draft_22[order(-draft_22$good_rate),]
newdata$rank <- row_number(newdata)

newdata$athlete_id <- 1:nrow(newdata)

newdata$rank <- newdata$athlete_id
newdata <- newdata %>% 
  relocate(rank, .before = player)

newdata$monmlp <- ifelse(newdata$monmlp == "Bad", "Backup", newdata$monmlp)
newdata <- rename(newdata, replace = c("Bad" = "Ba"))

renam
sort

Feat
real_pros <- pros %>% 
  filter(level != "TBD" & level != "NCAA")

real_pros$level_check <- ifelse(real_pros$level == "Bad" | real_pros$level == "Backup", "Bad_next",
                      ifelse(real_pros$level == "Franchise", "Franchise_next", "Star_next"))
real_pros <- real_pros %>% 
  relocate(level_check, .after = level)


caret::confusionMatrix(as.factor(real_pros$monmlp), as.factor(real_pros$level_check))



library(lime)

model_type.nnet <- function(x, ...) {
  return("classification")
}

predict_model.nnet <- function(x, newdata, ...) {
  pred <- predict(x, newdata)
  return(as.data.frame(pred))
}

here <- newdata[,-c(1:5,7:28,30:33)]
# get a few observations to perform local interpretation on
local_obs <- here[1, ]


# apply LIME
explainer <- lime(here, allen_next)
explanation <- explain(local_obs, explainer, n_labels = 3, n_features = 5)
plot_features(explanation)

local_obs <- here[2, ]


# apply LIME
explainer <- lime(here, allen_next)
explanation <- explain(local_obs, explainer, n_labels = 3, n_features = 5)
plot_features(explanation)
explanation[1,1:5]

#stroud
local_obs <- here[8, ]


# apply LIME
explainer <- lime(here, allen_next)
explanation <- explain(local_obs, explainer, n_labels = 3, n_features = 5)
plot_features(explanation)
explanation[1,1:5]

#bennett
local_obs <- here[1:10, ]


# apply LIME
explainer <- lime(here, allen_next)
explanation <- explain(local_obs, explainer, n_labels = 3, n_features = 5)
plot_features(explanation)
explanation[1,1:5]

test_pros <- pros %>% 
  filter(athlete_id %in% testing_new$athlete_id)

caret::confusionMatrix(as.factor(test_pros$monmlp), as.factor(test_pros$level_check))
confusionMa
saveRDS(allen_next, file = "qb_finder.rds")

vip(allen_next)


plot_explanations(explanation)




library(AppliedPredictiveModeling)

transparentTheme(trans = .4)

featurePlot(x=allen[,c(9,10,14,15,21)], 
            y=as.factor(allen$level), 
            plot="pairs", 
            auto.key=list(columns=3))

featurePlot(x=allen[,c(14,15,21)], 
            y=as.factor(allen$level), 
            plot = "ellipse",
            ## Add a key at the top
            auto.key = list(columns = 3))

featurePlot(x = allen[,c(9,10,14,15,21)], 
            y = as.factor(allen$level), 
            plot = "box", 
            ## Pass in options to bwplot() 
            scales = list(y = list(relation="free"),
                          x = list(rot = 90)),  
            layout = c(5,1 ), 
            auto.key = list(columns = 2))


featurePlot(x = allen[,c(9,10,14,15,21)], 
            y = as.factor(allen$level), 
            plot="density", 
            scales=list(x=list(relation="free"), 
                        y=list(relation="free")), 
            auto.key=list(columns=3))

featurePlot(x = allen[,c(9,10,14,15,21)], 
            y = allen$av_game, 
            plot = "scatter", 
            layout = c(5, 1),
            type = c("p", "smooth"))

write.csv(allen, file = "model_df.csv")
write.csv(pros, file = "all_df.csv")


##logistic regression------------------------------------------------------------

allen_log <- caret::train(
  level ~ .,
  data = training[,-c(1:3,5:8)],
  method = "glmnet",
  family = "multinomial",
  trControl = trainControl(method = "repeatedcv", 
                           repeats = 5,
                           number = 10,
                           sampling = "smote",
                           classProbs = TRUE)
)
caret::confusionMatrix(allen_log)
allen_log
allen_next
plot(allen_log)



predictions_log <- predict(allen_log, testing_new, type = "prob")


predictions_log <- predictions_log %>% 
  dplyr::rename("Bad_log" = "Bad", "Franchise_log" = "Franchise", "Star_log" = "Star")


testing_new <- cbind(testing_new, predictions_log)
# training <- training %>% 
#   rename(dt_pred_nnet_2 = predictions_nnet)
testing_new <- testing_new %>% 
  relocate(Bad_log, Franchise_log, Star_log, .after = Star)
testing_new$Bad_log <- round(testing_new$Bad_log, 2)
testing_new$Franchise_log <- round(testing_new$Franchise_log, 2)
testing_new$Star_log <- round(testing_new$Star_log, 2)

testing_new$log <- apply(testing_new[10:12], 1, function(x) names(x)[which.max(x)])
testing_new <- testing_new %>% 
  relocate(log, .after = monmlp)

testing_new$log <- ifelse(testing_new$log == "Bad_log", "Bad",
                          ifelse(testing_new$log == "Franchise_log", "Franchise",
                                 testing_new$log))


summary(allen_log)
allen_log

caret::confusionMatrix(as.factor(testing_new$log), as.factor(testing_new$level))
caret::confusionMatrix(as.factor(testing_new$knn), as.factor(testing_new$level))
caret::confusionMatrix(as.factor(testing_new$monmlp), as.factor(testing_new$level))

#logistic on new players--------------------------------------------------------

predictions_log2 <- predict(allen_log, pros, type = "prob")
predictions_log2 <- predictions_log2 %>% 
 dplyr::rename("Bad_log" = "Bad", "Franchise_log" = "Franchise", "Star_log" = "Star")

pros <- cbind(pros, predictions_log2)
# training <- training %>% 
#   rename(dt_pred_nnet_2 = predictions_nnet)
pros <- pros %>% 
  relocate(Bad_log, Franchise_log, Star_log, .after = Star)
pros$Bad_log <- round(pros$Bad_log, 2)
pros$Franchise_log <- round(pros$Franchise_log, 2)
pros$Star_log <- round(pros$Star_log, 2)

pros$log <- apply(pros[10:12], 1, function(x) names(x)[which.max(x)])
pros <- pros %>% 
  relocate(log, .after = monmlp)


pros$log <- ifelse(pros$log == "Bad_log", "Bad",
                          ifelse(pros$log == "Franchise_log", "Franchise",
                                 ifelse(pros$log == "Star_log", "Star",pros$log)))

coef(allen_log$finalModel,allen_log$bestTune$lambda)


##Logistic regression with less variables --------------------------------------

allen_log2 <- caret::train(
  level ~ .,
  data = training[,c(4,9,14,15,17:19,22,23)],
  method = "glmnet",
  family = "multinomial",
  trControl = trainControl(method = "repeatedcv", 
                           repeats = 5,
                           number = 10,
                           sampling = "smote",
                           classProbs = TRUE)
)
allen_log2
plot(allen_log2)
caret::confusionMatrix(allen_log2)



predictions_log2 <- predict(allen_log2, testing_new, type = "prob")


predictions_log2 <- predictions_log2 %>% 
  dplyr::rename("Bad_log2" = "Bad", "Franchise_log2" = "Franchise", "Star_log2" = "Star")


testing_new <- cbind(testing_new, predictions_log2)
# training <- training %>% 
#   rename(dt_pred_nnet_2 = predictions_nnet)
testing_new <- testing_new %>% 
  relocate(Bad_log2, Franchise_log2, Star_log2, .after = Star_log)
testing_new$Bad_log2 <- round(testing_new$Bad_log2, 2)
testing_new$Franchise_log2 <- round(testing_new$Franchise_log2, 2)
testing_new$Star_log2 <- round(testing_new$Star_log2, 2)

testing_new$log2 <- apply(testing_new[14:16], 1, function(x) names(x)[which.max(x)])
testing_new <- testing_new %>% 
  relocate(log2, .after = log)

testing_new$log2 <- ifelse(testing_new$log2 == "Bad_log2", "Bad",
                          ifelse(testing_new$log2 == "Franchise_log2", "Franchise",
                                 ifelse(testing_new$log2 == "Star_log2", "Star", testing_new$log2)))

caret::confusionMatrix(as.factor(testing_new$log2), as.factor(testing_new$level))
caret::confusionMatrix(as.factor(testing_new$log), as.factor(testing_new$level))
caret::confusionMatrix(as.factor(testing_new$knn), as.factor(testing_new$level))
caret::confusionMatrix(as.factor(testing_new$monmlp), as.factor(testing_new$level))


##Multicollinearity---------------------------------------------------------------

cor_matrix <- training[,-c(1:8)] %>%
  correlate() %>%
  rearrange()
cor_matrix <- as.data.frame(cor_matrix)

matrixrows <- cor_matrix$term
rownames(cor_matrix) <- as.vector(matrixrows)
cor_matrix <- cor_matrix[,-1]

sumdf <- data.frame() #make empty data frame
for (i in 1:ncol(cor_matrix)) {
  sum(as.numeric(cor_matrix[,i] >.7), na.rm = TRUE) -> count #count pearson's r cor. coef. > 0.7 for each variable
  sumdf <- rbind(sumdf, count)
}
countdf <- cbind(matrixrows, sumdf) #data frame of variable names and corresponding count of high correlations
countdf <- countdf %>% arrange(desc(X8))
print(countdf)

# apply transformation to each cell
df <- apply(cor_matrix, c(1, 2), function(x) ifelse(x > 0.7, 1, 0))

# convert result back to dataframe
df <- as.data.frame(df)
#evidence of multicollinearily: pitcher pitch count and pitch count vs. batter
#drop pitch count vs. batter
cj <- cj %>%
  subset(select = -pitch_count_batter_vs_pitcher)



varImp(allen_log)
varImp(allen_log2)
varIm



##Extreme Gradient Boosting Tree Model------------------------------------------
grid_default <- expand.grid(
  nrounds = 100,
  max_depth = 6,
  eta = 0.3,
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)
tune_grid <- expand.grid(
  nrounds = seq(from = 200, to = 1000, by = 50),
  eta = c(0.025, 0.05, 0.1, 0.3),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

allen_xg <- caret::train(
  level ~ .,
  data = training[,-c(1:3,5:8)],
  method = "xgbTree",
  metric = "Accuracy",
  tuneGrid = tune_grid,
  trControl = trainControl(method = "repeatedcv", 
                           repeats = 5,
                           number = 10,
                           sampling = "smote",
                           classProbs = TRUE,
                           verboseIter = TRUE, # no training log
                           allowParallel = TRUE )
)

caret::confusionMatrix(allen_xg)
caret::confusionMatrix(allen_next)


predictions_xg <- predict(allen_xg, testing_new, type = "prob")

predictions_xg <- predictions_xg %>% 
  dplyr::rename("Bad_xg" = "Bad", "Franchise_xg" = "Franchise", "Star_xg" = "Star")


testing_new <- cbind(testing_new, predictions_xg)
# training <- training %>% 
#   rename(dt_pred_nnet_2 = predictions_nnet)
testing_new <- testing_new %>% 
  relocate(Bad_xg, Franchise_xg, Star_xg, .after = Star_log2)
testing_new$Bad_xg <- round(testing_new$Bad_xg, 2)
testing_new$Franchise_xg <- round(testing_new$Franchise_xg, 2)
testing_new$Star_xg <- round(testing_new$Star_xg, 2)

testing_new$xg <- apply(testing_new[18:20], 1, function(x) names(x)[which.max(x)])
testing_new <- testing_new %>% 
  relocate(xg, .after = monmlp)

testing_new$xg <- ifelse(testing_new$xg == "Bad_xg", "Bad",
                           ifelse(testing_new$xg == "Franchise_xg", "Franchise",
                                  ifelse(testing_new$xg == "Star_xg", "Star", testing_new$xg)))

caret::confusionMatrix(as.factor(testing_new$log2), as.factor(testing_new$level))
caret::confusionMatrix(as.factor(testing_new$log), as.factor(testing_new$level))
caret::confusionMatrix(as.factor(testing_new$knn), as.factor(testing_new$level))
caret::confusionMatrix(as.factor(testing_new$monmlp), as.factor(testing_new$level))
caret::confusionMatrix(as.factor(testing_new$xg), as.factor(testing_new$level))


#XG on new players--------------------------------------------------------------
predictions_xg <- predict(allen_xg, pros, type = "prob")
predictions_xg <- predictions_xg %>% 
  dplyr::rename("Bad_xg" = "Bad", "Franchise_xg" = "Franchise", "Star_xg" = "Star")

pros <- cbind(pros, predictions_xg)
# training <- training %>% 
#   rename(dt_pred_nnet_2 = predictions_nnet)
pros <- pros %>% 
  relocate(Bad_xg, Franchise_xg, Star_xg, .after = Star)
pros$Bad_xg <- round(pros$Bad_xg, 2)
pros$Franchise_xg <- round(pros$Franchise_xg, 2)
pros$Star_xg <- round(pros$Star_xg, 2)

pros$xg <- apply(pros[11:13], 1, function(x) names(x)[which.max(x)])
pros <- pros %>% 
  relocate(xg, .after = monmlp)


pros$xg <- ifelse(pros$xg == "Bad_xg", "Bad",
                   ifelse(pros$xg == "Franchise_xg", "Franchise",
                          ifelse(pros$xg == "Star_xg", "Star",pros$xg)))


pros_tbd <- pros %>% 
  filter(level == "TBD" | level == "NCAA")

pros$model_agg <- paste(pros$monmlp, pros$xg, pros$log, sep = "_")
pros <- pros %>% 
  dplyr::relocate(model_agg, .after = level)

plot(allen_xg)
allen_xg
allen_xg$bestTune

##More tuning of xgboost-------------------------------------------------------
tune_grid4 <- expand.grid(
  nrounds = seq(from = 50, to = 500, by = 50),
  eta = allen_xg$bestTune$eta,
  max_depth = allen_xg$bestTune$max_depth,
  gamma = c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0),
  colsample_bytree = c(0.4, 0.6, 0.8, 1.0),
  min_child_weight = c(1, 2, 3),
  subsample = c(0.5, 0.75, 1.0)
)

allen_xg2 <- caret::train(
  level ~ .,
  data = training[,-c(1:3,5:8)],
  method = "xgbTree",
  metric = "Accuracy",
  tuneGrid = tune_grid4,
  trControl = trainControl(method = "repeatedcv", 
                           repeats = 5,
                           number = 10,
                           sampling = "smote",
                           classProbs = TRUE,
                           verboseIter = TRUE, #training log
                            allowParallel = TRUE )
)

saveRDS(allen_xg2, file = "allen_xg2.rds")
allen_xg2
summary(allen_xg2)
plot(allen_xg2)
caret::confusionMatrix(allen_xg)
caret::confusionMatrix(allen_xg2)
caret::confusionMatrix(allen_log)



predictions_xg2 <- predict(allen_xg2, testing_new, type = "prob")

predictions_xg2 <- predictions_xg2 %>% 
  dplyr::rename("Bad_xg2" = "Bad", "Franchise_xg2" = "Franchise", "Star_xg2" = "Star")


testing_new <- cbind(testing_new, predictions_xg2)
# training <- training %>% 
#   rename(dt_pred_nnet_2 = predictions_nnet)
testing_new <- testing_new %>% 
  relocate(Bad_xg2, Franchise_xg2, Star_xg2, .after = Star_xg)
testing_new$Bad_xg2 <- round(testing_new$Bad_xg2, 2)
testing_new$Franchise_xg2 <- round(testing_new$Franchise_xg2, 2)
testing_new$Star_xg2 <- round(testing_new$Star_xg2, 2)

testing_new$xg2 <- apply(testing_new[23:25], 1, function(x) names(x)[which.max(x)])
testing_new <- testing_new %>% 
  relocate(xg2, .after = xg)

testing_new$xg2 <- ifelse(testing_new$xg2 == "Bad_xg2", "Bad",
                         ifelse(testing_new$xg2 == "Franchise_xg2", "Franchise",
                                ifelse(testing_new$xg2 == "Star_xg2", "Star", testing_new$xg2)))

caret::confusionMatrix(as.factor(testing_new$log2), as.factor(testing_new$level))
caret::confusionMatrix(as.factor(testing_new$log), as.factor(testing_new$level))
caret::confusionMatrix(as.factor(testing_new$knn), as.factor(testing_new$level))
caret::confusionMatrix(as.factor(testing_new$monmlp), as.factor(testing_new$level))
caret::confusionMatrix(as.factor(testing_new$xg), as.factor(testing_new$level))
caret::confusionMatrix(as.factor(testing_new$xg2), as.factor(testing_new$level))

#XG2 on new players--------------------------------------------------------------
predictions_xg2 <- predict(allen_xg2, pros, type = "prob")
predictions_xg2 <- predictions_xg2 %>% 
  dplyr::rename("Bad_xg2" = "Bad", "Franchise_xg2" = "Franchise", "Star_xg2" = "Star")

pros <- cbind(pros, predictions_xg2)
# training <- training %>% 
#   rename(dt_pred_nnet_2 = predictions_nnet)
pros <- pros %>% 
  relocate(Bad_xg2, Franchise_xg2, Star_xg2, .after = Star)
pros$Bad_xg2 <- round(pros$Bad_xg2, 2)
pros$Franchise_xg2 <- round(pros$Franchise_xg2, 2)
pros$Star_xg2 <- round(pros$Star_xg2, 2)

pros$xg2 <- apply(pros[13:15], 1, function(x) names(x)[which.max(x)])
pros <- pros %>% 
  relocate(xg2, .after = monmlp)


pros$xg2 <- ifelse(pros$xg2 == "Bad_xg2", "Bad",
                  ifelse(pros$xg2 == "Franchise_xg2", "Franchise",
                         ifelse(pros$xg2 == "Star_xg2", "Star",pros$xg2)))


pros_tbd <- pros %>% 
  filter(level == "TBD" | level == "NCAA")


pros_23 <- pros_tbd[,c(3,5,8,14:16)]

pros_23$franchise_rate <- pros_23$Franchise_xg2+pros_23$Star_xg2
pros_23 <- pros_23 %>% 
  relocate(franchise_rate, .after = xg2)

draft_23 <- pros_23 %>% 
  filter(player %!in% c("Bo Nix", "Desmond Ridder", "Sam Howell",
                        "Jayden Daniels", "Drake Maye", "Caleb Williams",
                        "KJ Jefferson", "Quinn Ewers","Spencer Rattler", 
                        "Kenny Pickett") )


pros$franchise_rate <- pros$Franchise_xg2+pros$Star_xg2

pros <- pros %>% 
  relocate(franchise_rate, .after = xg2)

library(xgboost)
library(DiagrammeR)
xgb.plot.tree(model = allen_xg2$finalModel, trees = 1)


x <- dplyr::select(allen,-level)
y <- dplyr::select(allen,level)
predictor <- Predictor$new(allen_xg2, data = x, y = y, type = "prob")
hp_size <- as.numeric(allen_xg2$bestTune) # Tuned Hyperparameter Neruon in hiddenlayer
metric = "ce" # allowed losses "ce",


imp <- FeatureImp$new(predictor, 
                      loss = metric,
                      compare = "ratio",
                      n.repetitions = 50,
                      features = variables
)
plot(imp)


here <- newdata[,-c(1:5,7:28,30:33)]
# get a few observations to perform local interpretation on
local_obs <- here[2, ]


# apply LIME
explainer <- lime(here, allen_xg2)
explanation <- explain(local_obs, explainer, n_labels = 3, n_features = 10)
plot_features(explanation)

local_obs <- here[2, ]

#adding age, height, weight data------------------------------------------------
comb <- read_excel("combine.xlsx") 
comb2 <- comb %>% 
  subset(select = c("Player", "Age", "Height", "Wt"))
comb2 <- comb2[complete.cases(comb2),]

kam <- left_join(allen, comb2, by = c("player" = "Player"))

kam_na <- kam[!complete.cases(kam),]
kam$Height_2 <- as.numeric(format(as.Date(kam$Height), "%m%d"))
kam$Height_3 <- (as.numeric(substr(kam$Height_2, 1, 1)) * 12) + as.numeric(substr(kam$Height_2, 2, 3))

kam <- kam %>% 
  subset(select = -c(Height, Height_2))
kam <- kam %>% 
  dplyr::rename(Height = Height_3)


#filling in missing values
kam[kam$player == "Garrett Gilbert", "Height_3"] <- 76
kam[kam$player == "Garrett Gilbert", "Wt"] <- 221
kam[kam$player == "Garrett Gilbert", "Age"] <- 22

miss <- function(player, height, weight, age) {
  kam[kam$player == player, "Height"] <<- height
  kam[kam$player == player, "Wt"] <<- weight
  kam[kam$player == player, "Age"] <<- age
}

miss("Taysom Hill", 74, 221, 26)
miss("Trevor Siemian", 75, 214, 23)
miss("Tyler Murphy", 74, 213, 22)
miss("Taylor Heinicke", 73, 214, 22)
miss("Jake Rudock", 75, 207, 23)
miss("Chad Kelly", 74, 224, 23)
miss("Tim Boyle", 76, 232, 23)
miss("P.J. Walker", 71, 216, 23)
miss("Nick Mullens", 73, 196, 22)
miss("Kyle Allen", 75, 211, 22)
miss("David Blough", 72, 205, 23)

miss("John Wolford", 73, 200, 23)
miss("Bryce Perkins", 75, 215, 23)
miss("Tommy Stevens", 77, 235, 23)
miss("Anthony Brown", 73, 200, 22)
miss("Tyler Huntley", 73, 205, 22)
miss("Kellen Mond", 74, 211, 21)

kam$BMI <- (kam$Wt * 703) / (kam$Height)^2

#Decision trees again----------------------------------------------
set.seed(17)
inTraining <- createDataPartition(kam$level, p = .75, list = FALSE)
training_kam <- kam[ inTraining,]
testing_kam  <- kam[-inTraining,]

# Modeling packages
library(rpart)       # direct engine for decision tree application
library(caret)       # meta engine for decision tree application

# Model interpretability packages
library(rpart.plot)  # for plotting decision trees
library(vip)         # for feature importance
library(pdp)

kam_dt <- caret::train(
  level ~ .,
  data = training_kam[,-c(1:3,5:8)],
  method = "rpart",
  trControl = trainControl(method = "cv", number = 10),
  tuneLength = 20
)

ggplot(kam_dt)

#XG on Kam-----------------------------------------------------------------------
allen_xg2$bestTune

tune_grid4 <- expand.grid(
  nrounds = seq(from = 50, to = 500, by = 25),
  eta = c(0.025, 0.05, 0.1, 0.3),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0),
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = .5
)

kam_xg <- caret::train(
  level ~ .,
  data = training_kam[,-c(1:3,5:8)],
  method = "xgbTree",
  metric = "Accuracy",
  tuneGrid = tune_grid4,
  trControl = trainControl(method = "repeatedcv", 
                           number = 10,
                           repeats = 5,
                           sampling = "smote",
                           classProbs = TRUE,
                           verboseIter = TRUE, #training log
                           allowParallel = TRUE )
)

saveRDS(kam_xg, file = "kam_xg.rds")
kam_xg$bestTune
caret::confusionMatrix(kam_xg)


predictions_xg2 <- predict(kam_xg, testing_kam, type = "prob")

#predictions_xg2 <- predictions_xg2 %>% 
 # dplyr::rename("Bad_xg2" = "Bad", "Franchise_xg2" = "Franchise", "Star_xg2" = "Star")


testing_kam <- cbind(testing_kam, predictions_xg2)
# training <- training %>% 
#   rename(dt_pred_nnet_2 = predictions_nnet)
testing_kam <- testing_kam %>% 
  relocate(Bad, Franchise, Star, .after = level)
testing_kam$Bad <- round(testing_kam$Bad, 2)
testing_kam$Franchise <- round(testing_kam$Franchise, 2)
testing_kam$Star<- round(testing_kam$Star, 2)

testing_kam$xg <- apply(testing_kam[5:7], 1, function(x) names(x)[which.max(x)])
testing_kam <- testing_kam %>% 
  relocate(xg, .after = level)

caret::confusionMatrix(as.factor(testing_kam$xg), as.factor(testing_kam$level))





predictions_xg2 <- predict(kam_xg, kam, type = "prob")

#predictions_xg2 <- predictions_xg2 %>% 
# dplyr::rename("Bad_xg2" = "Bad", "Franchise_xg2" = "Franchise", "Star_xg2" = "Star")

dotson <- kam
dotson <- cbind(dotson, predictions_xg2)
# training <- training %>% 
#   rename(dt_pred_nnet_2 = predictions_nnet)
dotson <- dotson %>% 
  relocate(Bad, Franchise, Star, .after = level)
dotson$Bad <- round(dotson$Bad, 2)
dotson$Franchise <- round(dotson$Franchise, 2)
dotson$Star<- round(dotson$Star, 2)

dotson$xg <- apply(dotson[5:7], 1, function(x) names(x)[which.max(x)])
dotson <- dotson %>% 
  relocate(xg, .after = level)

caret::confusionMatrix(as.factor(dotson$xg), as.factor(dotson$level))


sam$Height <- sam$Height.x
sam$Age <- sam$Height.x
sam$Weight <- sam$Height.x


x <- dplyr::select(kam,-level)
y <- dplyr::select(kam,level)
predictor <- Predictor$new(kam_xg, training_kam[,-c(1:3,5:8)], kam$level, type = "prob")
hp_size <- as.numeric(allen_next$bestTune) # Tuned Hyperparameter Neruon in hiddenlayer
metric = "ce" # allowed losses "ce",

imp <- FeatureImp$new(predictor, 
                      loss = metric,
                      compare = "ratio",
                      n.repetitions = 50
)

plot(imp)



##pros df adding vars------------------------------------------------------------

comb_22 <- read.csv("22_measure.csv")
comb_22 <- comb_22[c(1:14),c(1:4)]
sam <- left_join(pros, comb_22, by = c("player", "Age", "Height", "Wt"))
sam <- left_join(sam, comb_22, by = "player")

now <- pros_23[-c(1,3,16:19,22,23,25,29),]

sam <- sam %>% 
  filter(season != 2022 | player %in% now$player)


merged_df <- left_join(pros, comb_22, by = "player")



# merged_df <- left_join(pros, comb_22, by = "player") %>%
#   mutate(Age = ifelse(is.na(Age.x), Age.y, Age.x),
#          Height = ifelse(is.na(Height.x), Height.y, Height.x),
#          Wt = ifelse(is.na(Wt.x), Wt.y, Wt.x)) %>%
#   select(-c(Height.x, Height.y, Age.x, Age.y, Wt.x, Wt.y))


merge_2 <- merged_df %>% 
  filter(!is.na(Age))
merge_2 <- merge_2[,-c(1,24:27)]

merge_2$BMI <- (merge_2$Wt * 703) / (merge_2$Height)^2


predictions_xg_23 <- predict(kam_xg, merge_2, type = "prob")

#predictions_xg2 <- predictions_xg2 %>% 
# dplyr::rename("Bad_xg2" = "Bad", "Franchise_xg2" = "Franchise", "Star_xg2" = "Star")

predictions_xg_23 <- predictions_xg_23 %>%
  dplyr::rename("Bad_comb" = "Bad", "Franchise_comb" = "Franchise", "Star_comb" = "Star")
merge_2 <- cbind(merge_2, predictions_xg_23)

merge_2 <- merge_2 %>% 
  relocate(Bad_comb, Franchise_comb, Star_comb, .after = level)
merge_2$Bad_comb <- round(merge_2$Bad_comb, 2)
merge_2$Franchise_comb <- round(merge_2$Franchise_comb, 2)
merge_2$Star_comb<- round(merge_2$Star_comb, 2)

merge_2$xg_comb <- apply(merge_2[5:7], 1, function(x) names(x)[which.max(x)])
merge_2 <- merge_2 %>% 
  relocate(xg_comb, .after = level)


x <- dplyr::select(merge_2,-level)
y <- dplyr::select(merge_2,level)


predictor <- Predictor$new(kam_xg, data = x, y = y, type = "prob")
hp_size <- as.numeric(kam_xg$bestTune) # Tuned Hyperparameter Neruon in hiddenlayer
metric = "ce" # allowed losses "ce",


imp <- FeatureImp$new(predictor, 
                      loss = metric,
                      compare = "ratio",
                      n.repetitions = 50,
                      features = variables
)
plot(imp)



local_obs <- merge_2[11, ]


# apply LIME
explainer <- lime(merge_2, kam_xg)
explanation <- explain(local_obs, explainer, n_labels = 3, n_features = 5)
plot_features(explanation)



##predict av per game-----------------------------------------------------------

tune_grid5 <- expand.grid(
  nrounds = seq(from = 50, to = 500, by = 25),
  eta = c(0.025, 0.05, 0.1, 0.3),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 1,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = .5
)

kam_xg_av <- caret::train(
  av_game ~ .,
  data = training_kam[,-c(1:4,6:8)],
  method = "xgbTree",
  tuneGrid = tune_grid5,
  trControl = trainControl(method = "cv", 
                           number = 5,
                           verboseIter = TRUE, #training log
                           allowParallel = TRUE )
)



sqrt(kam_xg_av$cv.error[best])

pred1 <- predict(kam_xg_av, newdata = testing_kam)
rmse_xg <- sqrt(sum((exp(pred1) - testing_kam$av_game)^2)/length(testing_kam$av_game))

#old players
pred_kam <- predict(kam_xg_av, newdata = kam)

forrest <- cbind(kam, pred_kam)
# training <- training %>% 
#   rename(dt_pred_nnet_2 = predictions_nnet)
forrest  <- forrest  %>% 
  relocate(pred_kam, .after = level)


#draftees

pred_merge<- predict(kam_xg_av, newdata = merge_2)

jamin <- cbind(merge_2, pred_merge)
# training <- training %>% 
#   rename(dt_pred_nnet_2 = predictions_nnet)
jamin <- jamin  %>% 
  relocate(pred_merge, .after = level)

forrest %>% 
  ggplot(aes(x = av_game, y = pred_kam))+
  geom_point()+
  theme_minimal()

#both
big <- bind_rows(forrest, jamin)
pred_kam <- predict(kam_xg_av, newdata = big)

big <- cbind(big, pred_kam)
big <- big %>% 
  relocate(pred_kam, .after = av_game)

big$highlight <- 0
big <- big %>% 
  relocate(highlight, .after = player)

big$highlight <- ifelse(big$level %in% c("Star", "NCAA"), 1,0)

big %>% 
  ggplot(aes(x = ifelse(level == "NCAA", 0,av_game), y = pred_kam))+
  geom_point()+
  theme_minimal() +
  geom_label_repel(aes(label=ifelse(highlight == 1,
                                    player,'')),hjust=-.1,vjust=0, max.overlaps = 20)



#MLR-------------------------------------
mlr_1 <- lm(av_game ~ ., data = kam[,-c(1:4,6:8)])
summary(mlr_1)


mlr_2 <- lm(av_game ~ ., data = training_kam[,-c(1:4,6:8)])
pred1 <- predict(mlr_2, newdata = testing_kam)
rmse_mlr <- sqrt(sum((exp(pred1) - testing_kam$av_game)^2)/length(testing_kam$av_game))
c(RMSE = rmse, R2=summary(mlr_2)$r.squared)


##ANN---------------------------------------

kam_xg_nn <- caret::train(
  av_game ~ .,
  data = training_kam[,-c(1:4,6:8)],
  method = "neuralnet",
  trControl = trainControl(method = "cv", 
                           number = 5,
                           verboseIter = TRUE, #training log
                           allowParallel = TRUE )
)

pred1 <- predict(kam_xg_nn, newdata = testing_kam)
rmse_nn <- sqrt(sum((exp(pred1) - testing_kam$av_game)^2)/length(testing_kam$av_game))



big2 <- bind_rows(forrest, jamin)
pred_kam <- predict(kam_xg_nn, newdata = big2)

big2 <- cbind(big2, pred_kam)
big2 <- big2[-5]
big2 <- big2 %>% 
  relocate(pred_kam, .after = av_game)

big2$highlight <- 0
big2 <- big2 %>% 
  relocate(highlight, .after = player)

big2$highlight <- ifelse(big2$level %in% c("Star", "NCAA"), 1,0)

big2 %>% 
  ggplot(aes(x = ifelse(level == "NCAA", 0,av_game), y = pred_kam))+
  geom_point()+
  theme_minimal() +
  geom_label_repel(aes(label=ifelse(highlight == 1,
                                    player,'')),hjust=-.1,vjust=0, max.overlaps = 20)


##---------------------------------------

kam_xg_xvm <- caret::train(
  av_game ~ .,
  data = training_kam[,-c(1:4,6:8)],
  method = "svmPoly",
  trControl = trainControl(method = "cv", 
                           number = 5,
                           verboseIter = TRUE, #training log
                           allowParallel = TRUE )
)
kam_xg_xvm
pred1 <- predict(kam_xg_xvm, newdata = testing_kam)
rmse_nn <- sqrt(sum((exp(pred1) - testing_kam$av_game)^2)/length(testing_kam$av_game))



big3 <- bind_rows(forrest, jamin)
pred_kam <- predict(kam_xg_xvm, newdata = big3)

big3 <- cbind(big3, pred_kam)
big3 <- big3[-5]
big3 <- big3 %>% 
  relocate(pred_kam, .after = av_game)

big3$highlight <- 0
big3 <- big3 %>% 
  relocate(highlight, .after = player)

big3$highlight <- ifelse(big3$level %in% c("Star", "NCAA"), 1,0)

big3 %>% 
  ggplot(aes(x = ifelse(level == "NCAA", 0,av_game), 
             y = pred_kam, 
             color = level))+
  geom_point()+
  theme_minimal() +
  geom_label_repel(aes(label=ifelse(highlight == 1,
                                    player,'')),hjust=-.1,vjust=0, max.overlaps = 20)


#Boom or Bust----------------------------------------------------------------

training_kam$boom_bust <- ifelse(training_kam$level == "Star", "Boom", "Bust")
testing_kam$boom_bust <- ifelse(testing_kam$level == "Star", "Boom", "Bust")

testing_kam$boom_bust[testing_kam$player == "Mac Jones"] <- "Boom"

testing_kam$boom_bust[testing_kam$player == "Justin Fields"] <- "Boom"
training_kam$boom_bust[training_kam$player == "Brock Purdy"] <- "Boom"


kam$boom_bust <- ifelse(kam$level == "Star", "Boom", "Bust")

kam$boom_bust[kam$player == "Mac Jones"] <- "Boom"

kam$boom_bust[kam$player == "Justin Fields"] <- "Boom"
kam$boom_bust[kam$player == "Brock Purdy"] <- "Boom"

kam_xg_bb <- caret::train(
  boom_bust ~ .,
  data = training_kam[,-c(1:8)],
  method = "xgbTree",
  tuneGrid = tune_grid5,
  trControl = trainControl(method = "cv", 
                           number = 5,
                           verboseIter = TRUE,
                           classProbs = TRUE,
                           sampling = "smote",#training log
                           allowParallel = TRUE )
)


predictions_xg_bb <- predict(kam_xg_bb, kam, type = "prob")

#predictions_xg2 <- predictions_xg2 %>% 
# dplyr::rename("Bad_xg2" = "Bad", "Franchise_xg2" = "Franchise", "Star_xg2" = "Star")

chase <- kam
chase <- cbind(chase, predictions_xg_bb)
# training <- training %>% 
#   rename(dt_pred_nnet_2 = predictions_nnet)
chase <- chase %>% 
  relocate(Boom, Bust, .after = level)
chase$Boom <- round(chase$Boom, 2)
chase$Bust <- round(chase$Bust, 2)


chase$xg_bb <- apply(chase[5:6], 1, function(x) names(x)[which.max(x)])
chase <- chase %>% 
  relocate(boom_bust, xg_bb, .after = level)

caret::confusionMatrix(as.factor(chase$xg_bb), as.factor(chase$boom_bust))

#on testing----------------------------------------------------------
predictions_xg_bb_tst <- predict(kam_xg_bb, testing_kam, type = "prob")

predictions_xg_bb_tst  <- predictions_xg_bb_tst  %>%
  dplyr::rename("Boom_xg" = "Boom", "Bust_xg" = "Bust")


testing_kam <- cbind(testing_kam, predictions_xg_bb_tst)
# training <- training %>% 
#   rename(dt_pred_nnet_2 = predictions_nnet)
testing_kam <- testing_kam %>% 
  relocate(boom_bust, Boom_xg, Bust_xg, .after = level)
testing_kam$Boom_xg <- round(testing_kam$Boom_xg, 2)
testing_kam$Bust_xg <- round(testing_kam$Bust_xg, 2)


testing_kam$xg_bb <- apply(testing_kam[6:7], 1, function(x) names(x)[which.max(x)])
testing_kam <- testing_kam %>% 
  relocate(boom_bust, xg_bb, .after = level)

testing_kam$xg_bb <- ifelse(testing_kam$xg_bb == "Boom_xg", "Boom",
                          ifelse(testing_kam$xg_bb == "Bust_xg", "Bust",
                                 testing_kam$xg_bb))



caret::confusionMatrix(as.factor(testing_kam$xg_bb), as.factor(testing_kam$boom_bust))


#on all qbs--------------------------------------------------------------------
caleb <- pros[c(80,83,110,113:114),]
caleb$Age <- 0
caleb$Height <- 0
caleb$Wt <- 0
caleb$BMI <- 0

caleb$Age[caleb$player == "Caleb Williams"] <- 22
caleb$Height[caleb$player == "Caleb Williams"] <- 73
caleb$Wt[caleb$player == "Caleb Williams"] <- 220

caleb$Age[caleb$player == "Drake Maye"] <- 21
caleb$Height[caleb$player == "Drake Maye"] <- 76
caleb$Wt[caleb$player == "Drake Maye"] <- 220

caleb$Age[caleb$player == "Kenny Pickett"] <- 23
caleb$Height[caleb$player == "Kenny Pickett"] <- 75
caleb$Wt[caleb$player == "Kenny Pickett"] <- 217

caleb$Age[caleb$player == "Desmond Ridder"] <- 22
caleb$Height[caleb$player == "Desmond Ridder"] <- 75
caleb$Wt[caleb$player == "Desmond Ridder"] <- 211

caleb$Age[caleb$player == "Sam Howell"] <- 21
caleb$Height[caleb$player == "Sam Howell"] <- 73
caleb$Wt[caleb$player == "Sam Howell"] <- 218


caleb$BMI <- (caleb$Wt * 703) / (caleb$Height)^2


merge_2$boom_bust <- "TBD"
caleb$boom_bust <- "TBD"



big4 <- bind_rows(kam, merge_2, caleb)
pred_kam <- predict(kam_xg_bb, newdata = big4, type = "prob")

big4 <- cbind(big4, pred_kam)
#big4 <- big4[-5]
big4 <- big4 %>% 
  relocate(boom_bust, Boom, Bust, .after = level)

big4$highlight <- 0
big4 <- big4 %>% 
  relocate(highlight, .after = player)

big4$level[big4$player == "Malik Willis"] <- "TBD"

big4$highlight <- ifelse(big4$level %in% c("Star", "NCAA", "Franchise", "TBD"), 1,0)

big4 %>% 
  ggplot(aes(x = ifelse(level == "NCAA", 0,av_game), 
             y = Boom, 
             color = level))+
  geom_point()+
  theme_minimal() +
  geom_label_repel(aes(label=ifelse(highlight == 1 | Boom >0.5,
                                    player,'')),hjust=-.1,vjust=0, max.overlaps = 20) +
  xlab("Approximate Value per NFL Game Played") +
  ylab("Boom Liklihood") +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  scale_y_continuous(breaks=seq(0,1.125,.125), 
                     labels = scales::percent) +
  labs(title = "Draft Pick Boom Chance vs. NFL Value per Game")

library(ggtext)

big4 %>% 
  filter(level == "NCAA") %>% 
  ggplot(aes(x = reorder(player, -Boom), y = Boom, 
             fill = Boom)) +
  geom_bar(stat = "identity")+
  scale_fill_gradient(high = "blue", low = "purple4")+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1,),
        axis.title.y = element_text(angle = 0, vjust = 0.5, size = 13),
        axis.title.x = element_text(size = 13),
        plot.title = element_text(hjust = 0.5,size = 15,face = "bold")) +
  ylab("Boom\nLikelihood") +
  xlab("2023 Draftees and Caleb Williams and Drake May") +
  labs(title = "QB Prospect Boom Potential")+
  scale_y_continuous(breaks=seq(0,1.125,.125), 
                     labels = scales::percent)+
  guides(fill="none")




x <- dplyr::select(big4,-boom_bust)
y <- dplyr::select(big4,boom_bust)


predictor <- Predictor$new(kam_xg_bb, data = x, y = y, type = "prob")
hp_size <- as.numeric(kam_xg_bb$bestTune) # Tuned Hyperparameter Neruon in hiddenlayer
metric = "ce" # allowed losses "ce",
kam_xg_bb$terms$

imp <- FeatureImp$new(predictor, 
                      loss = metric,
                      compare = "ratio",
                      n.repetitions = 50
)
plot(imp)



View(kam_xg_bb)

importance <- varImp(kam_xg_bb)

importance$importance
varIm



##adding PFF-------------------------------------------------------------------
pff_2014 <- read.csv("passing_summary_2014.csv")
pff_2015 <- read.csv("passing_summary_2015.csv")
pff_2016 <- read.csv("passing_summary_2016.csv")
pff_2017 <- read.csv("passing_summary_2017.csv")
pff_2018 <- read.csv("passing_summary_2018.csv")
pff_2019 <- read.csv("passing_summary_2019.csv")
pff_2020 <- read.csv("passing_summary_2020.csv")
pff_2021 <- read.csv("passing_summary_2021.csv")
pff_2022 <- read.csv("passing_summary_2022.csv")




pff <- rbind(pff_2014,pff_2015,pff_2016,pff_2017,pff_2018,pff_2019,pff_2020,
             pff_2021,pff_2022)

pff_mean <- pff %>% 
  group_by(player_id) %>% 
  mutate(across(where(is.numeric), mean))



pff_sum <- pff %>%
  group_by(player_id) %>%
  summarise(career_games = sum(player_game_count, na.rm = TRUE),
            carrer_aimed_passes = sum(aimed_passes, na.rm = TRUE),
            carrer_attempts = sum(attemps, na.rm = TRUE),
            )

pff_sum <- pff %>%
  dplyr::select(-contains("rate"), -contains("average")) %>%  # exclude columns with "rate" or "average" in their names
  group_by(player_id) %>%
  dplyr::summarize(where(is.numeric), sum, na.rm = TRUE)


pff_sum <- pff %>%
  dplyr::select(-contains("rate"), -contains("average")) %>%  # exclude columns with "rate" or "average" in their names
  dplyr::group_by(player_id) %>%
  dplyr::summarize(dplyr::across(where(is.numeric), sum, na.rm = TRUE),  # calculate the sum of all columns
            dplyr::across(contains("rate") | contains("average"), mean, na.rm = TRUE)) 


pff_sum2 <- pff %>%
 # dplyr::select(-contains("rate"), -contains("avg"), -contains("percent")) %>%  # exclude columns with "rate" or "average" in their names
  dplyr::group_by(player_id, player) %>%
  dplyr::summarize(dplyr::across(where(is.numeric), sum, na.rm = TRUE),  # calculate the sum of all columns
                   dplyr::across(contains("rate") | contains("avg") | contains("percent"), mean, na.rm = TRUE)) 




pff_avg <- pff[,c(1,2,6,9,10,13,14,18,31,32,33,40,42)]
  
  
  
  
count(unique(pff_sum2$player_id))
n_distinct(pff_sum2$player_id)








