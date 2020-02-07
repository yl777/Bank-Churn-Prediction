bankChurn <- read_csv('Churn_Modelling.csv')

bankChurn <- bankChurn %>% 
  dplyr::select(-RowNumber, -CustomerId, -Surname) %>% #remove unwanted column 
  mutate(Geography = as.factor(Geography),
         Gender = as.factor(Gender),
         HasCrCard = as.factor(HasCrCard),
         IsActiveMember = as.factor(IsActiveMember),
         Exited = as.factor(Exited),
         Tenure = as.factor(Tenure),
         NumOfProducts = as.factor(NumOfProducts))

bankChurn <- bankChurn %>% 
  dplyr::select(-Tenure, -HasCrCard)

set.seed(1234)
sample_set <- bankChurn %>%
  pull(.) %>% 
  sample.split(SplitRatio = .7)

bankTrain <- subset(bankChurn, sample_set == TRUE)
bankTest <- subset(bankChurn, sample_set == FALSE)
bankTrain <- SMOTE(Exited ~ ., data.frame(bankTrain), perc.over = 100, perc.under = 200)

ctrl <-
  trainControl(method = "cv",
               number = 10,
               selectionFunction = "best")

## Grid Search
grid <- expand.grid(
  nrounds = 40,
  max_depth = c(4,5,6,7,8),
  eta =  c(0.1,0.2,0.3,0.4,0.5),
  gamma = 0.01,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = c(0.5, 1)
)

## Build XGBoost
set.seed(1234)
modelCreation <-
  train(
    Exited ~ .,
    data = bankTrain,
    method = "xgbTree",
    metric = "Accuracy",
    trControl = ctrl,
    tuneGrid = grid
  )

save(modelCreation, file = "C:/Users/sberry5/Documents/teaching/courses/shinyDemo/stoneTest/modelCreation.RData")
