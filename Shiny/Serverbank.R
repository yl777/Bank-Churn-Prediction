library(shiny)
library(tidyverse)
library(caret)
library(caTools)
library(DMwR)
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

ui <- fluidPage( titlePanel("Customer Churn Prediction"),
                sidebarLayout(
                  sidebarPanel(radioButtons(inputId = "Gender", label = "Gender",
                                            choices = list("Female" = "Female", "Male" = "Male"), 
                                            selected = "Female"),
                               radioButtons(inputId = "IsActiveMember", label = "Is Active Member",
                                            choices = list("0" = "0", "1" = "1"), 
                                            selected = "0"),
                               selectInput(inputId = "Age",
                                           label = "Age",
                                           choices = c(18:100)),
                               selectInput(inputId = "Geography",
                                           label = "Geography",
                                           choices = c("France","Germany","Spain")),
                               selectInput(inputId = "NumOfProducts",
                                           label = "Num Of Products",
                                           choices = c("1","2","3","4")),
                               numericInput(inputId = "Balance",
                                            label = "Balance",
                                            value = 97000),
                               numericInput(inputId = "EstimatedSalary",
                                            label = "Estimated Salary",
                                            value = 100000),
                               sliderInput("CreditScore", "Credit Score",
                                           min = 300, max = 900, value = 350),
                               actionButton("submit", ("Submit"))),
                  mainPanel(p("This App shows the customer's churn prediction for a regional bank using Random Forest algorithm. Please input an appropriate value to the left side bar, thank you for using :)"),
                            verbatimTextOutput("summary"), 
                            verbatimTextOutput("recommendation"))))

server <- function(input, output) {

  modelCreation <- modelCreation
  
  newData <- reactive({
    df <- data.frame(Gender = input$Gender, 
                     Age = as.numeric(as.character(input$Age)), 
                     Geography = input$Geography,
                     IsActiveMember = input$IsActiveMember,
                     NumOfProducts = input$NumOfProducts,
                     Balance = input$Balance,
                     EstimatedSalary = input$EstimatedSalary,
                     CreditScore = input$CreditScore)
  })
  
  out <- eventReactive(input$submit, {
    predictedProbability <- predict(modelCreation, newdata = newData(), type = 'prob')[,2]
    round(predictedProbability, 3)
  })
  
  output$summary <- renderText({
    paste("The probability of being churned is ", out(), ".", sep = "")
  })
  
  output$recommendation <- renderText({
    if(out() > .5) print("The customer is very likely to churn! Be careful")
    else print("The customer is not very likely to churn! You're fine now!")
  })
}

shiny::shinyApp(ui, server)