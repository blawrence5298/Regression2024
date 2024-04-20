#Relevant libraries
library(ggplot2)
library(plotly)
library(grid)
library(gridExtra)
library(MASS)
library(pROC)
library(car)
library(caret)

#Custom functions to avoid redundant coding
#Creates basic scatter plots
gg_basic <- function(data, x, y) {
  plot <- ggplot(data = data, aes(x = {{x}}, y = {{y}})) + 
          geom_point()
  return(plot)
}

gg_basic_count <- function(data, x, y) {
  plot <- ggplot(data = data, aes(x = {{x}}, y = {{y}})) + 
          geom_count()
  return(plot)
}

#Creates a 2x2 grid of basic diagnostic plots of a lm object
lm_diag <- function(lm_model) {
  p1 <- gg_basic(lm_model$model, fitted(lm_model), residuals(lm_model)) + 
    labs(x="Fitted Values", y="Residuals")
  p2 <- gg_basic(lm_model$model, fitted(lm_model), rstandard(lm_model)) +
    labs(x="Fitted Values", y="Standardized Residuals")
  p3 <- gg_basic(lm_model$model, fitted(lm_model), hatvalues(lm_model)) +
    labs(x="Fitted Values", y="Leverage")
  
  # Create QQ plot of standardized residuals
  p4 <- ggplot(lm_model$model, aes(sample = rstandard(lm_model))) +
        stat_qq() +
        stat_qq_line(colour="red") + 
        labs(x ="Theoretical Quantiles", y="Sample Quantiles") +
        ggtitle("QQ Plot of Standardized Residuals")

  # Arrange all plots into a grid
  return(grid.arrange(p1, p2, p3, p4, ncol=2, nrow=2))
}


#Creates a correlation matrix for predictors in a lm model
cor_matrix <- function(lm_model) {
  model_data <- lm_model$model
  
  #Get names of predictors from model
  predictors <- all.vars(update(formula(lm_model), . ~ . -1))

  #Filter predictor columns  
  pred_data <- model_data[predictors]

  #Attempt to convert factors to numberic if not already
  for(col in colnames(pred_data)) {
    if(is.factor(pred_data[[col]])) {
        pred_data[[col]] <- as.numeric(as.factor(pred_data[[col]]))
    }
  }  
  
  cor_matrix <- cor(pred_data, use = "complete.obs")  # using complete cases
  return(cor_matrix)
}


glm_diag <- function(model, seed = 123) {
  
  # Retrieve the data used in the model
  data <- model.frame(model)
  outcome_var <- all.vars(formula(model))[1]  # Extracting the outcome variable from the formula

  # Ensure that the outcome_var is correctly used as a variable name in data
  if (!outcome_var %in% names(data)) {
    stop("Outcome variable not found in the model's data frame.")
  }
  
  # Set seed for reproducibility of the data split
  set.seed(seed)

  # Create data partitions
  split <- createDataPartition(y = data[[outcome_var]], p = 0.7, list = FALSE)
  training_set <- data[split, ]
  test_set <- data[-split, ]

  # Summarize the model (printing for immediate check, but can be stored or returned if needed)
  print(summary(model))

  # Predict and Evaluate on Test Set
  predicted_probs_test <- predict(model, newdata=test_set, type = "response")
  predicted_classes_test <- ifelse(predicted_probs_test > 0.5, 1, 0)

  # Confusion Matrix
  actual_classes_test <- factor(test_set[[outcome_var]], levels = c(0, 1))
  predicted_classes_test <- factor(predicted_classes_test, levels = c(0, 1))
  conf_mat_test <- confusionMatrix(predicted_classes_test, actual_classes_test)
  print(conf_mat_test)

  # ROC and AUC
  roc_curve_test <- roc(response = actual_classes_test, predictor = predicted_probs_test)
  plot(roc_curve_test, main="ROC Curve for Test Data", col="#1c61b6")
  roc_auc_test <- auc(roc_curve_test)
  print(paste("AUC for Test Data:", roc_auc_test))
}





