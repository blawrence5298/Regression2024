#Relevant libraries
library(ggplot2)
library(plotly)
library(grid)
library(gridExtra)
library(MASS)
library(pROC)
library(car)
library(caret)
library(knitr)
library(kableExtra)
library(tidyverse)

#Load data
math_perf <- read.csv("student-mat.csv", sep=";")
port_perf <- read.csv("student-por.csv", sep=";")

#Custom functions to avoid redundant coding
#Creates basic scatter plots
gg_basic <- function(data, x, y, title="") {
  plot <- ggplot(data = data, aes(x = {{x}}, y = {{y}})) + 
          geom_point() +
          labs(title=title) +
          theme(plot.title = element_text(hjust = 0.5))
  return(plot)
}

gg_basic_count <- function(data, x, y, title="") {
  plot <- ggplot(data = data, aes(x = {{x}}, y = {{y}})) + 
          geom_count() +
          labs(title=title) +
          theme(plot.title = element_text(hjust = 0.5))
  return(plot)
}


print_summary <- function(model, p_value = 0.05, predictors = NULL) {
  # Extract the summary of the model
  summary_model <- summary(model)

  # Filter significant coefficients
  significant_coefs <- summary_model$coefficients[summary_model$coefficients[, "Pr(>|t|)"] < p_value, ]

  # Print significant coefficients
  cat("Significant Coefficients:\n")
  print(significant_coefs)

  # Print R-squared and Adjusted R-squared
  cat("\nR-squared: ", summary_model$r.squared, "\n")
  cat("Adjusted R-squared: ", summary_model$adj.r.squared, "\n")

  # Print F-statistic
  f_value <- summary_model$fstatistic[1]
  f_df1 <- summary_model$fstatistic[2]
  f_df2 <- summary_model$fstatistic[3]
  f_pvalue <- pf(f_value, f_df1, f_df2, lower.tail = FALSE)
  cat("F-statistic: ", f_value, " on ", f_df1, " and ", f_df2, " DF, p-value: ", f_pvalue, "\n")

  # If predictors are provided, calculate and print the VIF for each predictor
  if (!is.null(predictors) && length(predictors) > 0) {
      vif_values <- vif(model)
      cat("\nGVIF for provided predictors:\n")
      # Loop through each predictor and print its VIF value
      for (predictor in predictors) {
          cat(predictor, ": ", vif_values[predictor, 1], "\n")
      }
  }
}


#Creates a 2x2 grid of basic diagnostic plots of a lm object
lm_diag <- function(lm_model, title = "", limResid = NULL, limSresid = NULL, limLev = NULL, limQQ = NULL) {
  # Helper function to apply y-axis limits if provided
  apply_limits <- function(plot, ylim) {
    if (!is.null(ylim)) {
      plot + coord_cartesian(ylim = ylim)
    } else {
      plot
    }
  }
  
  # Generate diagnostic plots
  p1 <- gg_basic(lm_model$model, fitted(lm_model), residuals(lm_model)) + 
    labs(x="Fitted Values", y="Residuals")
  p1 <- apply_limits(p1, limResid)
  
  p2 <- gg_basic(lm_model$model, fitted(lm_model), rstandard(lm_model)) +
    labs(x="Fitted Values", y="Standardized Residuals")
  p2 <- apply_limits(p2, limSresid)
  
  p3 <- gg_basic(lm_model$model, fitted(lm_model), hatvalues(lm_model)) +
    labs(x="Fitted Values", y="Leverage")
  p3 <- apply_limits(p3, limLev)
  
  p4 <- ggplot(lm_model$model, aes(sample = rstandard(lm_model))) +
        stat_qq() +
        stat_qq_line(colour="red") + 
        labs(x ="Theoretical Quantiles", y="Sample Quantiles")
  p4 <- apply_limits(p4, limQQ)
  
  # Arrange all plots into a grid and return the combined plot
  return(grid.arrange(p1, p2, p3, p4, ncol=2, nrow=2, top = title))
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





