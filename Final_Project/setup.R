#Relevant libraries
library(ggplot2)
library(plotly)
library(grid)
library(gridExtra)

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
