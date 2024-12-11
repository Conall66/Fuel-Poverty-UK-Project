
# Predictive Analytics
# Take sub regional data, build linear and non-linear regression models
# Divide existing data into training, test and validation sets
# Determine bias, mean absolute error and RMSE


# Libraries ---------------------------------------------------------------

library(ggplot2)
library(dplyr)

# Test Inputs -------------------------------------------------------------

Area_code <- "E06000008"
hop <- "households"
Years <- 2012:2021
state <- "Winter Mortality"

# Extract Datasets --------------------------------------------------------

## FP Data

# Prediction function with confidence intervals
predict_with_ci <- function(new_years, model, data_frame, confidence = 0.1) {
  
  se_residual <- sigma(model)

  # Predict values
  predicted_values <- predict(model, newdata = data.frame(Year = new_years), 
                              interval = "confidence", 
                              level = confidence)
  
  # Calculate standard error of prediction
  X <- model.matrix(model)
  H <- X %*% solve(t(X) %*% X) %*% t(X)
  leverage <- diag(H)
  
  # Prediction standard error
  new_X <- cbind(1, new_years)
  pred_se <- sqrt(se_residual^2 * (1 + diag(new_X %*% solve(t(X) %*% X) %*% t(new_X))))
  
  # Calculate confidence intervals
  t_value <- qt((1 - confidence) / 2, df = model$df.residual)
  
  col_name <- colnames(data_frame)[2]
  
  result_ft <- data.frame(
    Year = new_years,
    col_name = predicted_values[, 1],
    Lower_CI = predicted_values[, 1] + t_value * pred_se,
    Upper_CI = predicted_values[, 1] - t_value * pred_se
  )
  
  names(result_ft)[2] <- col_name
  
  return(result_ft)
}

Predict <- function(Area_code, hop, Years, state){
  
  Year_vect <- c(min(Years):max(Years))
  
  files <- list.files('Final Data Cleaned/', full.names = TRUE)
  FP_vals <- numeric(length(files))
  
  for(item in 1:length(files)){
    FP_data <- read.csv(files[item], header = TRUE)
    FP_val <- FP_data %>%
      filter(Area_code == Area.Codes)
    if(hop == "households"){
      FP_vals[item] <- FP_val$households
    } else if(hop == "proportion"){
      FP_vals[item] <- FP_val$proportion
    } else {
      print("Enter valid input")
      break
    }
  }
  
  ## WMI Data
  
  WMI_data <- read.csv('WMI_RelevantYears.csv')
  
  WMI_area <- WMI_data %>%
    filter(Area_code == Area.code)
  WMI_area <- WMI_area[3:ncol(WMI_area)]
  
  WMI_vals <- as.numeric(WMI_area[, seq(1, ncol(WMI_area), by = 3)])
  WMI_ub <- as.numeric(WMI_area[, seq(2, ncol(WMI_area), by = 3)])
  WMI_lb <- as.numeric(WMI_area[, seq(3, ncol(WMI_area), by = 3)])
  
  WMI_vals <- c(WMI_vals, 0)
  WMI_ub <- c(WMI_ub, 0)
  WMI_lb <- c(WMI_lb, 0)
  
  ## Data frame
  
  FP_WMI_dataframe <- data.frame(
    Year = Year_vect,
    FP_vals = FP_vals,
    WMI_vals = WMI_vals
    # WMI_ub = WMI_ub,
    # WMI_lb = WMI_lb
  )
  
  if(state == "Winter Mortality"){
    
    FP_WMI_dataframe <- FP_WMI_dataframe[, -2] #Remove fuel poverty data
    
    FP_WMI_dataframe <- FP_WMI_dataframe[-nrow(FP_WMI_dataframe), ] #Remove added 0 data point
    
    model <- lm(WMI_vals ~ Year, FP_WMI_dataframe)
    
    model_summary <- summary(model)
    se_residual <- model_summary$sigma  # Residual standard error
    
    # Generate future predictions
    future_years <- seq(max(Year_vect), 2025) # Hard code end year
    future_predictions <- predict_with_ci(future_years, model, FP_WMI_dataframe)
    
    # Combine historical and future data
    plot_data <- rbind(
      transform(FP_WMI_dataframe, Lower_CI = NA, Upper_CI = NA, Type = "Historical"),
      transform(future_predictions, Type = "Predicted")
    )
    
    # Create plot
    p <- ggplot(plot_data, aes(x = Year)) +
      # Historical data points
      geom_point(data = subset(plot_data, Type == "Historical"), 
                 aes(y = WMI_vals), color = "blue") +
      # Predicted data points
      geom_point(data = subset(plot_data, Type == "Predicted"), 
                 aes(y = WMI_vals), color = "red", shape = 2) +
      # Confidence interval ribbon
      geom_ribbon(data = subset(plot_data, Type == "Predicted"),
                  aes(ymin = Lower_CI, ymax = Upper_CI), 
                  fill = "red", alpha = 0.2) +
      # Regression line for historical data
      geom_smooth(data = subset(plot_data, Type == "Historical"), 
                  aes(y = WMI_vals), 
                  method = "lm", 
                  color = "blue", 
                  se = FALSE) +
      # Prediction line
      geom_line(data = subset(plot_data, Type == "Predicted"),
                aes(y = WMI_vals), 
                color = "red", 
                linetype = "dashed") +
      labs(
        title = paste("Winter Mortality Index Prediction for", Area_code),
        x = "Year",
        y = "Winter Mortality Index"
      ) +
      theme_minimal()
    
    # Print the plot
    print(p)
    
    # Return model summary and predictions
    return(list(
      model_summary = model_summary,
      future_predictions = future_predictions,
      plot = p
    ))
    
  } else if(state == "Fuel Poverty"){
    
    FP_WMI_dataframe <- FP_WMI_dataframe[, -3] #Remove winter mortality data
    
    # FP_WMI_dataframe <- FP_WMI_dataframe[-nrow(FP_WMI_dataframe), ]
    
    model <- lm(FP_vals ~ Year, FP_WMI_dataframe)
    
    model_summary <- summary(model)
    se_residual <- model_summary$sigma  # Residual standard error
    
    # Generate future predictions
    future_years <- seq(max(Year_vect) + 1, 2025) # Hard code end year
    future_predictions <- predict_with_ci(future_years, model, FP_WMI_dataframe)
    
    # Combine historical and future data
    plot_data <- rbind(
      transform(FP_WMI_dataframe, Lower_CI = NA, Upper_CI = NA, Type = "Historical"),
      transform(future_predictions, Type = "Predicted")
    )
    
    # Create plot
    p <- ggplot(plot_data, aes(x = Year)) +
      # Historical data points
      geom_point(data = subset(plot_data, Type == "Historical"), 
                 aes(y = FP_vals), color = "blue") +
      # Predicted data points
      geom_point(data = subset(plot_data, Type == "Predicted"), 
                 aes(y = FP_vals), color = "red", shape = 2) +
      # Confidence interval ribbon
      geom_ribbon(data = subset(plot_data, Type == "Predicted"),
                  aes(ymin = Lower_CI, ymax = Upper_CI), 
                  fill = "red", alpha = 0.2) +
      # Regression line for historical data
      geom_smooth(data = subset(plot_data, Type == "Historical"), 
                  aes(y = FP_vals), 
                  method = "lm", 
                  color = "blue", 
                  se = FALSE) +
      # Prediction line
      geom_line(data = subset(plot_data, Type == "Predicted"),
                aes(y = FP_vals), 
                color = "red", 
                linetype = "dashed") +
      labs(
        title = paste("Fuel Poverty Prediction for", Area_code),
        x = "Year",
        y = paste("Fuel Poverty by ", hop)
      ) +
      theme_minimal()
    
    # Print the plot
    print(p)
    
    # Return model summary and predictions
    return(list(
      model_summary = model_summary,
      future_predictions = future_predictions,
      plot = p
    ))
    
  }
  
}

test_results <- Predict("E07000179", "proportion", 2012:2021, "Winter Mortality")