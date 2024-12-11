
# Predictive Analytics
# Take sub regional data, build linear and non-linear regression models
# Divide existing data into training, test and validation sets
# Determine bias, mean absolute error and RMSE


# Libraries ---------------------------------------------------------------

library(ggplot2)

# Test Inputs -------------------------------------------------------------

Area_code <- "E06000008"
hop <- "households"
Years <- 2012:2021
state <- "Winter Mortality"

# Extract Datasets --------------------------------------------------------

## FP Data

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
    WMI_vals = WMI_vals,
    WMI_ub = WMI_ub,
    WMI_lb = WMI_lb
  )
  
  if(state == "Winter Mortality"){
    
    FP_WMI_dataframe <- FP_WMI_dataframe[, -2] # Remove fuel poverty data
    
    FP_WMI_dataframe <- FP_WMI_dataframe[-nrow(FP_WMI_dataframe), ]
    
    model <- lm(WMI_vals ~ Year, data = FP_WMI_dataframe) # Remove added 0 data point
    
    # Predict future values with confidence interval
    future_years <- seq((max(Year_vect)-1), 2025) # Hard coded 2025 as slider fixed end point
    future_predictions <- predict(model, newdata = data.frame(Year = future_years), 
                                  interval = "prediction")
    
    # Combine historical and future data
    plot_data <- rbind(
      cbind(FP_WMI_dataframe, Type = "Historical"),
      cbind(
        data.frame(
          Year = future_years,
          WMI_vals = future_predictions[, 1],
          WMI_ub = future_predictions[, 3],
          WMI_lb = future_predictions[, 2]
        ), 
        Type = "Predicted"
      )
    )
    
    p <- ggplot(plot_data, aes(x = Year)) +
      # Historical data points
      geom_point(data = subset(plot_data, Type == "Historical"), 
                 aes(y = WMI_vals), color = "#689E73") +
      # Predicted data points
      geom_point(data = subset(plot_data, Type == "Predicted"), 
                 aes(y = WMI_vals), color = "#6277A5", shape = 2) +
      # Uncertainty bounds
      geom_ribbon(data = plot_data,
                  aes(ymin = WMI_lb, ymax = WMI_ub, fill = Type),
                  alpha = 0.2) +
      # Regression line for historical data
      geom_smooth(data = subset(plot_data, Type == "Historical"), 
                  aes(y = WMI_vals), 
                  method = "lm", 
                  color = "#689E73", 
                  se = FALSE) +
      # Extrapolation line
      geom_smooth(data = subset(plot_data, Type == "Predicted"), 
                  aes(y = WMI_vals), 
                  method = "lm", 
                  color = "#6277A5", 
                  linetype = "dashed", 
                  se = FALSE) +
      labs(
        title = paste("Winter Mortality Index Prediction"),
        x = "Year",
        y = "Winter Mortality Index"
      ) +
      theme_minimal() +
      scale_fill_manual(values = c("Historical" = "#689E73", "Predicted" = "#6277A5"))
    
    # Print the plot
    print(p)
    
    # Return model summary and future predictions
    return(list(
      model_summary = summary(model),
      future_predictions = data.frame(
        Year = future_years,
        Predicted_Value = future_predictions[, 1],
        Lower_Bound = future_predictions[, 2],
        Upper_Bound = future_predictions[, 3]
      )
    ))
  }

}