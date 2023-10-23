#' Order and Threshold Data
#'
#' This function orders a dataset based on a priority order, calculates the cumulative area, and filters the data
#' to meet a specified threshold.
#'
#' @param data A data frame containing the input dataset.
#' @param threshold A numeric value representing the threshold for cumulative area.
#' @param priority_order A character vector specifying the priority order for processing data.
#'
#' @return A data frame containing the filtered data based on the threshold.
#'
#' @details The function iterates through the specified priority order, processes each priority in sequence, and
#' stops once the cumulative area exceeds the threshold. It ensures that IDs processed in earlier priorities are
#' not considered in subsequent priorities.
#'
#' @examples
#' # Load your dataset (replace 'your_data' with the actual variable name of your dataset)
#' your_data <- read.csv("your_dataset.csv")
#'
#' # Set the threshold for cumulative area (e.g., 5 square kilometers)
#' threshold <- 5
#'
#' # Define your priority order (customize it as needed)
#' custom_priority_order <- c("DIQ", "DOQ", "RVB", "RVE", "LAV", "BUF")
#'
#' # Call the function with the specified priority order
#' filtered_dataset <- orderAndThresholdData(your_data, threshold, custom_priority_order)
#'
#'
#'
#' @export


orderAndThresholdData <- function(data, threshold, priority_order) {
  Selected_IDs <- numeric()
  filtered_data <- data.frame()

  for (i in 1:length(priority_order)) {
    # Filter the data for the current priority
    current_data <- data[!is.na(data[[paste0(priority_order[i], "_ID")]]) & !is.nan(data[[paste0(priority_order[i], "_Area")]]) &- !(data$ID %in% Selected_IDs), ]
    current_data <- current_data[order(current_data[[paste0(priority_order[i], "_Area")]], decreasing = T), ]

    # Calculate cumulative area for the current priority
    current_data$CumArea <- cumsum(current_data$Area)

    if(max(current_data$CumArea) > threshold){
      message(paste("ready with", priority_order[i]))
    } else if(max(current_data$CumArea) < threshold){
      message(paste("not ready with", priority_order[i], round(threshold- max(current_data$CumArea), 2), "left"))
    }

    # Find the row where CumArea first exceeds the threshold for the current priority

    if (max(current_data$CumArea) > threshold) {
      # If there's a row above the threshold, take one row before it
      first_row_above_threshold <- min(which(current_data$CumArea > threshold))
      filtered_data <- rbind(filtered_data, current_data[1:(first_row_above_threshold), ])
      break  # Stop processing further priorities
    } else {
      # If the threshold is not exceeded for the current priority, add all rows
      filtered_data <- rbind(filtered_data, current_data)
      Selected_IDs <- filtered_data$ID
    }
    threshold <- threshold- max(current_data$CumArea)
  }
  filtered_data$CumArea <- cumsum(filtered_data$Area)
  return(filtered_data)
}
