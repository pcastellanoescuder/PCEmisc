#' Normality in a Data Frame
#'
#' This function computes the normality of all variables in a data frame.
#'
#' @param data A data frame.
#'
#' @return Percentage of normailty.
#'
#' @export
normality_percentage <- function(data) {
  # Check if the input is a data frame
  if (!is.data.frame(data)) {
    data <- as.data.frame(data)
  }

  # Initialize the counter for normally distributed variables
  normal_vars <- 0

  # Loop through each column in the data frame
  for (i in 1:ncol(data)) {
    # Perform the Shapiro-Wilk test for normality
    shapiro_test <- shapiro.test(data[,i])
    # Check if the p-value of the test is greater than the significance level (e.g., 0.05)
    if (shapiro_test$p.value >= 0.05) {
      normal_vars <- normal_vars + 1
    }
  }

  # Calculate the percentage of normally distributed variables
  normal_percentage <- (normal_vars / ncol(data)) * 100

  return(normal_percentage)
}

