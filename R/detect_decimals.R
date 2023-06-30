
#' Detect Columns with Decimals in a Data Frame
#'
#' This function detects columns in a data frame that contain decimal values.
#'
#' @param data A data frame.
#'
#' @return A logical vector indicating which columns contain decimal values.
#'
#' @examples
#' data <- data.frame(x = c(1, 2, 3), y = c(1.5, 2.7, 3.0), z = c(1, 2, 3))
#' detect_decimals(data)
#'
#' @export
detect_decimals <- function(data) {
  decimal_columns <- sapply(data, function(col) {
    any(col %% 1 != 0)
  })
  return(decimal_columns)
}

