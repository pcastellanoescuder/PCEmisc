#' Impute and Normalize a Data Frame
#'
#' This function imputes missing values and normalizes a data frame.
#'
#' @param data A data frame.
#'
#' @return Clean dataset.
#'
#' @export
cleaning <- function(data,
                     zeros_as_na = FALSE,
                     remove_zeros = FALSE,
                     zero_cutoff = 0.3,
                     na_cutoff = 0.3,
                     knn = TRUE,
                     norm = TRUE,
                     first_col_ids = TRUE) {

  if(first_col_ids) {
    data <- data %>%
      dplyr::rename("id" = 1) %>%
      tibble::column_to_rownames("id")
  }

  ## Remove columns that only have NAs
  data <- data[, apply(data, 2, function(x) !all(is.na(x)))]

  ## Remove columns that only have zeros
  data <- data[, apply(data, 2, function(x) !all(x == 0, na.rm = TRUE))]

  ## Remove columns with var = 0
  data <- data[, !apply(data, 2, function(x){var(x, na.rm = TRUE)}) == 0]

  if (remove_zeros) {
    ## Remove columns with more than x% of zeros
    data <- data[, colSums(data == 0, na.rm = TRUE)/nrow(data) < zero_cutoff]
  }

  if (zeros_as_na) {
    data[data == 0] <- NA
  }

  ## Remove columns that with more than x% of NAs
  data <- data[, !apply(data, 2, function(x){sum(is.na(x))/nrow(data)}) > na_cutoff]

  if (knn) {
    ## KNN imputation
    data <- t(data)
    data <- impute::impute.knn(data)
    data <- t(data$data)
  }

  if (norm) {
    data <- apply(data, 2, function(x) (log(x + 1) - mean(log(x + 1), na.rm = TRUE))/sd(log(x + 1), na.rm = TRUE))
  }

  if(first_col_ids) {
    data <- data %>%
      as.data.frame() %>%
      rownames_to_column("id")
  }

  return(data)
}

