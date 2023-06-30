
#' Find Optimal Elbow Point in a Vector
#'
#' This function identifies the optimal elbow point in a data vector using the elbow method.
#'
#' @param data A numeric vector representing the data.
#'
#' @return The index of the optimal elbow point.
#'
#' @examples
#' data <- c(1, 2, 3, 4, 5, 4, 3, 2, 1)
#' optimal_elbow(data)
#'
#' @export
optimal_elbow <- function(data) {

  data <- data.frame(value = sort(data, decreasing = TRUE),
                     rank = 1:length(data))

  i1 <- which.min(data$rank)
  i2 <- which.max(data$rank)
  slope <- (data$value[i2] - data$value[i1]) / (data$rank[i2] - data$rank[i1])
  int <- data$value[i1] - slope*data$rank[i1]

  perpslope <- -1/slope
  perpint <- data$value - perpslope*data$rank

  xcross <- (int - perpint) / (perpslope - slope)
  ycross <- slope*xcross + int

  dists <- sqrt((data$rank - xcross)^2 + (data$value - ycross)^2)

  elbow <- which.max(dists) - 1

  return(elbow)
}

