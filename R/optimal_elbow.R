optimal_elbow <- function(x) {

  colnames(x) <- c("name", "value")
  data <- x %>%
    arrange(desc(value)) %>%
    mutate(rank = 1:n())

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
