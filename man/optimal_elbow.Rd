% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/optimal_elbow.R
\name{optimal_elbow}
\alias{optimal_elbow}
\title{Find Optimal Elbow Point in a Vector}
\usage{
optimal_elbow(data)
}
\arguments{
\item{data}{A numeric vector representing the data.}
}
\value{
The index of the optimal elbow point.
}
\description{
This function identifies the optimal elbow point in a data vector using the elbow method.
}
\examples{
data <- c(1, 2, 3, 4, 5, 4, 3, 2, 1)
optimal_elbow(data)

}
