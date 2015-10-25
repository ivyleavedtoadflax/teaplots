#' @title sent_case
#'
#' @description \code{sent_case} Count number of zeros within a vector
#'
#' @details This functions counts the number of zeros in a vector
#'
#' @param x A vector
#'
#' @return numeric vector of length 1 giving number of zero values in vector \code{x}.
#'
#' @examples
#'
#' a <- cbind(1:10,0)
#' make_plot(x)
#' 
#' @export


sent_case <- function(x) {
  
  x <- gsub("_", " ", x, perl = TRUE)
  x <- gsub("\\b([a-z])([a-z]+)", "\\U\\1\\L\\2", tolower(x), perl = TRUE)
  return(x)
  
}

