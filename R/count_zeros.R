#' @title count_zeros
#'
#' @description \code{count_zeros} Count number of zeros within a vector
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
#' count_zeros(x)
#' 
#' @export

count_zeros <- function(x) {
     
     x <- x[!is.na(x)]
     return(
          sum((x == 0) * 1)
     )
     
}
