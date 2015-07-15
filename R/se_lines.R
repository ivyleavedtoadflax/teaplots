#' @title se_lines
#'
#' @description \code{se_lines} Add s confidence intervals to scatterplot
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

se_lines <- function(
     x, 
     SE, 
     y, 
     bar_width = 0.1
) {
     
     #SE = 0.5 * SE
     
     for (i in 1:length(x)) {
          lines(
               c(x[i],x[i]),
               c(y[i]+SE[i],y[i]-SE[i])
          )
          
          lines(
               c(x[i]-bar_width,x[i]+bar_width),
               c(y[i]+SE[i],y[i]+SE[i])
          )
          
          lines(
               c(x[i]-bar_width,x[i]+bar_width),
               c(y[i]-SE[i],y[i]-SE[i])
          )
     }
}
