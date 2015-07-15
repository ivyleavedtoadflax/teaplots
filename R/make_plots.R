#' @title make_plot
#'
#' @description \code{make_plot} Count number of zeros within a vector
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
#' @import dplyr
#' @export


make_plot <- function(x) {
     
     par(
          bty = "u",
          lend = "square",
          mar = c( 4.1, 3.9, 1, 4.1),
          mgp = c( 1.95, 0.6, 0),
          lend = "square",
          lwd = 0.5,
          cex = 1.2
     )
     
     title_text <- paste(i, ", ", unique(x$Country), sep = "") %>% tolower
     title_text <- gsub("\\b([a-z])([a-z]+)", "\\U\\1\\L\\2", title_text, perl = TRUE)
     
     b <- barplot(
          names.arg = x$MONTH,
          height = x$PRCP_mean,
          border = FALSE,
          ylim = c(0,600),
          ylab = expression(Temperature~(degree~C)),
          yaxt = "n",
          yaxs = "i",
          space = 1,
          col = rgb(149/255, 179/255, 215/255, 1),
          main = title_text
     )
     
     se_lines(
          x = b,
          SE = x$PRCP_sd,
          y = x$PRCP_mean
     )
     
     axis(
          side = 4,
          at = seq(0,600,100),
          las = 1,
          lwd = 0.5
     )
     
     
     conversion <- (600/40)
     
     points(
          x = b,
          y = x$MIN_mean * conversion ,
          pch = 1
     )
     points(
          x = b,
          y = x$TEMP_mean * conversion ,
          pch = 16
     )
     points(
          x = b,
          y = x$MAX_mean * conversion ,
          pch = 1
     )
     
     
     
     TeaPlots::se_lines(
          x = b,
          SE = x$MIN_sd * conversion ,
          y = x$MIN_mean * conversion 
     )
     TeaPlots::se_lines(
          x = b,
          SE = x$MAX_sd * conversion ,
          y = x$MAX_mean * conversion 
     )
     TeaPlots::se_lines(
          x = b,
          SE = x$TEMP_sd * conversion ,
          y = x$TEMP_mean * conversion 
     )
     
     axis(
          side = 2,
          at = seq(0,40,10)  * conversion ,
          labels = seq(0,40,10),
          las = 1,
          lwd = 0.5
     )
     
     mtext(
          expression(Rainfall~(mm)),
          side = 4,
          line = 2.6
     )
     
}

