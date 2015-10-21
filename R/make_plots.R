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
     
     x_PRCP <- x %>%
       dplyr::filter(KEY == "PRCP")
     
#      if (nrows(x_PRCP) == 0) {
#       
#        x_PRCP$VALUE_mean <- rep(0 
#        
#      }
     
     x_MIN <- x %>%
       dplyr::filter(KEY == "MIN")
     
     x_TEMP <- x %>%
       dplyr::filter(KEY == "TEMP")
     
     x_MAX <- x %>%
       dplyr::filter(KEY == "MAX")

          
     b <- barplot(
          names.arg = x_PRCP$MONTH,
          height = x_PRCP$VALUE1_mean,
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
          SE = x_PRCP$VALUE1_sd,
          y = x_PRCP$VALUE1_mean
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
          y = x_MIN$VALUE1_mean * conversion ,
          pch = 1
     )
     points(
          x = b,
          y = x_TEMP$VALUE1_mean * conversion ,
          pch = 16
     )
     points(
          x = b,
          y = x_MAX$VALUE1_mean * conversion ,
          pch = 1
     )
     
     
     
     TeaPlots::se_lines(
          x = b,
          SE = x_MIN$VALUE1_sd * conversion ,
          y = x_MIN$VALUE1_mean * conversion 
     )
     TeaPlots::se_lines(
          x = b,
          SE = x_MAX$VALUE1_sd * conversion ,
          y = x_MAX$VALUE1_mean * conversion 
     )
     TeaPlots::se_lines(
          x = b,
          SE = x_TEMP$VALUE1_sd * conversion ,
          y = x_TEMP$VALUE1_mean * conversion 
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

