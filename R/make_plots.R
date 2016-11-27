#' @title make_plot
#'
#' @description \code{make_plot} a plot for a tea growing region
#' @details This function takes a dataframe `x` and creates plots plots based on that data
#'
#' @param x A dataframe of weather data
#'
#' @return A plot.
#'
#' @examples
#'
#' library(teaplots)
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
     
     title_text <- paste(i, ", ", unique(x$Country), sep = "")
     title_text <- sent_case(title_text)
     
     x_PRCP <- x %>%
       dplyr::filter(KEY == "PRCP")
     
     if (nrow(x_PRCP) == 0) {
       
       x_PRCP <- data.frame(
         MONTH = month(1:12, label = TRUE, abbr = TRUE),
         VALUE1_mean = 0,
         VALUE1_sd = 0
       )

     }
     
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
          y = x_PRCP$VALUE1_mean,
          color = "lightsteelblue4"
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

