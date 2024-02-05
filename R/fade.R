#' @title Fade Colours
#'
#' @description Adds transparency values to a set of colours.
#'
#' @param x \code{R} colours.
#'
#' @export fade
#' 

fade <- function(x, alpha = 0.5){
   # Convert colours to RGB values:
   y <- col2rgb(x, alpha = TRUE) / 255
   
   # Buffer colors with default alpha value:
   if (nrow(y) == 3){
      y <- rbind(y, 1)
      rownames(y)[4] <- "alpha"
   }
   
   # Fade colours:
   y["alpha", ] <- alpha * y["alpha", ]
      
   # Convert RGB values to colours:
   y <- rgb(y["red",], y["green",], y["blue",], alpha = y["alpha", ])
 
   return(y)  
}
