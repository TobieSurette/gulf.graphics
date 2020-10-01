#' Plot \strong{eSonar} Data
#' 
#' @description Graphically display \code{esonar} data.
#' 
#' @param x \code{esonar} data object.
#' 
#' @examples 
#' plot.esonar(year = 2020, tow.id = "GP354F")
#' 
#' x <- read.esonar(year = 2020, tow.id = "GP354F")
#' plot(x)
#' 
#' @export plot.esonar
#' @export
plot.esonar <- function(x, ...){
   # Define time series in minutes:
   t <- 

   # Define set of variables to display:
   vars <- c("speed", "heading", "depth", "headline") 
   vars <- vars[vars %in% names(x)]
   vars <- vars[!unlist(lapply(x[, vars], function(x) return(all(is.na(x)))))]
   x$wingspread <- wingspread(x)
   vars <- c(vars, "wingspread")
   
   # Prepare layout:
   m <- kronecker(matrix(1:length(vars)), matrix(1, ncol = 5, nrow = 5))
   m <- rbind(0, cbind(0, m, 0), 0, 0)
   layout(m)
   par(mar = c(0,0,0,0))

   # Plot secondary sensor profile:
   xlim = range(time)
   for (i in  1:length(vars)){
      y <- x[, vars[i]]

      if (!(vars[i] %in% "heading")) ylim = c(0, 1.15 * max(y, na.rm = TRUE)) else ylim <- range(y, na.rm = TRUE)
      plot(xlim, ylim, type = "n", xlab = "", ylab = "", xaxs = "i", yaxs = "i", xaxt = "n")
      if (!any(is.na(y))){
         lines(time, y, col = "blue", lwd = 2)
      }else{
         points(time, y, pch = 21, bg = "blue")
      }
      
      # Y axis labels:
      ylab <- vars[i]
      ylab <- paste0(toupper(substr(ylab, 1, 1)), substr(ylab, 2, nchar(ylab)))
      if (vars[i] == "wingspread") u <- "meters" else u <- gulf.metadata::units(x)[vars[i]]
      u <- gsub("meters", "m", u)
      u <- gsub("degrees", "°", u)
      u <- gsub("knots", "kts", u)
      
      if (length(u) > 0) ylab <- paste0(ylab, "(", u, ")")
      mtext(ylab, 2, 2.5, cex = 0.8)
      
      # X axis:
      if (i == length(vars)){
         mtext("Time(min)", 1, 2.5, cex = 0.8)
         axis(1)
      } 
   }
}
