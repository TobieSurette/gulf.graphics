#' @title Draw Error Bars
#' 
#' @description Draw error bars with specified error levels at the specified coordinates.
#' 
#' @param x Horizontal coordinates.
#' @param y Central value(s) or lower and upper bound(s).
#' @param sigma Error deviation from \code{y} to be used to define the lower and upper error bounds.
#' @param lower Lower error bound or deviation (if \code{sigma} is specified).
#' @param upper Upper error bound or deviation (if \code{sigma} is specified).
#' @param add Logical value specifying whether the error bars are to be added to the current plot (Default
#'            is \code{TRUE}).
#' 
#' @examples 
#' # Constant error:
#' error.bar(1:10, y = rnorm(10), sigma = 2, add = FALSE)
#'  
#' # Variable error:
#' error.bar(1:10, y = rnorm(10), sigma = 0.5 + runif(10), add = FALSE)
#' 
#' # Explicit bounds:
#' error.bar(1:10, lower = 2, upper = 2 + runif(10), add = FALSE)
#' 
#' @export error.bar
#' 

error.bar <- function(x, y, sigma, lower, upper, polygon = FALSE, col = "grey30", border, add = TRUE, ...){
   # Parse input arguments:
   if (!is.null(dim(x))) x <- as.vector(x)
   if (!missing(y)){
      if (!is.null(dim(y))) if (ncol(y) == 1) y <- y[, 1]
      if (is.vector(y)){
         if (!missing(lower) & !missing(upper)){
            lower <- y - lower
            upper <- y + upper
         }
         if (!missing(sigma)){
            lower <- y - sigma  
            upper <- y + sigma
         }
      }
      if (!is.null(dim(y))){
         if (ncol(y) != 2) stop("'y' must contain two columns.")
         lower <- y[, 1]
         upper <- y[, 2]
      }
   }
   if (length(lower) == 1) lower <- rep(lower, length(x))
   if (!missing(lower) & !missing(upper)) r <- data.frame(lower = lower, upper = upper)
   if (missing(lower) | missing(upper)) stop("Bounds for error bars are undefined.")
   
   if (missing(x)) stop("Horizontal coordinates 'x' must be specified.")
   x <- as.vector(x)
   if (length(x) != nrow(r)) stop("'x' and error bounds have inconsistent sizes.")
      
   # Create plot if none exists:
   if (!add | is.null(dev.list())) plot(range(x), c(min(r$lower), max(r$upper)), type = "n", xlab = "", ylab = "")
   
   # Draw error bars:
   if (!polygon){
      # Draw error bars:
      w <- 0.15 * (length(x) / diff(par("usr")[1:2]))
      for (i in 1:nrow(r)){
         lines(rep(x[i], 2), c(r$lower[i], r$upper[i]), ...)
         lines(c(x[i]-w, x[i]+w), rep(r$lower[i], 2), ...)
         lines(c(x[i]-w, x[i]+w), rep(r$upper[i], 2), ...)
      }
   }else{
      # Draw polygon region:
      if (missing(border)) border <- col
      polygon(c(x, rev(x)), c(lower, rev(upper)), col = fade(col), border = border, ...)
   }
}
