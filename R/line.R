#' Draw Horizontal or Vertical Lines
#'
#' @description These functions are used to draw horizontal or vertical lines on an existing plot.
#'
#' @param x A numeric vector.
#' @param lower A vector which defines the line(s)' lower plot range.
#' @param upper A vector which defines the line(s)' upper plot range.
#' @param label Line label(s).
#' @param at Numeric value(s) specifying where along the line the labels are to be displayed.
#' 
#' @param ... Arguments passed onto \code{\link[graphics]{lines}} and \code{\link[graphics]{text}}.
#'
#' @examples
#'   plot(c(0, 10), c(0, 10), type = "n")
#'   hline(0, col = "red", lty = "solid")
#'   vline(9, col = "red", lty = "dashed")
#'   vline(1:5, col = "black", lty = "solid", lwd = 2)

#' @describeIn line Draw horizontal lines.
#' @export hline
hline <- function(x, lower, upper, label, at, ...){
   # Define default line bounds:
   if (missing(lower)) lower <- graphics::par("usr")[1]
   if (missing(upper)) upper <- graphics::par("usr")[2]

   # Remove lines which are not in the plot region:
   x <- x[(x >= graphics::par("usr")[3]) & (x <= graphics::par("usr")[4])]

   # Check argument sizes and draw lines:
   if (length(x) > 0){
      if (length(lower) == 1) lower <- rep(lower, length(x))
      if (length(upper) == 1) upper <- rep(upper, length(x))
      if (length(lower) != length(upper)) stop("'lower' and 'upper' have inconsistent dimensions.")
      if (any(lower > upper)) stop("Lower bounds cannot be greater than upper bounds.")
      if (length(lower) != length(x)) stop("Number of lines and bounds are inconsistent.")
      for (i in 1:length(x)) graphics::lines(c(lower[i], upper[i]), rep(x[i], 2), ...)
      
      # Display labels:
      if (!missing(label)){
         if (!missing(at)) if (length(at) == 1) at <- rep(at, length(x))
         if (missing(at)) at <- (lower + upper) / 2
         if (length(label) == 1) label <- rep(label, length(x))
         for (i in 1:length(x)) text(at[i], x[i], label[i], ...)
      }
   }
}

#' @describeIn line Draw vertical lines.
#' @export vline
vline <- function(x, lower, upper, label, at, ...){
   # Define default line bounds:
   if (missing(lower)) lower <- graphics::par("usr")[3]
   if (missing(upper)) upper <- graphics::par("usr")[4]

   # Remove lines which are not in the plot region:
   x <- x[(x >= graphics::par("usr")[1]) & (x <= graphics::par("usr")[2])]

   # Check argument sizes and draw lines:
   if (length(x) > 0){
      if (length(lower) == 1) lower <- rep(lower, length(x))
      if (length(upper) == 1) upper <- rep(upper, length(x))
      if (length(lower) != length(upper)) stop("'lower' and 'upper' have inconsistent dimensions.")
      if (any(lower > upper)) stop("Lower bounds cannot be greater than upper bounds.")
      if (length(lower) != length(x)) stop("Number of lines and bounds are inconsistent.")
      for (i in 1:length(x)) graphics::lines(rep(x[i], 2), c(lower[i], upper[i]), ...)
      
      # Display labels:
      if (!missing(label)){
         if (!missing(at)) if (length(at) == 1) at <- rep(at, length(x))
         if (missing(at)) at <- (lower + upper) / 2
         if (length(label) == 1) label <- rep(label, length(x))
         for (i in 1:length(x)) text(x[i], at[i], label[i], ...)
      }
   }
}
