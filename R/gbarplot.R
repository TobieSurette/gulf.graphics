#' Gulf Bar Plot
#'
#' @description Doug's alternative version of the \code{\link[graphics]{barplot}}
#' function. Numeric \code{x} values may be passed directly for proper positioning along the
#' X axis, allowing for easy overlay in existing plots. If both positive and negative 'y'
#' values are present, then the resulting stacked plot has bars separated above and below the X
#' axis.
#'
#' @param y A numeric vector, table, matrix or data frame object containing numeric values which are
#'          to be displayed as a bar plot. Each row of the matrix will be plotted as a separate bar.
#'
#' @param x A vector specifying the numeric values or labels to be used for separating each bar along
#'          the horizontal axis.
#'
#' @param labels A character vector specifying labels to be used for labelling each bar along the horizontal
#'               axis. The number of labels must equal or be consistent with those of \code{x} or \code{y}.
#'
#' @param width A numeric value from 0 to 1 specifying the relative width of each bar. A value of zero corresponds
#'              to a bar with no width while a value of one implies that adjacent bars are touching (i.e. there
#'              is no space between the bars). The default value is \code{0.8}.
#'
#' @param col Vector specifying the colour(s) of the bars to be drawn. Each column of 'y' is drawn in a separate
#'            colour. The default colour for a single variable is "grey". If there is more than one variable
#'            and \code{col} is unspecified, then colours are interpolated from a grayscale palette.
#'
#' @param border Border colour(s) of the bars.
#'
#' @param add A logical value specifying if bars should be added to an existing plot. The default is \code{FALSE}.
#' 
#' @param grid Logical value specifying whether to draw a grid.
#'
#' @param yaxs A character value specifying how to set the limits of the Y axis, see \code{\link[graphics]{par}}.
#'
#' @param legend A logical value specifying whether a legend is to be drawn. The default is \code{TRUE}.
#'             The legend entries are taken from the columns names of \code{y}.
#'
#' @param \dots Further arguments to be passed onto the \code{\link[graphics]{plot}} function.
#'
#' @examples
#'
#' # Generate random vector:
#' y <- 10*rnorm(5)+5
#'
#' # Simple barplot:
#' gbarplot(y)
#'
#' # Same barplot with explicit 'x':
#' gbarplot(y, x = c(1, 2, 4, 5, 7))
#'
#' # Add explicit labels:
#' gbarplot(y, x = c(1, 2, 4, 5, 7), labels = letters[1:5])
#'
#' # Display bar plot:
#' y <- data.frame("Variable 1" = c(1, -2, NA, -4, 5), "Variable 2" = 1:5)
#' gbarplot(y, xlab = "Order", ylab = "y",
#'          labels = c("First", "Second", "Third", "Fourth", "Fifth"))
#'
#' # Display a frequency table overlayed by the true distribution:
#' y <- table(round(rnorm(250, sd = 5)))
#' gbarplot(y, col = "grey", width = 1)
#' x <- seq(-15, 15, len = 100)
#' lines(x, 250 * dnorm(x, sd = 5), col = "red", lwd = 2)
#'
#' @export gbarplot
#' @seealso \code{\link{error.bar}}
#' 
gbarplot <- function(y, x, labels, width = 1, col = "grey", border = "grey50", add = FALSE, 
                     grid = FALSE, yaxs = ifelse(all(y[!is.na(y)] <= 0)|all(y[!is.na(y)] >= 0), "i", "r"), 
                     legend = TRUE, ...){

   # Parse 'y' argument:
   if (is.table(y)){
      y <- as.matrix(y)
      class(y) <- "matrix"
      if (nrow(y) == 1) y <- t(y)
      x <- as.numeric(rownames(y))
      if (any(is.na(x))) x <- NULL
   }
   if (!is.data.frame(y)) y <- as.data.frame(y)
   if (nrow(y) == 1) y <- t(y)

   # Define bar colours:
   if (length(col) == 1)       col <- colorRampPalette(c("white", col))(ncol(y))
   if (length(col) == ncol(y)) col <- gulf.utils::repvec(col, nrow = nrow(y))
   if (length(border) == 1)       border <- colorRampPalette(c("white", border))(ncol(y))
   if (length(border) == ncol(y)) border <- gulf.utils::repvec(border, nrow = nrow(y))
   
   # Define 'x' as an integer sequence if undefined:
   if (missing(x)){
      x <- as.numeric(rownames(y))
      if (any(is.na(x))) x <- 1:nrow(y)
   }

   # Define 'labels' as an integer sequence if undefined:
   if (missing(labels)) if (!is.null(x)) labels <- as.character(x) else labels <- rownames(y)

   # If 'x' is a character vector, define 'labels' as 'x':
   if (is.character(x)){
      labels <- x
      x <- 1:length(labels)
   }

   # Check if 'x' is a vector:
   if (!is.vector(x)) stop("'x' must be a vector.")

   # Check that 'x' and 'labels' are the same length:
   if (length(x) != length(labels)) stop("'x' and 'labels' must be the same length.")

   # Order data by values of 'x':
   ix<- order(x)
   x <- x[ix]
   y <- y[ix, , drop = FALSE]
   labels <- labels[ix]
   col <- col[ix, , drop = FALSE]
   border <- border[ix, , drop = FALSE]
   
   # Modify bar width:
   if (length(width) == 1){
      tmp <- sort(unique(diff(x)))
      tmp <- tmp[tmp > 0]
      if (length(tmp) > 0) width <- width * tmp[1]
   }
   
   # Define y axis limits:
   ylim <- c(NA, NA)
   temp <- y
   temp[y > 0] <- 0
   ylim[1] <- min(apply(temp, 1, sum, na.rm = TRUE))
   temp <- y
   temp[y < 0] <- 0
   ylim[2] <- max(apply(temp, 1, sum, na.rm = TRUE))
   if (yaxs == "i"){
      if (all(y[!is.na(y)] < 0)) ylim[1] <- 1.04 * ylim[1]
      if (all(y[!is.na(y)] > 0)) ylim[2] <- 1.04 * ylim[2]
   }

   # Create blank axes:
   if (!add){
      plot(c(min(x) - width/2, max(x) + width/2), ylim, type = "n", ann = FALSE, xaxt = "n", yaxs = yaxs, ...)
      if (all(as.character(x) == labels)) axis(1, ...)
      else axis(1, at = x, labels = labels, ...)
      if (grid) grid()
   }
   
   # Plot figure title:
   title(...)

   # Loop over each bar:
   for (i in 1:length(x)){
      y.lower <- 0
      y.upper <- 0

      # Draw negative bars:
      index <- which(!(is.na(y[i, ])) & (y[i, ] < 0))
      if (length(index) > 0){
         for (j in 1:length(index)){
            xx <- c(x[i] - width / 2, x[i] - width / 2, x[i] + width / 2, x[i] + width / 2, x[i] - width / 2)
            yy <- c(y.lower, y.lower + y[i, index[j]], y.lower + y[i, index[j]], y.lower, y.lower)
            polygon(xx, yy, col = col[i, index[j]], border = border[i, index[j]], ...)
            y.lower <- y.lower + y[i, index[j]]
         }
      }

      # Draw positive bars:
      index <- which(!(is.na(y[i, ])) & (y[i, ] >= 0))
      if (length(index) > 0){
         for (j in 1:length(index)){
            xx <- c(x[i] - width / 2, x[i] - width / 2, x[i] + width / 2, x[i] + width / 2, x[i] - width / 2)
            yy <- c(y.upper, y.upper + y[i, index[j]], y.upper + y[i, index[j]], y.upper, y.upper)
            polygon(xx, yy, col = col[i, index[j]], border = border[i, index[j]], ...)
            y.upper <- y.upper + y[i, index[j]]
         }
      }
   }

   # Draw legend:
   if (legend & (ncol(y) > 1)) legend("topleft", fill = col, legend = names(y))

   invisible(col)
}
