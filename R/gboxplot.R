#' Boxplot
#'
#' @description Draws a boxplot based on input data.
#'
#'

#' @export gboxplot
gboxplot <- function(y, x, labels, width = 0.5, xlab = "", ylab = "", alpha = 0.05,
                     lwd = 1, col = "black", bg = "white", ...){

    # Check input arguments:
    y <- as.matrix(y)
    if (!all(dim(y) > 1)) stop("'y' must be a matrix.")
    if ((length(alpha) != 1) | (alpha < 0) | (alpha > 1)) stop("'alpha' must be a numeric value between 0 and 1.")

    # Calculate 'y' summary statistics:
    med <- apply(y, 2, median)
    lq <-  apply(y, 2, quantile, 0.25)
    uq <-  apply(y, 2, quantile, 0.75)
    li <-  apply(y, 2, quantile, alpha / 2)
    ui <-  apply(y, 2, quantile, 1- alpha / 2)

    # Determine "x" labels:
    if (missing(x)) if (!is.null(dimnames(y)[[2]])) x <- dimnames(y)[[2]] else x <- 1:dim(y)[2]
    if (length(x) != dim(y)[2]) stop(paste("'x' must have", dim(y)[2], "elements."))

    # Define plot intervals:
    if (is.numeric(x)){
       las = 1
       xlim <- c(min(x), max(x))
    }else{
       las = 2
       labels <- as.character(x)
       x <- 1:length(x)
       xlim <- c(1, length(x))
    }
    if (missing(labels)) labels <- as.character(x)
    if (!is.null(labels)){
       if (length(labels) == 1) if (labels == "") labels <- rep("", length(x))
       if (length(labels) != length(x)) stop("Tick labels and 'x' have inconsistent lengths.")
    }

    # Define default y axis limits:
    ylim <- c(min(li), max(ui))

    # Number of categories:
    n <- length(x)

    # Create empty plot:
    graphics::plot(xlim, ylim, type = "n", xaxt = "n", xlab = xlab, ylab = ylab, ...)

    # Label axes:
    if (!is.null(labels)) axis(1, at = x, labels = labels, las = las, ...)

    # Calculate box widths:
    w <- min(diff(x)) * width / 2

    # Draw boxes:
    for (i in 1:n){
        color <- col[((i-1) %% length(col)) + 1]
        background <- bg[((i-1) %% length(bg)) + 1]

        # Upper quartile box:
        xx <- c(x[i]-w, x[i]-w, x[i]+w, x[i]+w, x[i]-w)
        yy <- c(med[i], uq[i], uq[i], med[i], med[i])
        graphics::polygon(xx, yy, lwd = lwd, col = background, ...)

        # Lower quartile box:
        xx <- c(x[i]-w, x[i]-w, x[i]+w, x[i]+w, x[i]-w)
        yy <- c(med[i], lq[i], lq[i], med[i], med[i])
        graphics::polygon(xx, yy, lwd = lwd, col = background, ...)

        # Draw median line:
        graphics::lines(c(x[i]-w, x[i]+w), c(med[i], med[i]), lwd = (lwd + 1), col = color)

        # Upper whisker line:
        graphics::lines(c(x[i], x[i]), c(uq[i], ui[i]), lty = "dashed", lwd = lwd, col = color)
        graphics::lines(c(x[i]-w/2, x[i]+w/2), c(ui[i], ui[i]), lwd = lwd, col = color)

        # Lower whisker line:
        graphics::lines(c(x[i], x[i]), c(lq[i], li[i]), lty = "dashed", lwd = lwd, col = color)
        graphics::lines(c(x[i]-w/2, x[i]+w/2), c(li[i], li[i]), lwd = lwd, col = color)
    }
}
