#' Draw a Colour Bar
#'
#' Draws a colour bar on a plot.
#'
#' Since \code{R} does not use dynamic graphical objects, i.e. graphical objects cannot be modified after
#' they are plotted, the user cannot call the \code{colorbar} function and expect an existing plot to adjust its layout in
#' accordance. Thus, the intended use is that the \code{colorbar} function be called prior to rather than after plotting.
#' See examples below.
#'
#' This function is designed to be compatible with the
#' \code{\link[graphics]{filled.contour}} function.
#'
#' @aliases colorbar colourbar
#' @param levels A character or numerical vector specifying the labels or level values used to delimit color bar categories.
#' @param labels A character vector specifying the labels of the color bar categories.  The number of elements is one less
#'               than the number of numerical elements in \code{levels}, or an equal number if \code{levels} is categorical.
#' @param colors A vector of color defintions. If the number of colours is not equal to or one less than the number of levels,
#'               then the specified colors are interpolated to generate a color vector of appropriate length.
#' @param caption A character vector specifying the caption of the color bar.
#' @param label.cex Text size of the labels.
#' @param caption.cex Text size of the caption.
#' @param width A numeric value between zero and one specifying the proportion of the width of the plotting axes to be taken
#'              up by the axes of the color bar. The default is \code{0.2}. If \code{horizontal = TRUE} then the
#' \code{width} and \code{height} arguments are exchanged.
#' @param height A numeric value between zero and one specifying the proportion
#' of the height of the plotting axes to be taken up by the axes of the color
#' bar. The default is \code{1}. If \code{horizontal = TRUE} then the
#' \code{width} and \code{height} arguments are exchanged.
#' @param pos An integer specifying whether to plot the colorbar at the bottom
#' (\code{= 1}), left (\code{= 2}), top (\code{= 3}) to the right (\code{= 4})
#' of the plotting axes.
#' @param nlevels An integer specifying then number of desired levels if
#' \code{levels} cannot be determined from other parameters.
#' @param range A two-element numeric vector specifying the desired range of
#' \code{levels} if it cannot be determined from other parameters.
#' @param box A logical value specifying whether to draw a bounding box about
#' the colorbar axes. Alternatively, the color of the box may be given. The
#' default value is \code{FALSE}.
#' @param border A logical value specifying whether to draw a bounding
#' rectangle around the entire color bar. The default is \code{FALSE}.
#' @param border.col A character value specifying the color of the bounding
#' rectangle around the color bar. The default is \code{\sQuote{black}}.
#' @param aspect.adjust A logical value specifying whether the dimensions of
#' both the color bar and graph plotting area are to be adjusted in such a way
#' as the aspect ratio of the plotting axes is conserved. The height of the
#' color bar axes and the plotting axes are also set to be equal. This is only
#' applied if \code{insert = TRUE}. The default value is \code{TRUE}.
#' @param insert A logical value specifying whether the color bar axes are to
#' be inserted, i.e. a space is created adjacent to, the present plotting axes.
#' The default is \code{TRUE}.
#' @param smooth A logical value wspecifying whether the color bar is a
#' continuum of interpolated colors rather than a discrete set of colored
#' rectangles. The default is \code{FALSE}.
#' @param bar.width A numeric value specifying the proportion of the color axes
#' to be taken by the width of the color bar. The default is \code{0.25}.
#' @param bar.height A numeric value specifying the proportion of the color
#' axes to be taken by the total height of the color bar.  The default is
#' \code{0.75}.
#' @param plus.category A logical value specifying whether to include a plus
#' category if the labels are numeric. The default is \code{FALSE}.
#' @param horizontal A logical value specifying whether the color bar is to be
#' drawn horizontally rather than vertically. The default is \code{FALSE}.
#' @param add A logical value specifying whether the color bar is to be drawn
#' in the present plot rather than a new one. The default is \code{FALSE}.
#' @param list() Further arguments passed onto \code{colorbar} sub-functions.
#' @return No value is returned.
#' @seealso \code{\link[graphics]{.filled.contour}}
#' @keywords IO
#' @examples
#'
#'    # Default colour bar:
#'    colorbar()
#'
#'    # Categorical colour bar:
#'    dev.new()
#'    colorbar(c("Low", "Medium", "High", "Extreme"),
#'             color = c("yellow", "blue", "white", "red"),
#'             caption = c("Caption", "Text"), border = TRUE)
#'
#'    # Display continuous color bar:
#'    dev.new()
#'    colorbar(seq(0, 20, by = 4), color = c("blue", "yellow", "red"),
#'             caption = c("Temperature", "(Celsius)"), smooth = TRUE)
#'
#'    # Volcano elevation example using 'image':
#'    dev.new()
#'    levels <- seq(90, 200, by = 5)
#'    colors <- terrain.colors(length(levels)-1)
#'    colorbar(levels, col = colors, caption = c("Elevation", "(meters)"),
#'    label.cex = 0.7, smooth = TRUE)
#'    image(x = seq(0, 1, len = dim(volcano)[1]),
#'          y = seq(0, 1, len = dim(volcano)[2]),
#'          z = volcano,
#'          zlim = c(min(levels), max(levels)),
#'          col = colors, xlab = "x", ylab = "y")
#'    box()
#'
#'    # Volcano elevation example using '.filled.contour':
#'    dev.new()
#'    levels <- seq(90, 200, by = 5)
#'    colors <- terrain.colors(length(levels)-1)
#'    colorbar(levels, col = colors, caption = c("Elevation", "(meters)"),
#'    label.cex = 0.7)
#'    plot.new() # Seems to require this step, not sure why.
#'    .filled.contour(x = seq(0, 1, len = dim(volcano)[1]),
#'                    y = seq(0, 1, len = dim(volcano)[2]),
#'                    z = volcano,
#'                    levels = levels, col = colors)
#'    box()
#'
#' @export colorbar
colorbar <- function(levels, labels, colors = "black", caption, label.cex = 0.8,
                     caption.cex = 0.8, width = 0.2, height = 1, pos = 4, nlevels = 5, range = c(0, 1),
                     box, border, border.col = "black", aspect.adjust = TRUE, insert = TRUE,
                     smooth = FALSE, bar.width = 0.25, bar.height = 0.75,
                     plus.category = FALSE, horizontal = FALSE, add = FALSE, ...){
   # COLORBAR - Draw a colorbar on a plot.

   # Create new plot:
   if ((length(grDevices::dev.list()) == 0) | (!add)){
      graphics::plot.new()
      graphics::plot.window(c(0,1), c(0,1), xaxs = "i", yaxs = "i")
   }

   # Save graphics parameters:
   gpar <- graphics::par()

   # Switch 'width' and 'height' arguments:
   if (horizontal){
      temp <- width
      width <- height
      height <- temp
      temp <- bar.width
      bar.width <- bar.height
      bar.height <- temp
   }

   # Rescale plot:
   if (insert){
      w <- graphics::par("plt")
      if (!horizontal){
         w[1] <- w[2] - diff(w[1:2]) * width
         if (aspect.adjust){
            a <- diff(w[3:4])*(1-width)
            w[3] <- (1-a)/2
            w[4] <- (1-a)/2 + a
         }
      }else{
         w[4] <- w[3] + diff(w[3:4]) * height
         if (aspect.adjust){
             a <- diff(w[1:2])*(1-height)
             w[1] <- (1-a)/2
             w[2] <- (1-a)/2 + a
         }
      }
      graphics::par(plt = w, usr = c(0, 1, 0, 1))
   }

   # If labels are given, define 'nlevels':
   if (missing(levels) & !missing(labels)) nlevels <- (length(labels)+1)

   # Define 'levels' if missing:
   if (missing(levels)){
      if (!is.numeric(range) | (length(range) != 2)){
         stop("'range' parameter must be a two-element numeric vector.")
      }
      range <- sort(range)
      levels <- seq(range[1], range[2], len = (nlevels+1))
   }

   # Check if 'levels' is a character vector:
   if (!is.numeric(levels)) levels <- as.character(levels)

   # Define number of colour categories required:
   if (is.numeric(levels)){
      numeric <- TRUE
      k <- length(levels)-1
   }else{
      numeric <- FALSE
      k <- length(levels)
   }

   # Define 'colors' if missing:
   if (missing(colors)) colors <- grDevices::colorRampPalette(c("white", "black"))(k)

   # Interpolate from white to specified color:
   if (length(colors) == 1) colors <- c("white", colors)

   # Interpolate colours using specified colours:
   if (length(colors) < k) colors <- grDevices::colorRampPalette(colors)(k)

   # Extract labels:
   if (missing(labels)) labels <- as.character(levels)

   # Correct format of numerical labels:
   if (numeric){
      index <- setdiff(1:length(labels), grep(".", labels, fixed = TRUE))
      if (length(index) < length(labels)) labels[index] <- paste(labels[index], ".0", sep = "")
      if (plus.category) labels[length(labels)] <- paste(labels[length(labels)], " +", sep = "")
   }

   # Define bar dimensions:
   if (!horizontal){
      td <- max(graphics::strwidth(labels, cex = label.cex)) + 0.5 * label.cex * graphics::strwidth("X")
      td[2] <- max(graphics::strheight(labels, cex = label.cex))
      bx <- (1-(td[1] + bar.width))/2 + td[1]
      bx[2] <- bx[1] + bar.width
      if (missing(caption)) cd <- 0 else cd <- max(graphics::strwidth(caption, cex = caption.cex))
      if (missing(caption)) cd[2] <- 0 else cd[2] <- sum(graphics::strheight(caption, cex = caption.cex)) + 0.75*caption.cex*graphics::strheight("X")
      by <- (1 - (cd[2] + bar.height))/2 + cd[2]
      by[2] <- by[1] + bar.height
   }else{
      td <- 0 # Text width (not used).
      td[2] <- graphics::strheight("X") * label.cex * (1 + 0.5) # Text height + offset.
      cd <- 0
      if (missing(caption)) cd[2] <- 0 else cd[2] <-  graphics::strheight("X") * caption.cex * (length(caption) + 0.5 + 0.5) # Caption height and offset.
      td[2] <- td[2] + cd[2]
      bx <- (1- bar.width)/2
      bx[2] <- bx[1] + bar.width
      by <- (1 - (td[2] + bar.height))/2 + td[2]
      by[2] <- by[1] + bar.height
   }

   # Set levels to normalized coordinates:
   if (numeric){
      levels <- (levels - levels[1]) / diff(range(levels))
      if (!horizontal) levels <- diff(by)*levels + by[1] else levels <- diff(bx)*levels + bx[1]
   }else{
      if (!horizontal) levels <- seq(by[1], by[2], len = length(colors) + 1)
      else levels <- seq(bx[1], bx[2], len = length(colors) + 1)
   }

   # Draw legend boxes:
   if (!smooth){
      if (!horizontal){
         graphics::rect(bx[1], levels[-length(levels)], bx[2], levels[-1L], col = colors, border = NULL)
      }else{
         graphics::rect(levels[-length(levels)], by[1], levels[-1L], by[2], col = colors, border = NULL)
      }
   }else{
      cols <- grDevices::colorRampPalette(colors)(200)
      levs <- seq(levels[1], levels[length(levels)], len = length(cols))
      if (!horizontal){
         graphics::rect(bx[1], levs[-length(levs)], bx[2], levs[-1L], col = cols, border = NA)
         graphics::rect(bx[1], levs[1], bx[2], levs[length(levs)])
      }else{
         graphics::rect(levs[-length(levs)], by[1], levs[-1L], by[2], col = cols, border = NA)
         graphics::rect(levs[1], by[1], levs[length(levs)], by[2])
      }
   }

   # Add labels to plot:
   if (numeric & (length(levels) == length(labels))){
      if (!horizontal) graphics::text(bx[1], levels, labels, cex = label.cex, pos = 2)
      else graphics::text(levels, by[1], labels, cex = label.cex, pos = 1)
   }else{
      if (!horizontal) graphics::text(bx[1], (levels[-length(levels)] + levels[-1L])/2, labels, cex = label.cex, pos = 2)
      else graphics::text((levels[-length(levels)] + levels[-1L])/2, by[1], labels, cex = label.cex, pos = 1)
   }

   # Draw tick marks if 'smooth' is TRUE:
   if (smooth){
     for (i in 1:length(levels)){
        if (!horizontal){
           graphics::lines(c(bx[1], bx[1] + diff(bx)*0.2), c(levels[i], levels[i]))
           graphics::lines(c(bx[1] + diff(bx)*0.8, bx[2]), c(levels[i], levels[i]))
        }else{
           graphics::lines(c(levels[i], levels[i]), c(by[1], by[1] + diff(by)*0.2))
           graphics::lines(c(levels[i], levels[i]), c(by[1] + diff(by)*0.8, by[2]))
        }
     }
   }

   # Add caption:
   if (!missing(caption)){
      if (length(caption) > 1) caption[2:length(caption)] <- paste("\n", caption[2:length(caption)], sep = "")
      if (!horizontal) graphics::text(0.5, by[1], pos = 1, offset = 0.75, caption, cex = caption.cex)
      else{
         graphics::text(0.5, by[1] - max(graphics::strheight(labels, cex = label.cex)) - 0.5 * label.cex * graphics::strheight("X"),
              pos = 1, offset = 0.5, caption, cex = caption.cex)
      }
   }

   # Draw box:
   if (!missing(box)){
      if (is.logical(box)) box <- "black"
      box(col = box)
   }

   # Draw border:
   if (!missing(border)){
      if (is.logical(border)) border <- "black"
      if (!horizontal){
         p <- c(0.8, 0.6)
         mx <- (1-(td[1] + bar.width)) / 2
         xx <- (1-p[1]) * mx
         xx[2] <- mx[1] + td[1] + bar.width + p[1] * mx
         my <- (1-(cd[2] + bar.height)) / 2
         yy <- (1-p[2]) * my
         yy[2] <- my[1] + cd[2] + bar.height + p[2] * my
         graphics::rect(xx[1], yy[1], xx[2], yy[2], border = border.col)
      }else{
         p <- c(0.5, 0.8)
         mx <- (1-(td[1] + bar.width)) / 2
         xx <- (1-p[1]) * mx
         xx[2] <- mx[1] + td[1] + bar.width + p[1] * mx
         my <- (1-(td[2] + bar.height)) / 2
         yy <- (1-p[2]) * my
         yy[2] <- my[1] + td[2] + bar.height + p[2] * my
         graphics::rect(xx[1], yy[1], xx[2], yy[2], border = border.col)
      }
   }

   # Restore previous graphic parameters state:
   graphics::par(plt = gpar$plt)

   # Rescale graph plotting area so that
   if (insert){
      graphics::par(usr = c(0, 1, 0, 1))
      w <- graphics::par("plt")
      if (!horizontal){
         w[2] <-  w[2] - diff(w[1:2]) * width
         a <- diff(w[3:4])*(1-width)
         if (aspect.adjust){
            w[3] <- (1-a)/2
            w[4] <- (1-a)/2 + a
         }
      }else{
         w[3] <- w[4] - diff(w[3:4]) * (1-height)
         a <- diff(w[1:2])*(1-height)
         if (aspect.adjust){
            w[1] <- (1-a)/2
            w[2] <- (1-a)/2 + a
         }
      }
      graphics::par(plt = w, new = TRUE)
   }

   invisible()
}
