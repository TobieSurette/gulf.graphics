% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/colorbar.R
\name{colorbar}
\alias{colorbar}
\alias{colourbar}
\title{Draw a Colour Bar}
\usage{
colorbar(
  levels,
  labels,
  colors = "black",
  caption,
  label.cex = 0.8,
  caption.cex = 0.8,
  width = 0.2,
  height = 1,
  pos = 4,
  nlevels = 5,
  range = c(0, 1),
  box,
  border,
  border.col = "black",
  aspect.adjust = TRUE,
  insert = TRUE,
  smooth = FALSE,
  bar.width = 0.25,
  bar.height = 0.75,
  plus.category = FALSE,
  horizontal = FALSE,
  add = FALSE,
  ...
)
}
\arguments{
\item{levels}{Character or numerical vector specifying labels or level values used to delimit color bar categories.}

\item{labels}{Character strings specifying the labels of the color bar categories. The number of elements is one less
than the number of numerical elements in \code{levels}, or an equal number if \code{levels} is categorical.}

\item{colors}{Vector of color definitions. If the number of colors is not equal to or one less than the number of levels,
then the specified colors are interpolated to generate a color vector of appropriate length.}

\item{caption}{Caption for the color bar.}

\item{label.cex}{Text size of the labels.}

\item{caption.cex}{Text size of the caption.}

\item{width}{Numeric value between zero and one specifying the proportion of the width of the plotting axes to be taken
up by the axes of the color bar. The default is \code{0.2}. If \code{horizontal = TRUE} then the \code{width} 
and \code{height} arguments are exchanged.}

\item{height}{Numeric value between zero and one specifying the proportion of the height of the plotting axes to be 
taken up by the axes of the color bar. The default is \code{1}. If \code{horizontal = TRUE} then the
\code{width} and \code{height} arguments are interchanged.}

\item{pos}{An integer specifying whether to plot the colorbar at the bottom (\code{= 1}), left (\code{= 2}), 
top (\code{= 3}) to the right (\code{= 4}) of the plotting axes.}

\item{nlevels}{An integer specifying then number of desired levels if \code{levels} cannot be determined from other parameters.}

\item{range}{A two-element numeric vector specifying the desired range of \code{levels} if it cannot be determined from other parameters.}

\item{box}{Logical value specifying whether to draw a bounding box about the colorbar axes. Alternatively, the color 
of the box may be given. The default value is \code{FALSE}.}

\item{border}{Logical value specifying whether to draw a bounding rectangle around the entire color bar. The default is \code{FALSE}.}

\item{border.col}{A character value specifying the color of the bounding rectangle around the color bar. The default is \code{\sQuote{black}}.}

\item{aspect.adjust}{Logical value specifying whether the dimensions of both the color bar and graph plotting area are 
to be adjusted in such a way as the aspect ratio of the plotting axes is conserved. The height of the
color bar axes and the plotting axes are also set to be equal. This is only applied if \code{insert = TRUE}.
The default value is \code{TRUE}.}

\item{insert}{Logical value specifying whether the color bar axes are to be inserted, i.e. a space is created adjacent to, 
the present plotting axes. The default is \code{TRUE}.}

\item{smooth}{Logical value wspecifying whether the color bar is a continuum of interpolated colors rather than a discrete 
set of colored rectangles. The default is \code{FALSE}.}

\item{plus.category}{Logical value specifying whether to include a plus category if the labels are numeric. The default is \code{FALSE}.}

\item{horizontal}{Logical value specifying whether the color bar is to be drawn horizontally rather than vertically. The default is \code{FALSE}.}

\item{add}{Logical value specifying whether the color bar is to be drawn in the present plot rather than a new one. The default is \code{FALSE}.}

\item{bar.width.bar.height}{Numeric values specifying the proportion of the color axes to be taken by the width and height of the color bar. The default is \code{0.25}.}

\item{list()}{Further arguments passed onto \code{colorbar} sub-functions.}
}
\description{
Draws a colour bar on a plot.

Since \code{R} does not use dynamic graphical objects, i.e. graphical objects cannot be modified after
they are plotted, the user cannot call the \code{colorbar} function and expect an existing plot to 
adjust its layout in accordance. Thus, the intended use is that the \code{colorbar} function be called 
prior to rather than after plotting.
}
\examples{
# Default colour bar:
colorbar()

# Categorical colour bar:
dev.new()
colorbar(c("Low", "Medium", "High", "Extreme"),
         color = c("yellow", "blue", "white", "red"),
         caption = c("Caption", "Text"), border = TRUE)

# Display continuous color bar:
dev.new()
colorbar(seq(0, 20, by = 4), color = c("blue", "yellow", "red"),
         caption = c("Temperature", "(Celsius)"), smooth = TRUE)

# Volcano elevation example using 'image':
dev.new()
levels <- seq(90, 200, by = 5)
colors <- terrain.colors(length(levels)-1)
colorbar(levels, col = colors, caption = c("Elevation", "(meters)"), label.cex = 0.7, smooth = TRUE)
image(x = seq(0, 1, len = dim(volcano)[1]),
      y = seq(0, 1, len = dim(volcano)[2]),
      z = volcano,
      zlim = c(min(levels), max(levels)),
      col = colors, xlab = "x", ylab = "y")
box()

# Volcano elevation example using '.filled.contour':
dev.new()
levels <- seq(90, 200, by = 5)
colors <- terrain.colors(length(levels)-1)
colorbar(levels, col = colors, caption = c("Elevation", "(meters)"), label.cex = 0.7)
plot.new() # Seems to require this step, not sure why.
graphics::.filled.contour(x = seq(0, 1, len = dim(volcano)[1]),
                          y = seq(0, 1, len = dim(volcano)[2]),
                          z = volcano,
                          levels = levels, col = colors)
box()
}
\seealso{
\code{\link[graphics]{filled.contour}}
}
