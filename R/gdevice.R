#' Prepare Graphics Device
#'
#' @description A wrapper function to prepare a graphics device for output.
#'
#' @param type Character string specifying the type of graphics device to be created (e.g. 'pdf', 'jpg', ...).
#' @param file File name, excluding file extension suffix.
#' @param width,height Dimensions in inches.

#'
#' @examples
#' gdevice("pdf", "Test.pdf")
#' gdevice() # Equivalent to 'dev.new()'.
#'
#' plot(runif(100), runif(100))
#' dev.off()
#'

gdevice <- function(type == "", file, width = 8.5, height = 8.5){
   if (missing(file)) type <- ""
   if (type == "jpeg") jpeg(file = file, width = width * 480, height = height * 480, res = 8.5 * 75)
   if (type == "pdf")  pdf(file = file, width = width, height = height)
   if (type == "")     dev.new(width = width, height = height)
}
