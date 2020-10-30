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
#' plot(runif(100), runif(100))
#' dev.off()

#' @export
gdevice <- function(type = "", file, width = 8.5, height = 8.5, ...){
   if (missing(file)) type <- ""
   if (type == ""){
      if (.Platform$OS.type == "unix") quartz(width = width, height = height, ...)
      if (.Platform$OS.type == "windows") windows(width = width, height = height, ...)
   }
   type <- tolower(type)
   if (type == "jpg") type <- "jpeg"
   if (type == "jpeg") jpeg(file = paste0(file, ".jpg"), width = width * 480, height = height * 480, res = 8.5 * 75, ...)
   if (type == "pdf")  pdf(file = paste0(file, ".pdf"), width = width, height = height, ...)
}
