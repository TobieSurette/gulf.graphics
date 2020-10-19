#' Draw Graphics Objects 
#' 
#' 


#' @describeIn draw Draw a points interactively on a plot.
#' @export draw.points
draw.points <- function(x = NULL, y = NULL, n = NULL, tol = 5E-3, snap = FALSE,
                        snap.to.grid = FALSE, snap.to.line = FALSE, grid.increment,
                        grid.offset = 0, pch = 21, bg = "black", ...){
   
   # Points supplied by user:
   flag <- FALSE
   if (!missing(x) & !missing(y)){
      if (length(x) != length(y)) stop("Specified 'x' and 'y' coordinate vectors must have the same length.")
      flag <- TRUE
   }

   # Set tolerance flag default:
   tolerance.flag <- TRUE

   # Create default plot:
   if (is.null(dev.list()) & !flag) plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "")
   if (is.null(dev.list()) & flag)  plot(x, y, xlab = "", ylab = "")

   # Define 'grid.increment':
   if (missing(grid.increment)){
      range <- c(0, mean(c(diff(range(par("usr")[1:2])), diff(range(par("usr")[3:4])))))
      grid.increment <- unique(round(diff(pretty(range, n = 100)), 9))
   }

   # Initialize point vectors to be returned:
   xx <- NULL
   yy <- NULL

   # Keyboard button function:
   keyboard.fun <- function(key){
      if (toupper(key) == "M"){
         cat("\n")
         cat("'g'   : Toggle snap-to-grid option.\n")
         cat("'m'   : Display menu options.\n")
         cat("'l'   : Toggle snap to nearest line option.\n")
         cat("'s'   : Toggle snap-to-nearest-point option.\n")
         cat("'t'   : Toggle tolerance option.\n")
         cat("'Esc' : Return coordinates.\n")
         cat("\n")
      }
      if (toupper(key) == "G"){
         snap.to.grid <<- !snap.to.grid
         if (snap.to.grid) cat("   Snap-to-grid is ON.\n") else cat("   Snap-to-grid is OFF.\n")
      }
      if (toupper(key) == "L"){
         if (length(x) >= 2){
            snap.to.line <<- !snap.to.line
            snap <<- FALSE
            snap.to.grid <<- FALSE
            lines(x, y, ...)
            if (snap.to.line) cat("   Snap-to-line is ON.\n") else cat("   Snap-to-line is OFF.\n")
         }else{
            cat("   Snap-to-line requires that 'x' and 'y' coordinates be specified.\n")
         }
      }
      if (toupper(key) == "S"){
         snap <<- !snap
         if (snap) cat("   Snap is ON.\n") else cat("   Snap is OFF.\n")
      }
      if (toupper(key) == "T"){
         tolerance.flag <<- !tolerance.flag
         if (tolerance.flag) cat("   Tolerance is ON.\n") else cat("   Tolerance is OFF.\n")
      }
      if (key == "ctrl-[") return(data.frame(x = xx, y = yy))

      return(NULL)
   }

   # Mouse plotting function:
   plot.point <- function(buttons, x0, y0){
      if (any(1:2 %in% buttons)) return(4)

      # Convert to user coordinates:
      x0 <- grconvertX(x0, "ndc", "user")
      y0 <- grconvertY(y0, "ndc", "user")

      # Add coordinates:
      xx <<- c(xx, x0)
      yy <<- c(yy, y0)

      # Snap last coordinate to neighbouring point:
      if (snap & (length(xx) >= 2)){
         d <- sqrt((xx[length(xx)] - c(x, xx[1:(length(xx)-1)]))^2 +
                   (yy[length(yy)] - c(y, yy[1:(length(yy)-1)]))^2)
         diag <- sqrt((par("usr")[2] - par("usr")[1])^2 + (par("usr")[4] - par("usr")[3])^2)
         index <- which(d == min(d, na.rm = TRUE))[1]
         if ((d[index] < (diag * tol)) | !tolerance.flag){
            xx[length(xx)] <<- c(x, xx[1:(length(xx)-1)])[index]
            yy[length(yy)] <<- c(y, yy[1:(length(yy)-1)])[index]
            cat("Point was snapped to nearest adjacent point.\n")
         }
      }

      # Snap point to line:
      if (snap.to.line & ((length(x) > 2) | (length(xx) > 2))){
         res <- nearest.point(xx[length(xx)], yy[length(yy)],
                              c(x, NA, xx[1:(length(xx)-1)]), c(y, NA, yy[1:(length(yy)-1)]))
         if (!is.na(res$x)){
            xx[length(xx)] <<- res$x
            yy[length(xx)] <<- res$y
         }
      }

      # Snap point to grid:
      if (snap.to.grid){
         xx[length(xx)] <<- round((xx[length(xx)] + grid.offset) / grid.increment) * grid.increment - grid.offset
         yy[length(yy)] <<- round((yy[length(xx)] + grid.offset) / grid.increment) * grid.increment - grid.offset
      }

      # Remove last point if it was identical to the previous point:
      if (length(xx) >= 2){
         if ((xx[length(xx)] == xx[length(xx)-1]) & (yy[length(xx)] == yy[length(xx)-1])){
            xx <<- xx[-length(xx)]
            yy <<- yy[-length(yy)]
         }
      }

      # Draw points:
      points(xx[length(xx)], yy[length(yy)], pch = pch, bg = bg, ...)

      # Check if length of vector exceeds 'n':
      if (!is.null(n)) if (length(xx) >= n) return(data.frame(x = xx, y = yy))

      return(NULL)
   }

   cat("Press 'm' for menu options.\n")

   getGraphicsEvent(prompt = "Click mouse in plot to draw points.",
                    onMouseDown = plot.point,
                    onKeybd = keyboard.fun)

   return(data.frame(x = xx, y = yy))
}


