draw.polygon <- function(x = NULL, y = NULL, n, tol = 5E-3, snap = TRUE,
                         snap.to.grid = FALSE, grid.increment, grid.offset = 0,
                         pch = 21, bg = "black", ...){
   # DRAW.POINTS - Draw a points interactively on a plot.
   
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

   if (missing(n)) n <- NULL

   # Initialize point vectors to be returned:
   xx <- NULL
   yy <- NULL

   # Packaging function for the polygon:
   package <- function(x, y){
      if ((x[length(x)] != x[1]) | (y[length(x)] != y[1])){
         x <- c(x, x[1])
         y <- c(y, y[1])
      }
      p <- as.polygon(x, y)
      plot.polygon(p)
      return(p)
   }
   
   # Keyboard button function:
   keyboard.fun <- function(key){
      if (toupper(key) == "M"){
         cat("\n")
         cat("'g'   : Toggle snap-to-grid option.\n")
         cat("'m'   : Display menu options.\n")
         cat("'l'   : Snap to nearest point along a line.\n")
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
         index <- which(d == min(d))[1]
         if ((d[index] < (diag * tol)) | !tolerance.flag){
            xx[length(xx)] <<- c(x, xx[1:(length(xx)-1)])[index]
            yy[length(yy)] <<- c(y, yy[1:(length(yy)-1)])[index]
            cat("Point was snapped to nearest adjacent point.\n")
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
      
      points(xx[length(xx)], yy[length(yy)], pch = pch, bg = bg, ...)
      if (length(xx) >= 2) lines(c(xx[length(xx)-1],  xx[length(xx)]), c(yy[length(yy)-1],  yy[length(yy)]), ...)
      
      # Stopping case if polygon is closed:
      if ((length(xx) > 1) & (xx[length(xx)] == xx[1]) & (yy[length(xx)] == yy[1])) return(package(xx, yy))
      
      if (!is.null(n)){
         if (length(xx) >= n){
            return(package(xx, yy))
         }
      }
      
      return(NULL)
   }

   cat("Press 'm' for menu options.\n")

   getGraphicsEvent(prompt = "Click mouse in plot to draw points.",
                    onMouseDown = plot.point,
                    onKeybd = keyboard.fun)

   return(package(xx, yy))
}
