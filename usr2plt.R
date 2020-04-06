usr2plt <- function(x = NULL, y = NULL){
   # USR2PLT - Converts from graphical 'usr' to 'plt' coordinates.

   # Flag whether to return value as a data frame.
   flag <- TRUE

   # Convert 'x' data frame to vector:
   if (is.data.frame(x)){
      y <- x[, 2]
      x <- x[, 1]
   }

   # Extract 'usr' graphical coordinates:
   if (is.null(x)){
      x <- par("usr")[1:2]
      y <- par("usr")[3:4]
      flag <- FALSE
   }

   # Parse single 'x' argument:
   if (is.null(y)){
      if (length(x) != 4) stop("Unable to parse input argument 'x'.")
      y <- x[3:4]
      x <- x[1:2]
      flag <- FALSE
   }

   # Check input arguments:
   if (length(x) != length(y)) stop("Input arguments have inconsistent lengths.")

   # Convert coordinates to plot inches:
   px <- ((x - par("usr")[1])/diff(par("usr")[1:2]))* par("pin")[1]
   py <- ((y - par("usr")[3])/diff(par("usr")[3:4]))* par("pin")[2]

   # Convert 'plt' coordinates figure inches:
   fx <- px + par("fin")[1]*par("plt")[1]
   fy <- py + par("fin")[2]*par("plt")[3]
   
   # Convert to figure relative coordinates:
   lx <- fx / par("fin")[1]
   ly <- fy / par("fin")[2]

   # Return results:
   if (flag){
      return(data.frame(x = lx, y = ly))
   }else{
      return(c(lx, ly))
   }
}
