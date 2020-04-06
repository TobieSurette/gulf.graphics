plt2usr <- function(x = NULL, y = NULL){
   # PLT2USR - Converts from graphical 'plt' to 'usr' coordinates.

   # Flag whether to return value as a data frame.
   flag <- TRUE

   # Convert 'x' data frame to vector:
   if (is.data.frame(x)){
      y <- x[, 2]
      x <- x[, 1]
   }

   # Extract 'usr' graphical coordinates:
   if (is.null(x)){
      x <- par("plt")[1:2]
      y <- par("plt")[3:4]
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

   # Convert 'plt' coordinates figure inches:
   fx <- x * par("fin")[1]
   fy <- y * par("fin")[2]

   # Convert to figure inches to plot inches:
   px <- fx - par("mai")[2]
   py <- fy - par("mai")[1]

   # Convert plot inches to user coordinates:
   ux <- (px / par("pin")[1]) * diff(par("usr")[1:2]) + par("usr")[1]
   uy <- (py / par("pin")[2]) * diff(par("usr")[3:4]) + par("usr")[3]
   
   # Return results:
   if (flag){
      return(data.frame(x = ux, y = uy))
   }else{
      return(c(ux, uy))
   }
}
