plt2pin <- function(x = NULL, y = NULL){
   # PLT2PIN - Converts from graphical 'plt' to plot inches ('pin').

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

   # Return results:
   if (flag){
      return(data.frame(x = px, y = py))
   }else{
      return(c(px, py))
   }
}
