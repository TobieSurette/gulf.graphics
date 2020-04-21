inset <- function(x = NULL, y = NULL, width = NULL, height = NULL,
                  margins = 0,
                  units = "user",
                  reference.frame = "plot",
                  plot){
   # INSET - Set up an inset plot.
   
   # Parse 'units' argument:
   units <- tolower(units)
   units <- match.arg(units, c("usr", "user", "plot", "inches", "relative"))
   if (units %in% c("usr", "user", "plot")) units <- "user"

   # Parse 'reference.frame' argument:
   reference.frame <- tolower(reference.frame)
   reference.frame <- match.arg(reference.frame, c("device", "plot", "figure"))
   
   # Parse 'x' argument:
   if (is.character(x)){
      x <- tolower(x)
      x <- gsub("lower", "bottom", x)
      x <- gsub("upper", "top", x)
      x <- gsub("middle", "center", x)
      x <- gsub(".", "", x, fixed = TRUE)
      x <- gsub("-", "", x, fixed = TRUE)
      x <- gsub(" ", "", x, fixed = TRUE)
      if (x == "center") x <- "centercenter"
      x <- match.arg(x, c("bottomright", "bottomleft", "topleft", "topright",
                          "centerright", "centerleft", "bottomcenter", "topcenter",
                          "centercenter"))
      x <- gsub("bottom", "bottom.", x, fixed = TRUE)
      x <- gsub("top", "top.", x, fixed = TRUE)
      x <- gsub("center", "center.", x, fixed = TRUE)
      if (substr(x, nchar(x), nchar(x)) == ".") x <- substr(x, 1, nchar(x) -1)
      x <- unlist(strsplit(x, ".", fixed = TRUE))
   }else{
      if (!is.numeric(x) | !is.null(dim(x)))
         stop("'x' must be either numeric vector or character string.")
      if (!is.null(y)){
         if ((length(y) != length(x)) | !(length(y) %in% c(1, 2)))
            stop("'x' and 'y' must be either numeric scalars or two-element vectors.")
      }
      if ((length(x) == 2) & is.null(y)){
         y <- x[2]
         x <- x[1]
      }
      if ((length(x) == 4) & is.null(y)){
         y <- x[3:4]
         x <- x[1:2]
      }
      if (is.null(y))
         stop("'x' and 'y' must be either numeric scalars or two-element vectors.")
      if ((length(x) == 1) & (is.null(width) | is.null(height)))
         stop("If only a single corner of the plot is specified, then 'width' and 'height' must be specified.")
      if (length(x) == 1){
         x <- c(x, x + width)
         y <- c(y, y + height)
         width = NULL
         height = NULL
      }
      if ((x[1] >= x[2])|(y[1] >= y[2]))
         stop("Plot corner coordinates are inconsistent.")
   }

   # Convert 'x' from character string to numeric:
   if (is.character(x)){
      # Check 'width' and 'height' arguments:
      if (is.null(width) | is.null(height))
         stop("'width' and 'height' must be specified for character string 'x'.")
      if ((length(width) != 1) | (length(height) != 1) | (!is.numeric(width)) | (!is.numeric(height)))
         stop("'width' and 'height' must be numeric scalars.")

      # Check 'margins' argument:
      if (!is.numeric(margins) | !(length(margins) %in% c(1, 2)))
         stop("'margins' must be a numeric scalar or two-element vector.")
      if (length(margins) == 1) margins <- c(margins, margins)

      # Convert 'width' and 'height' to figure inches:
      temp <- switch(units,
                     user     = plt2fin(usr2plt(width, height)),
                     inches   = c(width, height),
                     relative = plt2fin(width, height))
      width <- temp[1]
      height <- temp[2]

      # Convert margin values to inches:
      margins <- switch(units,
                        user     = plt2fin(usr2plt(margins[1], margins[2])),
                        inches   = margins,
                        relative = plt2fin(margins[1], margins[2]))
      
      # Initialize reference coordinte variable:
      ref <- c(NA, NA)
      
      # Calculate horizontal reference coordinate in figure inches:
      ref[1] <- switch(reference.frame,
                       plot = switch(x[2],
                                     left   = plt2fin()[1] + margins[1],
                                     right  = plt2fin()[2] - width - margins[1],
                                     center = plt2fin()[1] + (diff(plt2fin()[1:2])-width) / 2),
                       figure = switch(x[2],
                                       left   = margins[1],
                                       right  = par("fin")[1] - width - margins[1],
                                       center = (par("fin")[1] - width)/2))
                                     
      # Calculate vertical reference coordinate in inches:
      ref[2] <- switch(reference.frame,
                       plot = switch(x[1],
                                     top    = plt2fin()[4] - height - margins[2],
                                     bottom = plt2fin()[3] + margins[2],
                                     center = plt2fin()[3] + (diff(plt2fin()[3:4])-height) / 2),
                       figure = switch(x[1],
                                       top    = par("fin")[2] - height - margins[2],
                                       bottom = margins[2],
                                       center = (par("fin")[2]-height)/2))
                                     
      # Convert figure inches to relative figure coordinates:
      plt <- c(ref[1], ref[1]+width, ref[2], ref[2]+height)
      plt[1:2] <- plt[1:2] / par("fin")[1]
      plt[3:4] <- plt[3:4] / par("fin")[2]

      # Convert result to units:
      res <- switch(units,
                    user = plt2usr(plt),
                    inches = switch(reference.frame,
                                    plot   = plt2pin(plt),
                                    figure = plt2fin(plt)),
                    relative = plt)

      # Execute code in 'plot':
      if (!missing(plot)){
         # Save old graphical parameters:
         p <- par(no.readonly = TRUE)

         print(plt)
         print(plt2pin(plt))
         # Set new plot layout:
         par(plt = plt, new = TRUE)

         # Evaluate plot code:
         eval(substitute(plot, parent.frame()))

         # Restore graphical parameters:
         par(p)
      }else{
         par(plt = plt, new = TRUE)
      }
      
      return(invisible(res))
   }

   # Convert 'x' from character string to numeric:
   if (is.numeric(x)){
      plt <- switch(units,
                    user = usr2plt(c(x, y)),
                    inches = switch(reference.frame,
                                    plot = pin2plt(c(x, y)),
                                    figure = fin2plt(c(x, y))),
                    relative = c(x, y))

      # Update 'plt' graphical parameter:
      par(plt = plt, new = TRUE)
      
      return(invisible(c(x, y)))
   }
}
