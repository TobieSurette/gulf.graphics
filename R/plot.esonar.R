#' Plot \strong{eSonar} Data
#' 
#' @description Graphically display \code{esonar} data.
#' 
#' @param x \code{esonar} data object.
#' 
#' @examples 
#' plot.esonar(year = 2020, tow.id = "GP354F")
#' 
#' x <- read.esonar(year = 2020, tow.id = "GP354F")
#' plot(x)
#' 
#' @export plot.esonar
#' @export
plot.esonar <- function(x, ...){
   # Define time series in minutes:
   t <- time2min(time(x), start.time(x))
   x$wingspread <- wingspread(x)
   
   # Determine variables to be plotted:
   remove <- c("validity", "transducer", "sensor", "value", "error.code", "hydrophone", "signal.strength", "doormaster")
   vars <- setdiff(names(x), c(key(x), "longitude", "latitude", remove))
   
   # Prepare graph layout:
   clg()
   m <- rbind(t(kronecker(1:2, matrix(1, nrow = 5, ncol = 5))),
              0,
              kronecker(3:(3+length(vars)-1), matrix(1, nrow = 5, ncol = 10)))
   m <- rbind(0, cbind(0, m, 0), 0, 0)
   dev.new(height = 11, width = 8.5)
   layout(m)
   par(mar = c(0, 0, 0, 0))

   # Plot information:
   plot(c(0, 1), c(0, 1), type = "n", xaxt = "n", yaxt = "n")
   str <- header(x)
   names(str) <- names(header(x))
   str <- c(date = as.character(unique(gulf.utils::date(x))), tow.id = tow.id(x), str)
   str <- paste0(formatC(names(str), width = max(nchar(names(str)))), " : ",
                 str, " \n")
   text(0.3, 0.9, str, pos = 1)         

   # Plot coordinates relative to start time:
   xy <- 1000 * gulf.spatial::deg2km(lon(x), lat(x))
   x$xm <- xy$x; x$ym <- xy$y
   index <- which.min(abs(t))
   x$xm <- x$xm - x$xm[index]
   x$ym <- x$ym - x$ym[index]
   xlim <- range(x$xm); ylim <- range(x$ym)
   if (diff(xlim) < diff(ylim)) xlim[2] <- xlim[1] + diff(ylim) else ylim[2] <- ylim[1] + diff(xlim)
   plot(x$xm, x$ym, xlim = xlim, ylim = ylim, pch = 21, bg = "grey", cex = 0.75, xlab = "", ylab = "")
   grid()
   mtext("x(m)", 1, 1.5, cex = 1)
   mtext("y(m)", 2, 1.5, cex = 1)
   
   # Pre-defined axis limits:
   ylim <- list(wingspread = c(0, 15), speed = c(0, 5),  headline = c(0, 10), depth = c(5 * (floor(max(x$depth, na.rm = TRUE) / 5) + 1), 0))
   tmp <- setdiff(vars, names(ylim))
   if (length(tmp) > 0) {
      for (i in 1:length(tmp)){
         ylim[[tmp[i]]] <- range(x[, tmp[i]], na.rm = TRUE)
      }
   } 
   
   # Plot time series:
   for (i in 1:length(vars)){
      plot(range(t), ylim[[vars[i]]], type = "n", xaxt = "n", xaxs = "i", yaxs = "i")
      grid()
      if (vars[i] %in% c("speed", "heading")){ 
         lines(t, x[, vars[i]], lwd = 2, col = "blue")
      }else{
         points(t, x[, vars[i]], pch = 21, bg = "blue")
      }
      
      # Y axis labels:
      ylab <- paste0(toupper(substr(vars[i], 1, 1)), substr(vars[i], 2, nchar(vars[i])))
   
      if (vars[i] == "wingspread") u <- "meters" else u <- gulf.metadata::units(x)[vars[i]]
      u <- gsub("meters", "m", u)
      u <- gsub("degrees", "°", u)
      u <- gsub("knots", "kts", u)
      
      if (length(u) > 0) ylab <- paste0(ylab, "(", u, ")")
      mtext(ylab, 2, 2.5, cex = 0.8)
   }
   
   mtext("Time(min)", 1, 2.5, cex = 0.8)
   axis(1)
}
