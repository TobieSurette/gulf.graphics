#' Plot Snow Crab Trawl Data
#'
#' @description Displays auxiliary trawl data from the snow crab trawl survey.
#'
#' @param x \code{scsset} data object.
#' @param tow.id Character string specifying a tow identifier.
#' @param year Survey year
#'
#' @examples
#' plot.scsset(year = 2020, tow.id = "GP354F")
#'
#' x <- read.scsset(year = 2020)
#' plot(x[1, ])


#' @export plot.scsset
#' @export
plot.scsset <- function(x, tow.id, year, ...){
   # 1 - Photo and maps (compare vessel and trawl footrope probe heading)
   #2 - Vessel speed
   #3 - Depth profiles
   #4 - Wingspread profiles
   #5 - Headline height
   #6 - Temperature profiles
   #7 - Tilt profiles
   #8 - Compass heading, vessel heading, probe inclination
   #9 - Accelerometer profiles
   
   # Load tow data:
   if (missing(x)){
      if (missing(tow.id) | missing(year)) stop("'tow.id' and 'year' must be specified.")
      x <- read.scsset(year = 2020, tow.id = tow.id)
   }

   # Date labeling function:
   suffix <- function(n){
      suffixes <- c("th", "st", "nd", "rd", "th", "th", "th", "th", "th", "th")
      if (n %in% 11:20) return("th")
      if (n %in% c(1:10, 21:31)) return(suffixes[(n %% 10) + 1])
      return("")
   }

   if (nrow(x) == 1){
      # Define year:
      year <- unique(gulf.utils::year(x))

      # Read probe data:
      m <- NULL
      e <- read.esonar(x)
      s <- read.star.oddi(x, probe = "headline", project = "scs")
      t <- read.star.oddi(x, probe = "footrope", project = "scs")

      # Define reference time:
      reftime <- time(x, "touchdown")

      # Define set of event times:
      events <- c(start = gulf.utils::time(x, "start"), 
                  stop = gulf.utils::time(x, "end"),
                  haul = gulf.utils::time(x, "haul"),
                  touchdown = gulf.utils::time(x, "touchdown"),
                  liftoff = gulf.utils::time(x, "liftoff"))
      events <- gulf.utils::time2min(events, reftime)

      # Define time axes range:
      dt <- diff(range(events[c("start", "liftoff")]))
      xlim <- c(events[["start"]] - 0.2 * dt, events[["liftoff"]] + 0.2 * dt)

      # Define time in minutes from reference start time:
      if (!is.null(m)) m$time <- gulf.utils::time2min(time(m), reftime)
      if (!is.null(e)) e$time <- gulf.utils::time2min(gulf.utils::time(e), reftime)
      if (!is.null(s)) s$time <- gulf.utils::time2min(gulf.utils::time(s), reftime)
      if (!is.null(t)) t$time <- gulf.utils::time2min(gulf.utils::time(t), reftime)

      # Convert pressures to depth:
      index <- which(!is.na(e$depth) & e$time >= 0 & e$time <= events["stop"])
      if (!is.null(s)) s$depth <- s$pressure * mean(e$depth[index] / approx(s$time, s$pressure, e$time[index])$y)
      if (!is.null(t)) t$depth <- t$pressure * mean(e$depth[index] / approx(t$time, t$pressure, e$time[index])$y)

      # Define graphical layout:
      L <- rbind(t(kronecker(1:3, matrix(1, nrow = 5, ncol = 8))), 0,
                 kronecker(4:9, matrix(1, nrow = 4, ncol = 15)))
      L <- rbind(0, 0, cbind(0, 0, L, 0), 0, 0)
            # Create new graphics device:
      
      dev.new(width = 8.5, height = 11)
      layout(L)

      # Display catch photo:
      photo <- gulf.utils::locate(keywords = c(year, "photos"), pattern = paste0(x$tow.id, ".jpg"))
      if (length(photo) == 1){
         photo <- jpeg::readJPEG(photo, native = FALSE)
         par(mar = c(0, 0, 0, 0))
         photo <- photo[seq(1, nrow(photo), by = 10), seq(1, ncol(photo), by = 10), ]  # Thin-out image.
         plot(c(0, 1), c(0, 1), type = "n", xlab = "", xaxt = "n", ylab = "", yaxt = "n", xaxs = "i", yaxs = "i")
         tmp <- rep(NA, length(photo))
         dim(tmp) <- dim(photo)[c(2,1,3)]
         for (i in 1:3) tmp[,,i] <- t(photo[,,i])
         photo <- tmp
         w <- dim(photo)[2] / dim(photo)[1]
         graphics::rasterImage(photo, 0 + (1-w)/2, 0,  1- (1-w)/2, 1)
      }else{
         plot(c(0, 1), c(0, 1), type = "n", xaxt = "n", yaxt = "n")
      }

      # Display tow location on sGSL map:
      plot(c(0, 1), c(0, 1), type = "n", xaxt = "n", yaxt = "n")

      # Display plot title:
      title <- paste0(month.name[as.numeric(substr(x$date, 6, 7))], " ")
      day <- as.numeric(substr(x$date, 9, 10))
      title <- paste0(title, day, suffix(day), ", ", year, ": ")
      title <- paste0(title, "Tow #", x$tow.number)
      title <- paste0(title, " (station = ", x$tow.id, ")")
      mtext(title, 3, 1.5, at = par("usr")[1] + 0.5*diff(par("usr")[1:2]), cex = 1.5, font = 2)

      if (!is.null(e)){
         # Vessel track:
         xy <- 1000 * gulf.spatial::deg2km(gulf.spatial::lon(e), gulf.spatial::lat(e))
         xy[,1] <- xy[,1] - xy[which(e$time == 0),1]
         xy[,2] <- xy[,2] - xy[which(e$time == 0),2]
         index <- (e$time >= events["touchdown"]) & (e$time <= events["liftoff"])
         dx <- range(xy[index,1])
         dy <- range(xy[index,2])
         if (diff(dx) > diff(dy)){
            dy[2] <- dy[1] + diff(dx)
         }else{
            dx[2] <- dx[1] + diff(dy)
         } 
         dx <- c(dx[1] - 0.1*diff(dx), dx[2] + 0.1*diff(dx))
         dy <- c(dy[1] - 0.1*diff(dy), dy[2] + 0.1*diff(dy))
         par(mar = c(2,1,1,1))
         plot(dx, dy, type = "n", xaxs = "i", yaxs = "i")
         index <- (e$time >= events["touchdown"]) & (e$time <= events["stop"])
         lines(xy[index, 1], xy[index, 2], col = "grey", lwd = 2)
         index <- (e$time >= events["stop"]) & (e$time <= events["liftoff"])
         lines(xy[index, 1], xy[index, 2], col = "red", lwd = 2)

         par(mar = c(0,0,0,0))
         
         # Vessel speed:
         plot(xlim, c(0, 4), type = "n", xaxs = "i", yaxs = "i", xlab = "", ylab = "", xaxt = "n")
         grid()
         lines(e$time, e$speed, lwd = 2, col = "grey")
         gulf.graphics::vline(events)
         mtext("Speed (knots)", 2, 2.25, cex  = 0.8)
         box()

         # Wing spread:
         plot(xlim, c(0, 16), type = "n", xaxs = "i", yaxs = "i", xlab = "", ylab = "", xaxt = "n")
         grid()
         points(e$time, wingspread(e), pch = 21, bg = "grey")
         gulf.graphics::vline(events)
         mtext("Wing spread (m)", 2, 2.25, cex  = 0.8)
         box()

         # Headline height:
         plot(xlim, c(0, 6), type = "n", xaxs = "i", yaxs = "i", xlab = "", ylab = "", xaxt = "n")
         points(e$time, e$headline, pch = 21, bg = "grey")
         grid()
         gulf.graphics::vline(events)
         mtext("Headline height (m)", 2, 2.25, cex  = 0.8)
         box()

      }
            
      # Depth profiles:
      ylim <- -(floor(1.05 * gulf.spatial::depth(gulf.spatial::lon(x), gulf.spatial::lat(x)) / 5) + 1) * 5
      ylim <- c(ylim + 50, ylim)
      plot(xlim, ylim, type = "n", xaxs = "i", yaxs = "i", xlab = "", ylab = "", xaxt = "n")
      grid()
      lines(s$time, -s$depth, lwd = 2, col = "red")
      lines(t$time, -t$depth, lwd = 2, col = "blue")
      points(e$time, -e$depth, pch = 21, bg = "green")
      gulf.graphics::vline(events)
      mtext("Depth (m)", 2, 2.25, cex  = 0.8)
      box()

      # Temperature profiles:
      plot(xlim, c(-1, 4), type = "n", xaxs = "i", yaxs = "i", xlab = "", ylab = "", xaxt = "n")
      grid()
      lines(s$time, s$temperature, lwd = 2, col = "red")
      lines(t$time, t$temperature, lwd = 2, col = "blue")
      gulf.graphics::vline(events) 
      mtext("Temperature(ºC)", 2, 2.25, cex  = 0.8)
      box()

      # Tilt angle plot:
      index <- t$time >= events["start"] & t$time <= events["stop"]
      #t$"tilt-x" <- (t$"tilt-x" - mean(t$"tilt-x"[index], na.rm = TRUE)) / sd(t$"tilt-x"[index], na.rm = TRUE)
      #t$"tilt-y" <- (t$"tilt-y" - mean(t$"tilt-y"[index], na.rm = TRUE)) / sd(t$"tilt-y"[index], na.rm = TRUE)
      #t$"tilt-z" <- (t$"tilt-z" - mean(t$"tilt-z"[index], na.rm = TRUE)) / sd(t$"tilt-z"[index], na.rm = TRUE)
      ylim <- range(t[index, c("tilt-x", "tilt-y", "tilt-z")], na.rm = TRUE)
      plot(xlim, ylim, type = "n", xaxs = "i", yaxs = "i", xlab = "", ylab = "", xaxt = "n")
      grid()
      lines(t$time, t$"tilt-x", col = "blue", lwd = 1.5)
      lines(t$time, t$"tilt-y", col = "red", lwd = 1.5)
      lines(t$time, t$"tilt-z", col = "green", lwd = 1.5)
      gulf.graphics::vline(events) 
      mtext("Tilt (º)", 2, 2.25, cex  = 0.8)
      box()
            
      legend("topright",
             legend = c("Tilt X", "Tilt Y", "Tilt Z"),
             col = c("blue", "red", "green"), lwd = 1.5,
             bg = "white", cex = 0.8)
      
      axis(1)
      mtext("Time(min)", 1, 2, cex = 1.5)
   }
}
