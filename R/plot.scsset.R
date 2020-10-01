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
   # Load tow data:
   if (missing(x)){
      if (missing(tow.id) | missing(year)) stop("'tow.id' and 'year' must be specified.")
      x <- read.scsset(year = 2020, tow.id = tow.id)
   }

   # Date labelling function:
   suffix <- function(n){
      suffixes <- c("th", "st", "nd", "rd", "th", "th", "th", "th", "th", "th")
      if (n %in% 11:20) return("th")
      if (n %in% c(1:10, 21:31)) return(suffixes[(n %% 10) + 1])
      return("")
   }

   if (nrow(x) == 1){
      # Create new graphics device:
      dev.new(width = 8.5, height = 11)

      # Define year:
      year <- unique(year(x))

      # Read probe data:
      m <- NULL
      e <- read.esonar(x)
      s <- read.star.oddi(x, probe = "headline")
      t <- read.star.oddi(x, probe = "footrope")

      # Define reference time:
      reftime <- start.time(x)

      # Define set of event times:
      events <- c(0, gulf.utils::time2min(end.time(x), start.time(x)))
      names(events) <- c("start", "stop")

      # Define time axes range:
      xlim <- c(min(events) - 0.2 * diff(range(events)), max(events) + 0.2 * diff(range(events)))

      # Define time in minutes from reference start time:
      if (!is.null(m)) m$time <- gulf.utils::time2min(time(m), reftime)
      if (!is.null(e)) e$time <- gulf.utils::time2min(time(e), reftime)
      if (!is.null(s)) s$time <- gulf.utils::time2min(time(s), reftime)
      if (!is.null(t)) t$time <- gulf.utils::time2min(time(t), reftime)

      # Convert pressures to depth:
      index <- which(!is.na(e$depth) & e$time >= events["start"] & e$time <= events["stop"])
      if (!is.null(s)) s$depth <- s$pressure * mean(e$depth[index] / approx(s$time, s$pressure, e$time[index])$y)
      if (!is.null(t)) t$depth <- t$pressure * mean(e$depth[index] / approx(t$time, t$pressure, e$time[index])$y)

      # Define graphical layout:
      M <- rbind(t(kronecker(1:3, matrix(1, nrow = 5, ncol = 5))),
                 0,
                 kronecker(4:7, matrix(1, nrow = 5, ncol = 15)))
      M <- rbind(0, cbind(0, 0, M, 0), 0, 0)
      layout(M)

      # Display catch photo:
      photo <- gulf.utils::locate(keywords = c(year, "photos"), pattern = paste0(x$tow.id, ".jpg"))
      if (length(photo) == 1){
         photo <- jpeg::readJPEG(photo, native = FALSE)
         par(mar = c(0, 0, 0, 0))
         photo <- photo[seq(1, nrow(photo), by = 10), seq(1, ncol(photo), by = 10), ]  # Thin-out image.
         plot(c(0, 1), c(0, 1), type = "n", xlab = "", xaxt = "n", ylab = "", yaxt = "n", xaxs = "i", yaxs = "i")
         rasterImage(photo, 0, 0, 1, 1)
         box()
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

      # Display vessel track:
      index <- (e$time >= events["start"]) & (e$time <= events["stop"])
      xy <- 1000 * deg2km(lon(e)[index], lat(e)[index])
      xy[,1] <- xy[,1] - xy[1,1]
      xy[,2] <- xy[,2] - xy[1,2]
      plot(xy, pch = 21, type = "l", col = "grey", lwd = 2)
      grid()

      # Vessel speed:
      plot(e$time, e$speed, xlim = xlim, ylim = c(0, 4), type = "l", lwd = 2, xaxs = "i", yaxs = "i")
      grid()
      #gulf.graphics::hline(events, label = TRUE, col = , lty = )
      mtext("Speed (knots)", 2, 2.25)
      box()

      # Wing spread:
      plot(e$time, wingspread(e), xlim = xlim, ylim = c(0, 16), pch = 21, bg = "grey", xaxs = "i", yaxs = "i")
      grid()
      #gulf.graphics::hline(events, label = TRUE, col = , lty = )
      mtext("Wing spread (m)", 2, 2.25)
      box()

      # Headline height:
      plot(e$time, e$headline, xlim = xlim, ylim = c(0, 16), pch = 21, bg = "grey", xaxs = "i", yaxs = "i")
      grid()
      #gulf.graphics::hline(events, label = TRUE, col = , lty = )
      mtext("Headline height (m)", 2, 2.25)
      box()

      # Depth profiles:
      ylim <- c(0, -(floor(1.2 * depth(lon(x), lat(x)) / 5) + 1) * 5)
      plot(xlim, ylim, type = "n", xaxs = "i", yaxs = "i", yaxt = "n")
      grid()
      lines(s$time, -s$depth, lwd = 2, col = "red")
      lines(t$time, -t$depth, lwd = 2, col = "blue")
      points(e$time, -e$depth, pch = 21, bg = "green")
      #gulf.graphics::hline(events, label = TRUE, col = , lty = )
      mtext("Depth (m)", 2, 2.25)
      box()

      # Temperature profiles:
      plot(xlim, c(-1, 10), type = "n", xaxs = "i", yaxs = "i", yaxt = "n")
      grid()
      lines(s$time, s$temperature, lwd = 2, col = "red")
      lines(t$time, t$temperature, lwd = 2, col = "blue")
      #gulf.graphics::hline(events, label = TRUE, col = , lty = )
      mtext("Temperature(C)", 2, 2.25)
      box()

      # Tilt angle plot:
      index <- t$time >= events["start"] & t$time <= events["stop"]
      t$"tilt-x" <- (t$"tilt-x" - mean(t$"tilt-x"[index], na.rm = TRUE)) / sd(t$"tilt-x"[index], na.rm = TRUE)
      t$"tilt-y" <- (t$"tilt-y" - mean(t$"tilt-y"[index], na.rm = TRUE)) / sd(t$"tilt-y"[index], na.rm = TRUE)
      t$"tilt-z" <- (t$"tilt-z" - mean(t$"tilt-z"[index], na.rm = TRUE)) / sd(t$"tilt-z"[index], na.rm = TRUE)
      plot(xlim, range(t[index, c("tilt-x", "tilt-y", "tilt-z")], na.rm = TRUE), type = "n", col = "blue")
      grid()
      mtext("Time (min)", 1, 2.5)
      mtext("Normalized Angle", 2, 2.25)
      lines(t$time, t$"tilt-x", col = "blue", lwd = 1.5)
      lines(t$time, t$"tilt-y", col = "red", lwd = 1.5)
      lines(t$time, t$"tilt-z", col = "green", lwd = 1.5)

      legend("bottomleft",
             legend = c("Tilt X", "Tilt Y", "Tilt Z"),
             col = c("blue", "red", "green"), lwd = 1.5,
             bg = "white", cex = 0.8)
  # 3-plot
        # Photo
        # Map
        # Track

      # eSonar:
         # Speed
         # Deppth -
         # Headline
         # Wingspread
         # Doorspread

      # Star Oddi
         # headline - pressure
         # headline - temperature
         # footrope - temperature
         # footrope - pressure
         # footrope - tilt-x, tilt-y, tilt-z
         # footrope - comp.head
         # footrope - inclination



  # 1 - Photo and maps (compare vessel and trawl footrope probe heading)
   #2 - Vessel speed
   #3 - Depth profiles
   #4 - Wingspread profiles
   #5 - Headline height
   #6 - Temperature profiles
   #7 - Tilt profiles
   #8 - Compass heading, vessel heading, probe inclination
   #9 - Accelerometer profiles
   }
}
