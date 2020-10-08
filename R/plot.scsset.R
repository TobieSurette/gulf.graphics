#' Plot Snow Crab Trawl Data
#'
#' @description Displays auxiliary trawl data from the snow crab trawl survey.
#'
#' @param x \code{scsset} data object.
#' @param tow.id Character string specifying a tow identifier.
#' @param year Survey year.
#' @param pdf Logical values specifying whether to output the figure to a pdf file.
#' @param path Path to which output file is to written.
#'
#' @examples
#' plot.scsset(year = 2020, tow.id = "GP354F")
#'
#' x <- read.scsset(year = 2020)
#' plot(x[1, ])


#' @export plot.scsset
#' @export
plot.scsset <- function(x, tow.id, year, pdf = FALSE, path = getwd(), ...){
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

   # Plot multiple tows:
   if (nrow(x) > 1) for (i in 1:nrow(x)) plot(x[i, ], pdf = pdf, path = path, ...)

   # Plot single tows:
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

      background <- function(xlim, events){
         col <- colorRamp(c("white", "tomato2"))(0.25) / 255
         col <- rgb(col[1,1], col[1,2], col[1,3])
         #graphics::rect(xlim[1], par("usr")[3], events["touchdown"], par("usr")[4], col = "grey60", border = NA)
         graphics::rect(events["touchdown"], par("usr")[3], events["stop"], par("usr")[4], col = "grey93", border = NA)
         graphics::rect(events["stop"], par("usr")[3], events["liftoff"], par("usr")[4], col = col, border = NA)
         #graphics::rect(events["liftoff"], par("usr")[3], xlim[2], par("usr")[4], col = "grey90", border = NA)
         grid()
         gulf.graphics::vline(events)
      }
         
      # Define time in minutes from reference start time:
      if (!is.null(m)) m$time <- gulf.utils::time2min(time(m), reftime)
      if (!is.null(e)) e$time <- gulf.utils::time2min(gulf.utils::time(e), reftime)
      if (!is.null(s)) s$time <- gulf.utils::time2min(gulf.utils::time(s), reftime)
      if (!is.null(t)) t$time <- gulf.utils::time2min(gulf.utils::time(t), reftime)

      # Convert pressures to depth:
      depth <- gulf.spatial::depth(gulf.spatial::lon(x), gulf.spatial::lat(x))
      if (!is.null(e)) e$depth <- depth * e$depth / mean(e$depth[which(e$time >= 0 & (e$time <= events["stop"]))], na.rm = TRUE)  
      if (!is.null(s)) s$depth <- depth * s$pressure / mean(s$pressure[s$time >= 0 & s$time <= events["stop"]]) 
      if (!is.null(t)) t$depth <- depth * t$pressure / mean(t$pressure[t$time >= 0 & t$time <= events["stop"]]) 
      
      # Create new graphics device:
      if (pdf){
         file <- paste0(path, "/", x$tow.id, ".pdf")
         pdf(width = 8.5, height = 11, file = file)
      }else{
         dev.new(width = 8.5, height = 11)
      }
      
      # Define graphical layout:
      L <- rbind(0, t(kronecker(1:3, matrix(1, nrow = 5, ncol = 8))), 0, 0,  
                 kronecker(4:9, matrix(1, nrow = 4, ncol = 15)))
      L <- rbind(0, 0, cbind(0, 0, L, 0), 0, 0, 0)      
      layout(L)

      # Display catch photo:
      photo <- gulf.utils::locate(keywords = c(year, "photos"), pattern = paste0(x$tow.id, ".jpg"))
      if (length(photo) == 1){
         photo <- jpeg::readJPEG(photo, native = FALSE)
         par(mar = c(0, 0, 0, 0))
         photo <- photo[seq(1, nrow(photo), by = 10), seq(1, ncol(photo), by = 10), ]  # Thin-out image.
         plot(c(0, 1), c(0, 1), type = "n", xlab = "", xaxt = "n", ylab = "", yaxt = "n", xaxs = "i", yaxs = "i", axes = F, frame.plot = F)
         tmp <- rep(NA, length(photo))
         dim(tmp) <- dim(photo)[c(2,1,3)]
         for (i in 1:3) tmp[,,i] <- t(photo[,,i])
         photo <- tmp
         w <- dim(photo)[2] / dim(photo)[1]
         graphics::rasterImage(photo, 0 + (1-w)/2, 0,  1- (1-w)/2, 1)
         rect(0 + (1-w)/2, 0,  1- (1-w)/2, 1)
      }else{
         plot(c(0, 1), c(0, 1), type = "n", xaxt = "n", yaxt = "n")
      }

      # Display tow location on sGSL map:
      #plot(c(0, 1), c(0, 1), type = "n", xaxt = "n", yaxt = "n")
      par(mar = c(1,0,0,2))
      map <- read.csv(locate(package = "gulf.spatial", keywords  = "gulf.coast.h"), header = TRUE, stringsAsFactors = FALSE)
      plot(c(-66.5, -60), c(45, 49), type = "n", cex.axis = 0.75, mgp = c(2, 0.5, 0), xaxs = "i", yaxs = "i")
      graphics::polygon(map, col = "grey80")
      points(gulf.spatial::lon(x), gulf.spatial::lat(x), pch = 21, bg = "tomato2", cex = 1.25)
      box()
      
      # Display plot title:
      title <- paste0(month.name[as.numeric(substr(x$date, 6, 7))], " ")
      day <- as.numeric(substr(x$date, 9, 10))
      title <- paste0(title, day, suffix(day), ", ", year, ": ")
      title <- paste0(title, "Tow #", x$tow.number)
      title <- paste0(title, " (station = ", x$tow.id, ")")
      mtext(title, 3, 1.3, at = par("usr")[1] + 0.5*diff(par("usr")[1:2]), cex = 1.25, font = 2)

      if (!is.null(e)){
         # Vessel track:
         xy <- 1000 * gulf.spatial::deg2km(gulf.spatial::lon(e), gulf.spatial::lat(e))
         xy[,1] <- xy[,1] - xy[which(e$time == 0),1]
         xy[,2] <- xy[,2] - xy[which(e$time == 0),2]
         index <- (e$time >= events["touchdown"]) & (e$time <= events["liftoff"])
         dx <- range(xy[index,1])
         dy <- range(xy[index,2])
         if (diff(dx) > diff(dy)){
            w <- diff(dx) - diff(dy)
            dy[1] <- dy[1] - w/2
            dy[2] <- dy[2] + w/2
         }else{
            w <- diff(dy) - diff(dx)
            dx[1] <- dx[1] - w/2
            dx[2] <- dx[2] + w/2
         } 
         dx <- c(dx[1] - 0.1*diff(dx), dx[2] + 0.1*diff(dx))
         dy <- c(dy[1] - 0.1*diff(dy), dy[2] + 0.1*diff(dy))
         par(mar = c(1,1,0,1))
         plot(dx, dy, type = "n", xaxs = "i", yaxs = "i", cex.axis = 0.75, mgp = c(2, 0.5, 0),)
         grid()
         index <- (e$time >= events["touchdown"]) & (e$time <= events["stop"])
         lines(xy[index, 1], xy[index, 2], col = "grey", lwd = 2)
         index <- (e$time >= events["stop"]) & (e$time <= events["liftoff"])
         lines(xy[index, 1], xy[index, 2], col = "tomato2", lwd = 2)
         mtext("x(meters)", 1, 1.5, cex = 0.8)
         mtext("y(meters)", 2, 1.5, cex = 0.8)
         points(0,0,pch = 21, bg = "grey")
         text(0,0,"touchdown")
         index <- which.min(abs(e$time - events["stop"]))
         points(xy[index,1], xy[index,2], pch = 21, bg = "grey")
         #text(xy[index,1], xy[index,2], "stop")
         index <- which.min(abs(e$time - events["liftoff"]))
         points(xy[index,1], xy[index,2], pch = 21, bg = "tomato2")
         
         par(mar = c(0,0,0,0))
         
         # Vessel speed:
         plot(xlim, c(0, 4), type = "n", xaxs = "i", yaxs = "i", xlab = "", ylab = "", xaxt = "n")
         background(xlim, events)
         lines(e$time, e$speed, lwd = 2, col = "grey")
         mtext("Speed(knots)", 2, 2.25, cex  = 0.65)
         box()
   
         # Label trawling phases:
         msg <- c("Descent", "Active Trawling", "Passive", "Ascent")
         at <- c((xlim[1]+events["touchdown"])/2, (events["touchdown"]+ events["stop"])/2,
                 (events["stop"]+ events["liftoff"])/2, (events["liftoff"]+ xlim[2])/2)
         mtext(msg, 3, 0.5, at = at)
            
         # Wing spread:
         plot(xlim, c(0, 14), type = "n", xaxs = "i", yaxs = "i", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
         background(xlim, events)
         points(e$time, wingspread(e), pch = 21, bg = "grey")
         mtext("Wing spread(m)", 2, 2.25, cex  = 0.65)
         axis(2, at = seq(0, 12, by = 2))
         box()

         # Headline height:
         plot(xlim, c(0, 4), type = "n", xaxs = "i", yaxs = "i", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
         background(xlim, events)
         points(e$time, e$headline, pch = 21, bg = "grey")
         mtext("Headline height(m)", 2, 2.25, cex = 0.65)
         axis(2, at = seq(0, 3.5, by = 0.5))
         box()
      }
            
      # Depth profiles:
      ylim <- c(-depth - 5, -depth +25)
      ylim <- c(floor(ylim[1] / 5) * 5, floor((ylim[2] / 5)+1)* 5)
      plot(xlim, ylim, type = "n", xaxs = "i", yaxs = "i", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
      background(xlim, events)
      lines(s$time, -s$depth, lwd = 2, col = "tomato2")
      lines(t$time, -t$depth, lwd = 2, col = "skyblue3")
      points(e$time, -e$depth, pch = 21, bg = "darkolivegreen3")
      mtext("Depth(m)", 2, 2.25, cex  = 0.7)
      at <- pretty(ylim)
      axis(2, at = at[-length(at)])
      box()
      legend("bottomright",
             legend = c("eSonar", "headline", "footrope"), pch = c(21, NA, NA),
             col = c("black", "tomato2", "skyblue3"), lwd = c(NA, 2, 2),
             pt.bg <- c("darkolivegreen3", NA, NA),
             bg = "white", cex = 0.8)
      
      # Temperature profiles:
      index <- (s$time >= events["touchdown"]) & (s$time <= events["liftoff"])
      ylim <- c(min(s$temperature[index], na.rm = TRUE), max(s$temperature[index], na.rm = TRUE))
      ylim <- c(floor(ylim[1] * 4) / 4, (floor(ylim[2] * 4)+1) / 4)
      plot(xlim, ylim, type = "n", xaxs = "i", yaxs = "i", xlab = "", ylab = "", xaxt = "n")
      background(xlim, events)
      lines(s$time, s$temperature, lwd = 2, col = "tomato2")
      #lines(t$time, t$temperature, lwd = 2, col = "blue")
      mtext("Temperature(ºC)", 2, 2.25, cex  = 0.7)
      msg <- round(mean(s$temperature[(s$time >= (events["stop"]-2)) & (s$time <= events["stop"])], na.rm = TRUE), 2)
      msg <- paste0("Bottom temp. = ", msg,  "ºC") 
      text((events["touchdown"] + events["stop"])/2, par("usr")[3] + 0.85 * diff(par("usr")[3:4]), msg)
      box()

      # Tilt angle plot:
      index <- t$time >= xlim[1] & t$time <= xlim[2]
      ylim <- range(t[index, c("tilt-x", "tilt-y", "tilt-z")], na.rm = TRUE)
      ylim <- c(floor(ylim[1] / 10) * 10, (floor(ylim[2] / 10)+1) * 10)
      plot(xlim, ylim, type = "n", xaxs = "i", yaxs = "i", xlab = "", ylab = "", xaxt = "n")
      background(xlim, events)
      lines(t$time, t$"tilt-z", col = "darkolivegreen3", lwd = 1.5)
      lines(t$time, t$"tilt-y", col = "tomato2", lwd = 1.5)
      lines(t$time, t$"tilt-x", col = "skyblue3", lwd = 1.5)
      mtext("Tilt angle(º)", 2, 2.25, cex  = 0.7)
      box()
      legend("topright",
             legend = c("X", "Y", "Z"),
             col = c("skyblue3", "tomato2", "darkolivegreen3"), lwd = 1.5,
             bg = "white", cex = 0.8)
      axis(1)
      mtext("Time(min)", 1, 2.5, cex = 1.25)
      
      if (pdf) dev.off()
   }
}
