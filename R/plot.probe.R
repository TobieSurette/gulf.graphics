#' Plot Data Probe

#'@export
plot.probe <- function(x, ...){
   vars <- setdiff(names(x), c("data", "time"))
   m <- kronecker(1:length(n), matrix(1, nrow = 5, ncol = 5))
   m <- rbind(0, cbind(0, m, 0), 0, 0)
   layout(m)
   par(mar = c(0,0,0,0))
   t <- time2min(time(x))
   for (i in 1:length(vars)){
      plot(t, x[, vars[i]])
      grid()
   }
}
