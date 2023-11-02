FDA___Smoothing___Bspline___Plotting = function(y, breaks, smoothed, main="", file.name=NULL, save.path=NULL){
  if(is.null(file.name) || is.null(save.path)){
    plot(x = 1:length(y), y = y, col = 2, main = main)
    abline(v = knots, lty = 2, lwd = 1)
    lines(smoothed, col = 4, lwd = 2)
  }else{
    png(filename = paste0(save.path, "/", file.name))
    plot(x = 1:length(y), y = y, col = 2, main = main)
    abline(v = knots, lty = 2, lwd = 1)
    lines(smoothed, col = 4, lwd = 2)
    dev.off()
  }
}

