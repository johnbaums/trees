plot.seedling <- function(x, trunk.width=20, ...) {
  plot(c(x$x0, x$x1), c(x$y0, x$y1), asp=1, type='n', axes=FALSE, xlab='', ylab='')  
  with(x, segments(x0, y0, x1, y1, lwd=pmax(trunk.width/nchar(x$branches), 1), ...))
}