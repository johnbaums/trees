#'Plot method for plant objects.
#'
#'@param x The \code{plant} object to be plotted.
#'@param trunk.width The plotting line width for the trunk. Successive child
#'  branches are plotted with increasingly finer \code{lwd}, to a minimum of
#'  \code{1}.
#'@param ... Additional arguments passed to \code{plot} and \code{segments}.
#'@return \code{NULL}
#'@seealso \code{\link{germinate}}
#'@export
plot.plant <- function(x, trunk.width=20, ...) {
  plot(c(x$x0, x$x1), c(x$y0, x$y1), type='n', asp=1, axes=FALSE, xlab='', ylab='', ...)  
  with(x, segments(x0, y0, x1, y1, lwd=pmax(trunk.width/nchar(x$branches), 1), ...))
}