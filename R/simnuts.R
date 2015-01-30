simnuts <- function(x, n, min.depth=3, max.depth, ...) {
  b <- x[sample(which(x$depth > min.depth), n, replace=TRUE), ]
  if(missing(max.depth)) max.depth <- max(b$depth)
  nuts <- get.xy(b$angles, runif(n, 0, b$length), b$x0, b$y0)
  points(nuts, ...)
}