#'Calculate x and y coordinates in a cartesian plane.
#'
#' Calculate x and y coordinates in a cartesian plane, given distance and angle
#'from a given origin.
#'
#'@param a Angle from (\code{x0}, \code{y0}).
#'@param d Distance from (\code{x0}, \code{y0}).
#'@param x0 x coordinate from which \code{d} and \code{a} are measured.
#'@param y0 y coordinate from which \code{d} and \code{a} are measured.
#'@return a \code{data.frame} with coordinates of the desired point.
get.xy <- function(a, d, x0, y0) {
  a <- ifelse(a <= 90, 90 - a, 450 - a)
  data.frame(x = x0 + d * cos(a / 180 * pi), 
             y = y0+ d * sin(a / 180 * pi))
}