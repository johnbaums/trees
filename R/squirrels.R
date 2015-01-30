squirrels <- function(x, branches, pos, left='L', right='R', ...) {
  if (any(nchar(c(left, right))) != 1) 
    stop('left and right must be a single alphanumeric character.')
  if (grepl('L', right)) 
    stop('right must not contain "L".')
  branches <- gsub(left, 'L', branches)
  branches <- gsub(right, 'R', branches)
  b <- x[match(branches, x$branch), ]
  xy <- get.xy(b$angles, pos, b$x0, b$y0)
  points(xy, ...)
  xy
}