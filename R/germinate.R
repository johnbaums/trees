#'Germinate a seed.
#'
#'Observe the miracle of life as your seed germinates and spews forth a glorious
#'tree.
#'
#'@param x Either a \code{seed} object returned by \code{\link{seed}}, or a
#'  named \code{list} containing: \describe{\item{\code{trunk.height}}{Exactly
#'  how glorious will this tree be?} \item{\code{branches}}{A binary coded
#'  vector of addresses of branches to be included. Branches can branch to the
#'  left or the right from the end of the trunk, or from the end of another
#'  branch included in \code{branches}. Elements of \code{branches} may only
#'  contain the characters given in args \code{left} and \code{right}, and all
#'  parent branches of each element must also be provided. E.g. \code{'L'} is
#'  the first branch to the left stemming from the top of the trunk, while
#'  \code{'R'} is the first branch to the right, stemming from the top of the
#'  trunk; \code{'LR'} would be a branch forking to the right from the end of
#'  the first branch that forked left off the trunk.} \item{\code{lengths}}{A
#'  vector of branch lengths corresponding to the elements of \code{branches}.
#'  Should be the same length as \code{branches}.}}
#'@param angle The angle of branches relative to their parent branch (or 
#'  relative to the trunk). This angle is negated for left-pointing branches.
#'@param trunk.width The line width of the trunk. Widths are then scaled down 
#'  for successive child branches, to a minimum of 1.
#'@param left The character used to represent left-turning branches in the 
#'  \code{branches} vector (default is \code{'L'}).
#'@param right The character used to represent right-turning branches in the 
#'  \code{branches} vector (default is \code{'R'}). This must not be \code{'L'}.
#'@param plot Should the tree be plotted? (logical).
#'@param ... Further arguments to \code{plot.plant}.
#'@return a \code{plant} object, which is a \code{data.frame} comprising branch 
#'  addresses, depths, lengths, angles, base coordinates, and tip coordinates.
#'@seealso \code{\link{seed}} \code{\link{foliate}} \code{\link{prune}}
#'@export
#'@examples
#' 
#' # Motivating example from http://stackoverflow.com/q/28163979/489704.
#' # Pass a named list (describing the seed) to germinate.
#' germinate(list(trunk.height=32, 
#'                branches=c(1,2,11,12,121, 122), 
#'                lengths=c(21, 19, 5, 12, 6, 2)), 
#'           left='1', right='2', angle=40)
#'           
#' # Or simulate a seed and pass it directly to germinate
#' s <- seed(50, 10, min.branch.length=0, max.branch.length=5, 
#'              min.trunk.height=3, max.trunk.height=5)
#' g <- germinate(s, trunk.width=15)
#' 
#' # Additional realism (hard to imagine!) is possible by actually plotting the 
#' #  trees in a browny colour...
#' plot(g, trunk.width=15, col='peachpuff4')
#' # (Pro-tip: see ?foliate for more realism)
#' 
#' # In addition to the mighty oaks above, you might also like to germinate a 
#' #  clumpy grassy-type thing
#' s <- seed(60, 15, min.branch.length=0, max.branch.length=5, 
#'              min.trunk.height=0, max.trunk.height=0)
#' g <- germinate(s, angle=5, trunk.width=10)
germinate <- function(x, angle=20, trunk.width=20, left='L', right='R', 
                      plot=TRUE, ...) {
  if(is(x, 'seed')) {
    x <- list(trunk.height=x$length[1],
                 branches=x$branch[-1],
                 lengths=x$length[-1])
  }
  if (any(nchar(c(left, right))) != 1)
    stop('left and right must be a single alphanumeric character.')
  if (grepl('L', right)) 
    stop('right must not contain "L".')
  x$lengths <- x$lengths[order(x$branches)]
  x$branches <- sort(x$branches)
  x$branches <- gsub(left, 'L', x$branches)
  x$branches <- gsub(right, 'R', x$branches)
  x$angles <- sapply(gsub('R', paste0('+', angle), 
                             gsub('L', paste0('-', angle), x$branches)), 
                        function(x) eval(parse(text=x)), USE.NAMES=FALSE)
  y1 <- x1 <- y0 <- x0 <- rep(NA, length(x$branches))
  for (i in seq_len(length(x$branches))) {
    if(x$branches[i] %in% c('L', 'R')) {
      x0[i] <- 0
      y0[i] <- x$trunk.height 
    } else {
      parent <- substr(x$branches[i], 1, nchar(x$branches[i])-1)
      x0[i] <- x1[which(x$branches==parent)]
      y0[i] <- y1[which(x$branches==parent)]
    } 
    tip <- get.xy(x$angles[i], x$lengths[i], x0[i], y0[i])
    x1[i] <- tip[, 1]
    y1[i] <- tip[, 2]
  }
  d <- rbind(c(0, 0, 0, 0, 0, 0, 0, x$trunk.height),
             data.frame(branches=x$branches, 
                        depth=ifelse(x$branches==0, 0, nchar(x$branches)),
                        length=x$lengths,
                        angles=x$angles, x0, y0, x1, y1,
                        stringsAsFactors=FALSE))
  class(d) <- c('plant', 'data.frame')
  if(isTRUE(plot)) plot(d, trunk.width=trunk.width, ...)
  return(d)
}
