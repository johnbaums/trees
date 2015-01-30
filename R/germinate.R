#'Germinate a seed
#'
#'Observe the miracle of life as your seed germinates and spews forth a glorious
#'tree.
#'
#'@param seed A named \code{list} containing:
#'  \describe{\item{\code{trunk.height}}{Exactly how glorious will this tree 
#'  be?} \item{\code{branches}}{A binary coded vector of addresses of branches to be
#'  included. Branches can branch to the left or the right from the end of the
#'  trunk, or from the end of another branch included in \code{branches}. Elements of
#'  \code{branches} may only contain the characters given in args \code{left} and \code{right},
#'  and all parent branches of each element must also be provided. E.g. \code{'L'} is
#'  the first branch to the left stemming from the top of the trunk, while \code{'R'}
#'  is the first branch to the right, stemming from the top of the trunk; \code{'LR'}
#'  would be a branch forking to the right from the end of the first branch that
#'  forked left off the trunk.} \item{\code{lengths}}{A vector of branch lengths
#'  corresponding to the elements of \code{branches}. Should be the same length as
#'  \code{branches}.}}
#'@param angle The angle of branches relative to their parent branch (or 
#'  relative to the trunk). This angle is negated for left-pointing branches.
#'@param trunk.width The line width of the trunk. Widths are then scaled down 
#'  for successive child branches, to a minimum of 1.
#'@param left The character used to represent left-turning branches in the 
#'  \code{branches} vector (default is \code{'L'}).
#'@param right The character used to represent right-turning branches in the 
#'  \code{branches} vector (default is \code{'R'}). This must not be \code{'L'}.
#'@param plot Should the tree be plotted? (logical).
#'@param ... Further arguments to \code{\link{plot.seedling}}.
#'@return a \code{seedling} object, which is a \code{data.frame} comprising branch 
#'  addresses, depths, lengths, angles, base coordinates, and tip coordinates.
#'@seealso \code{\link{seed}}
#'@export
#'@examples
#' 
#' # Motivating example from http://stackoverflow.com/q/28163979/489704.
#' # Pass a named list (describing the seed) to germinate.
#' germinate(list(trunk.height=32, 
#'                branches=c(1,2,11,12,121, 122), 
#'                lengths=c(21, 19, 5, 12, 6, 2)), 
#'           left='1', right='2')
#'           
#' # Or simulate a seed and pass it directly to germinate
#' s <- seed(50, 10, min.branch.length=0, max.branch.length=5, 
#'              min.trunk.height=3, max.trunk.height=5)
#' germinate(s, trunk.width=15)
#' 
#' # In addition to the mighty oak above, you might also like to germinate a 
#' #  clumpy grassy-type thing
#' s <- seed(60, 15, min.branch.length=0, max.branch.length=5, 
#'              min.trunk.height=0, max.trunk.height=0)
#' germinate(s, angle=5, trunk.width=10)
germinate <- function(seed, angle=20, trunk.width=20, left='L', right='R', 
                      plot=TRUE, ...) {
  if(is(seed, 'seed')) {
    seed <- list(trunk.height=seed$length[1],
                 branches=seed$branch[-1],
                 lengths=seed$length[-1])
  }
  if (any(nchar(c(left, right))) != 1)
    stop('left and right must be a single alphanumeric character.')
  if (grepl('L', right)) 
    stop('right must not contain "L".')
  seed$branches <- sort(seed$branches)
  seed$lengths <- seed$lengths[order(seed$branches)]
  seed$branches <- gsub(left, 'L', seed$branches)
  seed$branches <- gsub(right, 'R', seed$branches)
  seed$angles <- sapply(gsub('R', paste0('+', angle), 
                             gsub('L', paste0('-', angle), seed$branches)), 
                        function(x) eval(parse(text=x)), USE.NAMES=FALSE)
  y1 <- x1 <- y0 <- x0 <- rep(NA, length(seed$branches))
  for (i in seq_len(length(seed$branches))) {
    if(seed$branches[i] %in% c('L', 'R')) {
      x0[i] <- 0
      y0[i] <- seed$trunk.height 
    } else {
      parent <- substr(seed$branches[i], 1, nchar(seed$branches[i])-1)
      x0[i] <- x1[which(seed$branches==parent)]
      y0[i] <- y1[which(seed$branches==parent)]
    } 
    tip <- get.xy(seed$angles[i], seed$lengths[i], x0[i], y0[i])
    x1[i] <- tip[, 1]
    y1[i] <- tip[, 2]
  }
  d <- rbind(c(0, 0, 0, 0, 0, 0, 0, seed$trunk.height),
             data.frame(branches=seed$branches, 
                        depth=ifelse(seed$branches==0, 0, nchar(seed$branches)),
                        length=seed$lengths,
                        seed$angles, x0, y0, x1, y1,
                        stringsAsFactors=FALSE))
  class(d) <- c('seedling', 'data.frame')
  if(isTRUE(plot)) plot(d, trunk.width=trunk.width, ...)
  return(d)
}
