#' Prune a plant.
#' 
#' If your plant is out of control and you have a bit of time on the weekend, get
#' out the secateurs and tidy up the garden.
#' 
#' @param x A \code{plant} as returned by \code{\link{germinate}}.
#' @param intensity The pruning depth. Default is \code{1}.
#' @param plot Should the plant be plotted?
#' @param ... Further arguments to \code{plot.plant} (ignored if \code{plot} is
#'   \code{FALSE}).
#' @return a modified \code{plant} object with the outermost \code{intensity}
#'   stems hacked off.
#' @seealso \code{\link{germinate}}
#' @export
#' @examples
#' s <- seed(50, 10, min.branch.length=0, max.branch.length=5,
#'           min.trunk.height=5, max.trunk.height=8)
#' g <- germinate(s, col='navajowhite4')
#' prune(g, 3, xlim=par('usr')[1:2], ylim=par('usr')[3:4], xaxs='i', yaxs='i',
#'       col='navajowhite4')
#'
#' s <- seed(70, 12, min.branch.length=0, max.branch.length=5,
#'           min.trunk.height=5, max.trunk.height=8)
#' g <- germinate(s, plot=FALSE)
#' par(mar=rep(0, 4), mfrow=n2mfrow(max(g$depth)+1))
#' plot(g, trunk.width=8, col='peachpuff4')
#' leafygreens <- colorRampPalette(paste0('darkolivegreen', c('', 1:4)))(100)
#' nleaves <- length(which(g$depth >= 3))
#' foliate(g, nleaves, 3, pch=24:25, col=NA, cex=1.8, bg=paste0(leafygreens, '30'))
#' for (i in seq_len(max(g$depth))) {
#'   g <- prune(g, 1, xlim=par('usr')[1:2], ylim=par('usr')[3:4],
#'              xaxs='i', yaxs='i', col='peachpuff4', trunk.width=5)
#'   nleaves <- length(which(g$depth >= 3))
#'   nleaves
#'   if (nleaves > 0) foliate(g, nleaves, 3, pch=24:25, col=NA, cex=1.5,
#'                            bg=paste0(leafygreens, '30'))
#' }
prune <- function(x, intensity=1, plot=TRUE, ...) {
  if(!is(x, 'plant')) stop('x should be a plant (see ?germinate).')
  if(!any(x$depth != 0)) stop('Nothing to prune. x is a lifeless trunk.')
  x <- x[which(x$depth <= (max(x$depth)) - intensity), ]
  if(isTRUE(plot)) plot(x, ...)
  x
}
