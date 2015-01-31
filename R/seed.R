#'Generate random seeds.
#'
#'Generate a random seed whose DNA will shape its development upon germination.
#'
#'@param n A measure of branchiness. How many terminal branches will this seed 
#'  produce?
#'@param max.depth How deep will the hierarchy of branches be?
#'@param min.trunk.height How short might the trunk be? Default is \code{10}.
#'@param max.trunk.height How tall might the trunk be? Default is \code{30}.
#'@param min.branch.length How short might each branch (branch segment) be?
#'  Default is \code{3}.
#'@param max.branch.length How long might each branch (branch segment) be?
#'  Default is \code{10}.
#'@return a \code{seed} object containing \code{branches} and \code{lengths}, 
#'  and suitable for input to \code{\link{germinate}}.
#'@seealso \code{\link{germinate}}
#'@export
#'@examples
#' 
#' s <- seed(50, 10, min.branch.length=0, max.branch.length=5, 
#'          min.trunk.height=5, max.trunk.height=8)
#'          
#' head(s)
seed <- function(n, max.depth, min.trunk.height=10, max.trunk.height=30, 
                 min.branch.length=3, max.branch.length=10) {
  if(min.branch.length > max.branch.length) 
    stop('min.branch.length > max.branch.length')
  if(min.trunk.height > max.trunk.height) 
    stop('min.trunk.height > max.trunk.height')
  if (n > 2^max.depth) 
    stop(sprintf('Maximum number of terminal branches is %s', 2^max.depth))
  all <- unlist(lapply(seq_len(max.depth), function(x) 
    do.call(paste0, expand.grid(rep(list(c('L', 'R')), x)))))
  
  twigs <- sample(all, n)
  while(any(rowSums(outer(paste0('^', twigs), twigs, Vectorize(grepl))) > 1)) {
    knots <- rowSums(outer(paste0('^', twigs), twigs, Vectorize(grepl))) > 1
    all <- all[rowSums(Vectorize(grepl, 'pattern')
                       (paste0('^', twigs[!knots]), all)) == 0]
    twigs <- c(twigs[!knots], sample(all, sum(knots)))
  }
  
  branches <- sort(unique(c(unlist(lapply(twigs, function(x) 
    sapply(seq_len(nchar(x)), function(y) substr(x, 1, y)))))))
  seed <- data.frame(branch=c(0, branches), 
                     length=c(runif(1, min.trunk.height, max.trunk.height), 
                              runif(length(branches), min.branch.length, 
                                    max.branch.length)), 
                     stringsAsFactors=FALSE)
  class(seed) <- c('seed', 'data.frame')
  seed
}