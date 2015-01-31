\name{trees-package}
\alias{trees-package}
\alias{trees}
\docType{package}
\title{
The miracle of binary life.
}
\description{
Create seeds, plant them, and watch them grow. 

The genetic make-up of the tree will determine it's final appearance. We can control this with arguments to \code{seed()}, such as \code{n} (the number of terminal branches); \code{max.depth} (the number of "tiers" of branches); \code{min.trunk.height} and \code{max.trunk.height} (the range within which the expressed trunk height will be randomly sampled); and \code{min.branch.length} and \code{max.branch.length} (the range within which branches' lengths will be randomly sampled). When a seed is germinated with \code{germinate()}, we can also control the \code{trunk.width} (which in turn determines the width of successive child branches) and the branching \code{angle}.
}
\details{
\tabular{ll}{
Package: \tab trees\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-01-30\cr
License: \tab Copyright Super-plus\cr
}
Take a look at \code{\link{seed}}, \code{\link{germinate}}, \code{\link{prune}}, and \code{\link{foliate}}.
}
\author{
John Baumgartner

Maintainer: John Baumgartner <johnbaums@gmail.com>
}
\references{
}
\keyword{ package }
\seealso{
}
\examples{
}
