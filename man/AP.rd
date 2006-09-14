\name{AP}
\alias{AP}
\docType{data}
\title{ Angina pectoris data}
\description{
  A data set is generated (from normal distribution) to imitate the summary statistics in Table II of Bauer et al. (1998). 
  In the experiment, patients with chronic stable angina pectoris were randomized to five treatment arms (placebo, three doses of a new compound, and an active control).
  The primary endpoint is the difference in the duration of an exercise test before and after treatment.
}
\usage{data(AP)}
\format{
  A data frame with 303 observations on the following 2 variables.
  \describe{
    \item{\code{pre_post}}{a numeric vector}
    \item{\code{treatment}}{a factor with levels \code{AC}the active control, \code{D0} the zero dose (placebo), and \code{D50}} \code{D100}, \code{D150} the three dose groups of the new compound.
  }
}
\source{
Bauer, P., Roehmel, J., Maurer, W., and Hothorn, L. (1998): Testing strategies in multi-dose experiments including active control. Statistics in Medicine 17, 2133-2146.
}

\examples{

library(mratios)

data(AP)
boxplot(pre_post ~ treatment, data=AP)
by(AP,AP$treatment, function(x){mean(x$pre_post)})
by(AP,AP$treatment, function(x){sd(x$pre_post)})
}
\keyword{datasets}
