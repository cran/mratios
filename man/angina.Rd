\name{angina}
\alias{angina}
\docType{data}
\title{ The angina data set from 'multcomp' package }
\description{
  Dose response study of an angina drug; data set taken from 
     Westfall et al. (1999, p. 164).
}
\usage{data(angina)}
\format{
  A data frame with 50 observations on the following 2 variables.
  \describe{
    \item{\code{dose}}{a factor with levels \code{0} \code{1} \code{2} \code{3} \code{4}}
    \item{\code{response}}{a numeric vector giving the change from pretreatment as measured
          in minutes of pain-free walking.}
  }
}
\details{
  See Westfall et al. (1999, p. 164)
}
\source{
   P. H. Westfall, R. D. Tobias, D. Rom, R. D. Wolfinger, Y. Hochberg
     (1999). Multiple Comparisons and Multiple Tests Using the SAS
     System. Cary, NC: SAS Institute Inc.
}
\references{
  angina(multcomp)
}
\examples{

library(mratios)

data(angina)

plot(response~dose, data=angina)

}
\keyword{datasets}

