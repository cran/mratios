\name{SRAssay}
\alias{SRAssay}
\docType{data}
\title{ Slope ratio assay of panthotenic acid contents in plant tissues }
\description{
  Content of panthotenic acid in a standard and three unknown samples were measured. The response variable is the
titer of a sample to pH 6.8.

}
\usage{data(SRAssay)}
\format{
  A data frame with 34 observations on the following 3 variables.
  \describe{
    \item{\code{Response}}{a numeric vector, containing the response variable (titer to pH 6.8)}
    \item{\code{Treatment}}{a factor with levels \code{St} \code{U1} \code{U2} \code{U3}, specifying the standard and 3 unknown samples}
    \item{\code{Dose}}{a numeric vector}
  }
}
\details{
  
}
\source{
  Jensen, D.R. (1989): Joint confidence sets in multiple dilution assays. Biometrical Journal 31, 841-853.
}
\references{
  Data originally from Bliss, C.I. (1952): The Statistics of Bioassay. Academic Press, New York.
}
\examples{

library(mratios)

data(SRAssay)

str(SRAssay)

plot(Response~Dose, data=SRAssay)

# library(lattice)
# xyplot(Response~Dose|Treatment, data=SRAssay)

# see ?sci.ratio.gen for  the analysis of this dataset

}
\keyword{datasets}
\concept{slope ratio assay}
