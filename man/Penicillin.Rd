\name{Penicillin}
\alias{Penicillin}
\docType{data}
\title{Comparing 6 strains with respect to production of antibiotics }
\description{
The production of antibiotics of 6 strains (mutants of the same micro organism) was compared. MO were put to holes in agar infected with Bacteria.
 The diameter of Baceria-free areas around the colonies of the MO was recorded. Each strain was repeated 8 times.
  
}
\usage{data(Penicillin)}
\format{
  A data frame with 48 observations on the following 2 variables.
  \describe{
    \item{\code{strain}}{a numeric veactor, the number identifying the strains}
    \item{\code{diameter}}{a numeric vector, size of the diameter of Bacteria-free area around each colony}
  }
}

\source{
  Horn, M, Vollandt, R (1995): Multiple Tests und Auswahlverfahren in Biomtrie (Lorenz, RJ, Vollmar, J, eds). Gustav Fischerverlag, Stuttgart Jena New York.
}
\examples{
data(Penicillin)
## maybe str(Penicillin) ; plot(Penicillin) ...
}
\keyword{datasets}
