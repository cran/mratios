\name{BW}
\alias{BW}
\docType{data}
\title{ Body weights measured in a toxicological study}
\description{
  Body weights of a 90-day chronic toxicological study on rats with a control and three dose groups.
}
\usage{data(BW)}
\format{
  A data frame with 60 observations on the following 2 variables.
  \describe{
    \item{\code{Weight}}{a numeric vector containing the bodyweights of rats}
    \item{\code{Dose}}{a factor with levels \code{1} \code{2} \code{3} \code{4}, specifying the dose groups, where \code{1} is the control group }
  }
}
\source{
  Hothorn, L.A. (2004): Statistische Auswerteverfahren. In: Regulatorische Toxikologie (Reichl, F.X., ed.). Springer Verlag Heidelberg, pp. 167-181.
}
\references{

}
\examples{
data(BW)
boxplot(Weight~Dose, data=BW)


}
\keyword{datasets}
