\name{plot.sci.ratio}
\alias{plot.sci.ratio}

\title{ Plot output for sci.ratio and sci.ratio.gen }
\description{
  Plot the intervals returned by sci.ratio
}
\usage{
\method{plot}{sci.ratio}(x, rho0 = 1, rho0lty = 2, CIvert = FALSE, CIlty = 1, CIlwd = 1, CIcex = 1, main = NULL, ylab = NULL, xlab = NULL, sub = NULL, ...)
}

\arguments{
  \item{x}{ an object of class "sci.ratio" as can be obtained by calling the function sci.ratio}
  \item{rho0}{ a single numeric value or vector of values defining the hypothesized ratio }
  \item{rho0lty}{ integer values to specify the line type for the rho0 line }
  \item{CIvert}{ logical, CI are plotted vertical if CIvert=TRUE and horizontal otherwise }
  \item{CIlty}{ numeric value, giving the line type of the plotted confidence interval, see argument lty in ?par }
  \item{CIlwd}{ numeric value, giving the line width of the plotted confidence interval, see argument lwd in ?par }
  \item{CIcex}{ a single numeric value: by which amount the symbols in the CI shall scaled relative to the default see argument cex in ?par }
  \item{main}{ character string to be plotted as main title of the plot }
  \item{ylab}{ character string, label of the y axis (ignored if CIvert=TRUE) }
  \item{xlab}{ character string, label of the x axis (ignored if CIvert=FALSE)  }
  \item{sub}{ as in plot }
  \item{\dots}{ further arguments from plot or par, e.g. cex.axis }
 }

\details{
  Too long names of the contrasts/comparisons should be avoided, otherwise use par() to change plot parameters.
}
\value{
A plot of the confidence intervals in the sci.ratio object.
}
\references{ plot.hmtest(multcomp) }
\author{ Frank Schaarschmidt }
\seealso{ plot.hmtest(multcomp) }
\examples{

data(angina)
aCI<-sci.ratio(response~dose, data=angina, type="Dunnett", alternative="greater")

# Visualize testing for superiority

plot(aCI, rho0=1.25, rho0lty=3)



}
\keyword{ hplot }
\keyword{ htest }
