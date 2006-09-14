\name{contrMatRatio}
\alias{contrMatRatio}

\title{ Creates numerator and denominator contrast matrices for ratio-based hypotheses for common multiple comparison and trend test problems }
\description{
 Creates numerator and denominator contrast matrices for some common multiple comparison and trend test problems. 
 These matrices are internally used by the sci.ratio and simtest.ratio functions. The contrMatRatio function is a modification of the function contrMat (multcomp).

 Whether the given definitions of contrast matrices for trend test problems in terms of ratios make sense and how they are to be interpreted is to be discussed.
}
\usage{
contrMatRatio(n, type = "Tukey", base = 1)
}

\arguments{
  \item{n}{ integer vector of sample sizes }
  \item{type}{the type of multiple contrasts 
  \itemize{
    \item{"Dunnett"}{ many to one comparisons, with the control group in the denominator}
    \item{"Tukey"}{ all-pair comparisons }
    \item{"Sequen"}{ comparison of consecutive groups, where the groups of lower order is the denominator }
    \item{"AVE"}{ comparison of each group with average of all others, where the average is taken as denominator}
    \item{"Changepoint"}{ ratio of averages of groups of higher order divided by averages of groups of lower order }
    \item{"Marcus"}{ Marcus contrasts defined for ratios }
    \item{"McDermott"}{ McDermott contrasts for ratios }
    \item{"Williams"}{ Williams contrasts for ratios }
    }
}
  \item{base}{a single integer specifying the control (i.e. denominator) group for "Dunnett"-type contrasts for calculating the ratios to the control }
}
\details{
  This is a simple adaption of the contrMat function in the package multcomp for ratio hypotheses.
}
\value{
  A list containing:
  \item{numC }{the (named) numerator contrast where rows correspond to contrasts } 
  \item{denC }{the (named) denominator contrast where rows correspond to contrasts }
  \item{rnames }{ a character vector with names of the contrasts }
  and the type of contrast as attr.

}

\author{ Frank Schaarschmidt, by slightly modifying the code of contrMat(multcomp) }
\seealso{ contrMat(multcomp) }
\examples{

library(mratios)

n=c(A=10,B=20,Z=10,D=10)

contrMatRatio(n=n, type="Dunnett", base=1)
contrMatRatio(n=n, type="Dunnett", base=3)

contrMatRatio(n=n, type="Tukey")
contrMatRatio(n=n, type="Sequen")
contrMatRatio(n=n, type="AVE")
contrMatRatio(n=n, type="Williams")
}

\keyword{htest}