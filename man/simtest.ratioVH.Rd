\name{simtest.ratioVH}
\alias{simtest.ratioVH}

\title{Approximate simultaneous tests for ratios of normal means with heterogeneous variances}
\description{
  Performs simultaneous tests for several ratios of linear combinations of treatment means in a normal one-way layout,
 assuming normal distribution of the data allowing heterogeneous variances.
 
}
\usage{
simtest.ratioVH(formula, data,
 type = "Dunnett", base = 1, alternative = "two.sided",
 Margin.vec = NULL, FWER = 0.05,
 Num.Contrast = NULL, Den.Contrast = NULL,
 names = TRUE)
}

\arguments{
  \item{formula}{ A formula specifying a numerical response and a grouping factor (e.g., response ~ treatment) }
  \item{data}{  A dataframe containing the response and group variable }
  \item{type}{ type of contrast, with the following options:
  \itemize{
    \item{"Dunnett":}{ many-to-one comparisons, with control in the denominator}
    \item{"Tukey":}{ all-pair comparisons }
    \item{"Sequen":}{ comparison of consecutive groups, where the group with lower order is the denomniator}
    \item{"AVE":}{ comparison of each group with average of all others, where the average is taken as denominator}
    \item{"Changepoint":}{ ratio of averages of groups of higher order divided by averages of groups of lower order }
    \item{"Marcus":}{ Marcus contrasts as ratios }
    \item{"McDermott":}{ McDermott contrasts as ratios }
    \item{"Williams":}{ Williams contrasts as ratios }
    }
 Note: type is ignored if Num.Contrast and Den.Contrast are specified by the user (See below).
}
  \item{base}{ a single integer specifying the control (i.e. denominator) group for the Dunnett contrasts, ignored otherwise }
  \item{alternative}{ a character string:
    \itemize{
    \item{"two.sided":}{ for two-sided tests }
    \item{"less":}{ for lower tail tests }
    \item{"greater":}{ for upper tail tests }
}}
  \item{Margin.vec}{ a single numerical value or vector of Margins under the null hypotheses, default is 1 }
  \item{FWER}{ a single numeric value specifying the family-wise error rate to be controlled }
  \item{Num.Contrast}{ Numerator contrast matrix, where columns correspond to groups and rows correspond to contrasts  }
  \item{Den.Contrast}{ Denominator contrast matrix, where columns correspond to groups and rows correspond to contrasts }
  \item{names}{ a logical value: if TRUE, the output will be named according to names of user defined contrast or factor levels }
  
}

\details{
The associated ratio test statistic T[i] has a t-distribution. Multiplicity adjustment is achieved by using quantiles of r r-variate t-distributions, which differ in the degree of freedom and share the correlation structure. The compariso-specific degrees of freedom
are derived using the approximation according to Satterthwaite (1946).
}


\value{
  An object of class simtest.ratio containing:
  
  \item{estimate }{a (named) vector of estimated ratios}
  \item{teststat }{ a (named) vector of the calculated test statistics}
  \item{Num.Contrast }{the numerator contrast matrix}
  \item{Den.Contrast }{the denominator contrast matrix}
  \item{CorrMat }{the correlation matrix of the multivariate t-distribution calculated under the null hypotheses}
  \item{critical.pt }{the equicoordinate critical value of the multi-variate t-distribution for a specified FWER}
  \item{p.value.raw }{a (named) vector of unadjusted p-values}
  \item{p.value.adj }{a (named) vector of p-values adjusted for multiplicity}
  \item{Margin.vec }{the vector of margins under the null hypotheses}

and some other input arguments.

}

\references{  No publication so far for the implemented method.
\emph{Satterthwaite, FE (1946)}. An approximate distribution of estimates of variance components. \emph{Biometrics 2, 110-114}.
 }
\author{ Mario Hasler, Frank Schaarschmidt }
\note{ 

}
\seealso{ \code{\link{plot.sci.ratio}} }

\examples{

data(Mutagenicity, package="mratios")

boxplot(MN~Treatment, data=Mutagenicity)

Mutas<-subset(Mutagenicity, Treatment!="Cyclo25")

simtest.ratioVH(MN~Treatment, data=Mutagenicity,
 type="Dunnett", base=6, Margin.vec=1.2, alternative="less")

}

\keyword{ htest }
\concept{ratio}
\concept{heteroscedasticity}
\concept{multiple comparisons}