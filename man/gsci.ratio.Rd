\name{gsci.ratio}
\alias{gsci.ratio}
\title{Test version! Simultaneous confidence intervals for ratios of linear combinations of parameter.}
\description{
This function calculates simultaneous confidence intervals for ratios of user-defined linear combinations, given a vector parameter estiamtes and a corresponding variance-covariance matrix.
Beside unadjusted intervals, multiplicity adjustments are available using quantiles of a multivariate Normal- or t-distribution. The function provides a more general, but less user-friendly function
to calculate ratios of mean parameters from linear (mixed models).
}
\usage{
gsci.ratio(est, vcmat, Num.Contrast, Den.Contrast,
 degfree = NULL, conf.level = 0.95, alternative = "two.sided",
 adjusted = TRUE)
}
\arguments{
  \item{est}{A numeric vector of parameter estimates, for example coefficients of a linear model}
  \item{vcmat}{The corresponding variance-covariance matrix (Number of rows and columns should be the same as the length of the parameter vector)}
  \item{Num.Contrast}{Numerator contrast matrix, where the number of columns must be the same as the length of the parameter vector, and each row represents one contrast}
  \item{Den.Contrast}{Denominator contrast matrix, where the number of columns must be the same as the length of the parameter vector, and each row represents one contrast}
  \item{degfree}{Degrees of freedom used for calculating quantiles of a (multivariate) t-distribution. If NULL, Normal approximations are used}
  \item{conf.level}{Simultaneous confidence level in case of \kbd{adjusted == TRUE}, and comparisonwise confidence level in case of \kbd{adjusted == FALSE}}
  \item{alternative}{a character string: \kbd{"two.sided"} for two-sided intervals, \kbd{"less"} for upper confidence limits, \kbd{"greater"} for lower confidence limits }
  \item{adjusted}{If \kbd{TRUE}, the simultaneous confidence level is controlled, otherwise the comparisonwise confidence level is used}
}
\details{
THIS VERSION IS ONLY FOR TESTING!!!

Given a parameter vector and it's corresponding variance-covariance matrix, simultaneous confidence intervals for several ratios of linear combinations of these parameters are calculated.
For simultaneous confidence intervals (adjusted=TRUE) the Plug-in method is used (plugging the maximum likelihood estimates of the ratios in the unknown correlation matrix, to calculate quantiles of a multivariate distribution).

Linear combinations can be defined by providing matrices for the nominator and the denominator; some pre-defined contrasts can be constructed by the function contrMatRatio. (These may be weighted for different sample sizes.)
}
\value{
An object of class "sci.ratio" and "gsci.ratio", containing a list with elements:

  \item{estimate}{point estimates of the ratios}
  \item{CorrMat.est}{estimate of the correlation matrix}
  \item{Num.Contrast}{matrix of contrasts used for the numerator of ratios}
  \item{Den.Contrast}{matrix of contrasts used for the denominator of ratios}
  \item{conf.int}{confidence interval estimates of the ratios}
  
And some further elements to be passed to print and summary functions.
}
\references{ The code is an adaptation of the methods described for the completely randomized one-way layout by 
\emph{Dilba, G., Bretz, F., and Guiard, V. (2006):} Simultaneous confidence sets and confidence intervals for multiple ratios. Journal of Statistical Planning and Inference 136, 2640-2658.

When used in mixed models, the methods are only asymptically valid.
}
\author{Daniel Gerhard, adapting code of Gemechis Dilba Djira}
\seealso{glht(multcomp) for simultaneous CI of differences of means, and function sci.ratio.gen(mratios)}

\examples{

library(mratios)

##############################################################

# A 90-days chronic toxicity assay:
# Which of the doses (groups 2,3,4) do not show a decrease in
# bodyweight more pronounced than 90 percent of the bodyweight
# in the control group?

#############################################################

data(BW)
boxplot(Weight~Dose,data=BW)

lmfit <- lm(Weight~Dose-1, data=BW)
est <- coefficients(lmfit)
vc <- vcov(lmfit)
CMAT <- contrMatRatio(table(BW$Dose), type="Dunnett")

BWnoninf <- gsci.ratio(est, vc, CMAT$numC, CMAT$denC,
 alternative="greater", degfree=lmfit$df.residual)

BWnoninf
# the same:
sci.ratio(Weight~Dose, data=BW, type="Dunnett", alternative="greater")

# Plot
plot(BWnoninf, rho0=0.9)

##############################################################

#### Mixed Model Example

##############################################################

library(nlme)
data(Milk)

# Fit a linear mixed model (maybe there are nicer models available!)

lmefit <- lme(protein ~ Diet-1, data=Milk,
 random=~Time|Cow, correlation=corAR1(form=~Time|Cow))

# Extract the parameter estimates and the corresponding
# variance-covariance matrix

estm <- fixef(lmefit)
vcm <- vcov(lmefit)


# Define the matrices defining the ratios of interest for
# all-pair comparisons: CM is the numerator matrix and 
# DM is the denominator matrix.

CM <- rbind(c(1,0,0),
            c(1,0,0),
            c(0,1,0))
DM <- rbind(c(0,1,0),
            c(0,0,1),
            c(0,0,1))

# Add some row names (This is optional!)

rownames(CM) <- c("b/b+l", "b/l", "b+l/l")

# Calculate and plot simultaneous confidence intervals:

gscimix <- gsci.ratio(estm, vcm, CM, DM, degfree=anova(lmefit)[,2])
plot(gscimix)

}

\keyword{htest}
\concept{ratio}
\concept{mixed model}