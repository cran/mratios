"summary.sci.ratio.gen" <-
function(object, digits=4,...)
{
cat("       ", "\n")
cat("The following linear model has been fitted in lm: ","\n")

print(summary(object$fit))

cat("       ", "\n")
cat("Numerator contrast matrix:        ","\n")
print(object$Num.Contrast, digits=digits )

cat("       ", "\n")
cat("Denominator contrast matrix:        ","\n")
print( object$Den.Contrast, digits=digits )

cat("       ","\n")
cat("Estimated ratios:        ","\n")
print( object$estimate, digits=digits )


if(object$method=="Plug")
{
 cat("       ","\n")
 cat("Estimated correlation matrix used for calculation of quantiles:        ","\n")
 print(object$CorrMat.est, digits=digits)
}

cat("                                      ","\n")

if(object$method=="Unadj")

{

if(object$alternative=="two.sided")
 {
  cat("Two-sided",object$conf.level*100, "%", "unadjusted confidence intervals for ratios:","\n")
 }
if(object$alternative=="less")
 {
  cat("Upper",object$conf.level*100, "%", "unadjusted confidence limits for ratios:","\n")
 }

if(object$alternative=="greater")
 {
  cat("Lower",object$conf.level*100, "%", "unadjusted confidence limits for ratios:","\n")
 }

}

else

{
if(object$alternative=="two.sided")
 {
  cat("Two-sided",object$conf.level*100, "%", "simultaneous confidence intervals for ratios:","\n")
 }
if(object$alternative=="less")
 {
  cat("Upper",object$conf.level*100, "%", "simultaneous confidence limits for ratios:","\n")
 }

if(object$alternative=="greater")
 {
  cat("Lower",object$conf.level*100, "%", "simultaneous confidence limits for ratios:","\n")
 }
}

cat("                                      ","\n")

if(object$NSD)
{
 print(cbind(estimate=object$estimate, object$conf.int) )

 cat("                                      ","\n")
 cat("   NSD = The mean in the denominator is not significantly different from zero. ","\n")
 cat("                                      ","\n") 
}
else
{
 print(cbind(estimate=object$estimate, object$conf.int) ,digits=digits )
}


}

