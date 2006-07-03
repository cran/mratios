"print.sci.ratio" <-
function(x, digits=4,...)
{
cat("                                      ","\n")

if(x$method=="Unadj")

{

if(x$alternative=="two.sided")
 {
  cat("Two-sided",x$conf.level*100, "%", "unadjusted confidence intervals for ratios:","\n")
 }
if(x$alternative=="less")
 {
  cat("Upper",x$conf.level*100, "%", "unadjusted confidence limits for ratios:","\n")
 }

if(x$alternative=="greater")
 {
  cat("Lower",x$conf.level*100, "%", "unadjusted confidence limits for ratios:","\n")
 }

}

else

{
if(x$alternative=="two.sided")
 {
  cat("Two-sided",x$conf.level*100, "%", "simultaneous confidence intervals for ratios:","\n")
 }
if(x$alternative=="less")
 {
  cat("Upper",x$conf.level*100, "%", "simultaneous confidence limits for ratios:","\n")
 }

if(x$alternative=="greater")
 {
  cat("Lower",x$conf.level*100, "%", "simultaneous confidence limits for ratios:","\n")
 }
}

cat("                                      ","\n")


if(x$NSD)
{
 print(cbind(estimate=x$estimate, x$conf.int) )

 cat("                                      ","\n")
 cat("   NSD = The mean in the denominator is not significantly different from zero. ","\n")
 cat("                                      ","\n") 
}
else
{
 print(cbind(estimate=x$estimate, x$conf.int), digits=digits )
}

}

