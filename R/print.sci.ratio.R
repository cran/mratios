"print.sci.ratio" <-
function(x, digits=4,...)
{
cat("                                      ","\n")
cat(x$methodname)
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
 print(round(cbind(estimate=x$estimate, x$conf.int), digits=digits) )
}
invisible(x)
}

