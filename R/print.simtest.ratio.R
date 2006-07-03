"print.simtest.ratio" <-
function(x,digits=4,...)
{
if(x$alternative=="two.sided")
 {cat("Alternative hypotheses: Ratios different from margins", "\n")}
if(x$alternative!="two.sided")
 {cat("Alternative hypotheses: Ratios ",x$alternative," than margins", "\n")}

out<-cbind( x$Margin.vec, x$estimate, x$teststat, x$p.value.raw, x$p.value.adj)

rownames(out) <- x$compnames
colnames(out) <- c("margin", "estimate", "statistic", "p.value.raw", "p.value.adj")

cat("","\n")
print(out, digits=digits)
cat("","\n")
}

