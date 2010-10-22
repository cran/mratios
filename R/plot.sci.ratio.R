"plot.sci.ratio" <-
function(x, rho0 = 1, rho0lty=2, rho0lwd=1, rho0col="black", CIvert=FALSE, CIlty = 1, CIlwd=1, CIcex=1, main=NULL, ylab=NULL, xlab=NULL, sub=NULL, ...)
{

old.par <- par(no.readonly=TRUE)

method <- x$method
conf.int <- x$conf.int
esti <- x$estimate
compn <- x$compnames
num <- 1:length(esti)
args <- list(...)
alternative <- x$alternative
conf.level <- x$conf.level
mymai <- par("mai")

if(is.null(sub))
{
 if(x$type=="User defined")
  {sub <- paste("User defined contrasts")}
 else
  {
   if(any(c("Marcus", "McDermott", "Williams")==x$type))
    {sub <- paste(x$type, "-like contrasts for ratios")}
   else
    {

     if(method=="UmbrellaWilliams"){sub <- paste("Umbrella-protected Williams contrasts")}
     if(method=="Changepoint"){sub <- paste("Changepoint contrasts")}
     if(method=="GrandMean"){sub <- paste("Comparisons to grand mean")}
     if(method=="AVE"){sub <- paste("Comparisons to average of others")}
     if(method=="Tukey"){sub <- paste("All pairwise comparisons")}
     if(method=="Dunnett"){sub <- paste("Many-to-one comparisons")}
     if(method=="Sequen"){sub <- paste("Sequence contrasts")}
    }
  }
}


if(x$method=="Plug")
 {mI <- "(method: Plug-in)"; mcp<-"simultaneous" }

if(x$method=="Bonf")
 {mI <- "(method: Bonferroni)"; mcp<-"simultaneous"   }

if(x$method=="MtI")
 {
  if(alternative=="two.sided")
   {mI <- "(method: Sidak)"; mcp<-"simultaneous" }
  else
   {mI <- "(method: Slepian)"; mcp<-"simultaneous" }
 } 

if(x$method=="Unadj")
 {mI <- ""; mcp<-"unadjusted"   }



if( alternative == "two.sided" )
 {

  lower <- conf.int[,1]; upper <- conf.int[,2] 

 if(any(lower=="NSD") || any(upper=="NSD"))
   {stop("Mean of control not significantly different from 0, no CI available")}
   
 # plot range:
 
 lplot <- min(lower, rho0)
 uplot <- max(upper, rho0)

# # # # rplot <- c(lplot,uplot) + c(-0.05,0.05)*c(uplot-lplot)

 }

  # extract the intervall bounds:

 if(alternative == "less")
  {
   upper <- conf.int[,1] 

 if(any(upper=="NSD"))
   {stop("Mean of control not significantly different from 0, no CI available")}
   
  lplot <- min(rho0, esti) 
  uplot <- max(upper, rho0)

  }

 if(alternative == "greater")
  {
   lower <- conf.int[,1] 

  if(any(lower=="NSD") )
   {stop("Mean of control not significantly different from 0, no CI available")}
   
  lplot <- min(rho0, lower) 
  uplot <- max(esti, rho0)


  }

 llplot <- lplot - 0.1*abs(lplot-uplot)
 uuplot <- uplot + 0.1*abs(lplot-uplot)
  
if(is.null(main))
 {if(alternative=="two.sided")
   {main <- paste(conf.level*100, "%",mcp," CI (two-sided) for ratios ",mI)}

  if(alternative=="less")
    {main <- paste(conf.level*100,  "%",mcp, " upper confidence limits for ratios ",mI)}

  if(alternative=="greater")
    {main <- paste("Lower ",conf.level*100, "%",mcp, " lower confidence limits for ratios ",mI)}
 }
 
if (is.null(ylab)) {ylab=""}
if (is.null(xlab)) {xlab=""}
### produce the plot:

# vertical CI:

if(CIvert==TRUE)
{

 plot.new()
  args <- list(...)
 # the default margin size in inches
  mymai <- par("mai")

 # adjust margin under the x axis according to length of comparison names
  xwidth<- 1.5 * max(strwidth(compn, units = "inches", cex = par("cex.axis"))) 

 if (mymai[1] < xwidth) 
        mymai[1] <- xwidth
 par(mai=mymai, new=TRUE)

plot(x = num, y = esti, axes = FALSE, ylim = c(llplot, uuplot)  , 
 type="p", pch=16, cex=CIcex,
 main=main,
 xlab="",
 ylab=ylab,
 sub=sub
 )


axis(side = 1, at = num, labels=compn, las=2, ... )
axis(side=2, ...)
box()

if(alternative=="two.sided")
{
 for(i in 1:length(num))
  {
  lines(x = c(num[i],num[i]), y = c(lower[i], upper[i]), lty = CIlty, lwd=CIlwd)
  points(x = num[i], y = lower[i], pch="-", cex = CIcex*1.5)
  points(x = num[i], y = upper[i], pch="-", cex = CIcex*1.5)
  }
 }


if(alternative=="less")
{
 for(i in 1:length(num))
  {
  lines(x = c(num[i],num[i]), y = c(llplot, upper[i]), lty = CIlty, lwd=CIlwd)
  points(x = num[i], y = upper[i], pch="-", cex = CIcex*1.5)
  }
 }


if(alternative=="greater")
{
 for(i in 1:length(num))
  {
  lines(x = c(num[i],num[i]), y = c(lower[i], uuplot), lty = CIlty, lwd=CIlwd)
  points(x = num[i], y = lower[i], pch="--", cex = CIcex*1.5)
  }
 }

abline(h=rho0, lty=rho0lty, lwd=rho0lwd, col=rho0col)

}


# horizontal CI:


if(CIvert==FALSE)
{

 plot.new()
  args <- list(...)
 # the default margin size in inches
  mymai <- par("mai")

 # adjust margin under the x axis according to length of comparison names
  ywidth<- 1.5 * max(strwidth(compn, units = "inches", cex = par("cex.axis"))) 

 if (mymai[2] < ywidth) 
        mymai[2] <- ywidth
 par(mai=mymai, new=TRUE)

plot(y = num, x = esti, axes = FALSE, xlim = c(llplot, uuplot), 
 type="p", pch=16, cex=CIcex,
 main=main,
 xlab=xlab,
 ylab="",
 sub=sub
 )


axis(side = 2, at = num, labels=compn, las=2, ...)
axis(side = 1, ...)
box()

if(alternative=="two.sided")
{
 for(i in 1:length(num))
  {
  lines(y = c(num[i],num[i]), x = c(lower[i], upper[i]), lty = CIlty, lwd=CIlwd)
  points(y = num[i], x = lower[i], pch="|", cex = CIcex*1.5)
  points(y = num[i], x = upper[i], pch="|", cex = CIcex*1.5)
  }
 }


if(alternative=="less")
{
 for(i in 1:length(num))
  {
  lines(y = c(num[i],num[i]), x = c(llplot, upper[i]), lty = CIlty, lwd=CIlwd)
  points(y = num[i], x = upper[i], pch="|", cex = CIcex*1.5)
  }
 }


if(alternative=="greater")
{
 for(i in 1:length(num))
  {
  lines(y = c(num[i],num[i]), x = c(lower[i], uuplot), lty = CIlty, lwd=CIlwd)
  points(y = num[i], x = lower[i], pch="|", cex = CIcex*1.5)
  }
 }

abline(v=rho0, lty=rho0lty, lwd=rho0lwd, col=rho0col)

}


par(old.par)


}

