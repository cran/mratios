"t.test.ratio.default" <-
function(x, y, alternative="two.sided", rho=1, var.equal=FALSE, conf.level=0.95, ...)

{ 
 addargs<-list(...)
 
 mx <- mean(x); my <- mean(y)
 nx <- length(x); ny <- length(y)
 vx <- var(x); vy <- var(y)
 est <- mx/my
 
 if(is.null(addargs$namex) || is.null(addargs$namey))
  {
   namex="x"
   namey="y"
  }
 else
  {
   namex=addargs$namex
   namey=addargs$namey
  }
 
 # # # t.test.ratio
 
 if(var.equal==TRUE)
  {
    degf <- nx+ny-2
    spool <- sqrt( (vx*(nx-1) + vy*(ny-1)) / degf )
    
    statistic<-(mx-my*rho)/(spool*sqrt( 1/nx + (rho^2)/ny) )
 
    if(alternative=="less")
     {p.value<-pt(q=statistic, df=degf, lower.tail=TRUE)
      alpha <- (1-conf.level)}
 
    if(alternative=="greater")
     {p.value<-pt(q=statistic, df=degf, lower.tail=FALSE)
      alpha <- (1-conf.level)}   
 
    if(alternative=="two.sided")
     {p.value<-min(1, 2*pt(q=abs(statistic), df=degf, lower.tail=FALSE))
      alpha <- (1-conf.level)/2}   

   method<-"Ratio-t-test for equal variances"
 
 
  vpool <- (vx*(nx-1) + vy*(ny-1)) / degf 
  
  quant <- qt(p=1-alpha, df=degf, lower.tail=TRUE)
 
  tA <- ( (vpool*quant^2)/ny ) - my^2
  tB <- 2*mx*my
  tC <- ((vpool*quant^2)/nx ) - mx^2
 
  if(tA>=0)
   {
    upper<-"NSD"
    lower<-"NSD"
   }
  else
   {
   upper <- tB/(-2*tA) - sqrt( ((tB/2)^2)- tA*tC)/tA
   lower <- tB/(-2*tA) + sqrt( ((tB/2)^2)- tA*tC)/tA
   }
 
 
  }
 
 # # # Tamhane and Logan 2004
 
 
 if(var.equal==FALSE)
  {
 
    degf <- (( vx/nx + (rho^2)*vy/ny )^2) / ( (vx^2)/((nx^2)*(nx-1)) + (rho^4)*(vy^2)/((ny^2)*(ny-1)) )
 
    stderr <- sqrt(vx/nx + (rho^2)*vy/ny)
    
    statistic<-(mx-my*rho)/stderr
 
    if(alternative=="less")
     {p.value<-pt(q=statistic, df=degf, lower.tail=TRUE); alpha <- (1-conf.level)}
 
    if(alternative=="greater")
     {p.value<-pt(q=statistic, df=degf, lower.tail=FALSE); alpha <- (1-conf.level)}   
 
    if(alternative=="two.sided")
     {p.value<-min(1, 2*pt(q=abs(statistic), df=degf, lower.tail=FALSE)); alpha <- (1-conf.level)/2}   
    method<-"Ratio t-test for unequal variances"
  # Confidence interval: approx Satterthwaite degree of freedom using the estimated ratio !!
 
   quant <- qt(p=1-alpha, df=degf, lower.tail=TRUE)
 
  tA <- ( (vy*quant^2)/ny ) - my^2
  tB <- 2*mx*my
  tC <- ((vx*quant^2)/nx ) - mx^2 
  if(tA>=0)
   {
    upper<-"NSD"
    lower<-"NSD"
   }
  else
   {
   upper <- tB/(-2*tA) - sqrt( ((tB/2)^2)- tA*tC)/tA
   lower <- tB/(-2*tA) + sqrt( ((tB/2)^2)- tA*tC)/tA
   }
 
  }
 
 if(alternative=="two.sided")
  {conf.int<-c(lower, upper)}
   else
    {
     if(alternative=="less")
      {conf.int<-c(-Inf, upper)}
       else
        {
        if(alternative=="greater")
        {conf.int<-c(lower, Inf)}
  }}
 
 names(statistic)<-"t"
 estimate<-c(mx,my,est)
 names(estimate)<-c(paste("mean",namex), paste("mean",namey), paste(namex, namey, sep="/") )
 names(degf)<-"df"
 names(rho)<-"ratio of means" 
 data.name<-paste(namex,namey, sep=" and ")
 
 if(any(conf.int=="NSD"))
  {cat("Mean of denominator group is not significantly different from zero","\n")}
  
 
 out<-list(
 
 statistic=statistic,
 parameter=degf,
 p.value=p.value,
 conf.int=conf.int,
 estimate=estimate,
 null.value=rho,
 alternative=alternative,
 method=method,
 data.name=data.name
 )
 
 class(out)<-"htest"
 
 return(out)
 }

