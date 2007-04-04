"sci.ratioI" <-
function(Response, Treatment, Num.Contrast, Den.Contrast, alternative = 'two.sided', conf.level = 0.95, 
method="Plug") {

         
CMat <- Num.Contrast
DMat <- Den.Contrast


n.Treat <- tapply(Response,Treatment,length)
Mean.Treat <- tapply(Response,Treatment,mean)
Var.Treat <- tapply(Response,Treatment,var)

degree.f <- sum(n.Treat-1) 

Pooled.Var <- sum( (n.Treat - 1)*Var.Treat)/degree.f

M <- diag(1/n.Treat)  #  Diagonal matrix containing reciprocals of the ni"s

n.comp <- nrow (CMat)     # Number of comparisons 


# print(cbind(n.Treat,Mean.Treat))

gammaC.vec <- CMat%*%Mean.Treat/DMat%*%Mean.Treat   #  MLE of the ratios

CorrMat.plug <- matrix(rep(NA,n.comp*n.comp),nr=n.comp)
    for(i in 1:n.comp) {
        for(j in 1:n.comp) {
            CorrMat.plug[i,j] <- (gammaC.vec[i]*DMat[i,] - CMat[i,])%*%M%*%(gammaC.vec[j]*DMat[j,] - CMat[j,])/
            (sqrt((gammaC.vec[i]*DMat[i,] - CMat[i,])%*%M%*%(gammaC.vec[i]*DMat[i,] - CMat[i,]))*
             sqrt((gammaC.vec[j]*DMat[j,] - CMat[j,])%*%M%*%(gammaC.vec[j]*DMat[j,] - CMat[j,])))
        }
    }



Quad.root <- function(Aj, Bj, Cj){
        Discrimi <- Bj^2 - 4*Aj*Cj   
        if ((Aj > 0)&(Discrimi >= 0)) Limit.s <- (-Bj + plus.minus*sqrt(Discrimi))/(2*Aj)
        else  Limit.s <- "NSD"
        return(Limit.s)}

if (alternative=="two.sided"){ 
    side <- 2
    plus.minus <- c(-1,1)
    

      #
      # UNADJUSTED
      #
    cpUAd <- qt(1- (1-conf.level)/(side), degree.f, lower.tail = TRUE)
      #
      # BONFERRONI
      #
    cpBon <- qt(1- (1-conf.level)/(side*n.comp), degree.f, lower.tail = TRUE)
      #
      #  MtI
      #
    cpMtI <- qmvt(conf.level, interval=c(0,10),df=degree.f,corr=diag(n.comp),delta=rep(0,n.comp), tail="both", abseps=1e-05)$quantile
      #
      #  Plug-in
      #
    Cplug <- qmvt(conf.level, interval=c(0,10),df=degree.f,corr=CorrMat.plug,delta=rep(0,n.comp), tail="both", abseps=1e-05)$quantile
    
   } # End of two-sided CI
    
if ((alternative=="less")|(alternative=="greater")){
    side <- 1
    if (alternative=="less") plus.minus <- 1
    else plus.minus <- -1

      #
      # UNADJUSTED
      #
    cpUAd <- qt(1- (1-conf.level)/(side), degree.f, lower.tail = TRUE)
      #
      # BONFERRONI
      #
    cpBon <- qt(1- (1-conf.level)/(side*n.comp), degree.f, lower.tail = TRUE)
      #
      #  MtI
      #
    cpMtI <- qmvt(conf.level, interval=c(0,10),df=degree.f,corr=diag(n.comp),delta=rep(0,n.comp), 
tail="lower.tail", abseps=1e-05)$quantile
      #
      #  Plug-in
      #
    Cplug <- qmvt(conf.level, interval=c(0,10),df=degree.f,corr=CorrMat.plug,delta=rep(0,n.comp), 
tail="lower.tail", abseps=1e-05)$quantile
   
    } # End of one-sided CI    
    
# Confidence intervals

switch(method,

# UNADJUSTED CI:


Unadj = 
{
UAdCL <- matrix(rep(NA,side*n.comp),nr=n.comp)
for(j in 1:n.comp)
 {
                  AjUAd <- (DMat[j,]%*%Mean.Treat)^2 - (cpUAd^2)*Pooled.Var*DMat[j,]%*%M%*%DMat[j,]
                  BjUAd <- -2*((CMat[j,]%*%Mean.Treat)*(DMat[j,]%*%Mean.Treat) - 
(cpUAd^2)*Pooled.Var*CMat[j,]%*%M%*%DMat[j,])
                  CjUAd <- (CMat[j,]%*%Mean.Treat)^2 - (cpUAd^2)*Pooled.Var*CMat[j,]%*%M%*%CMat[j,]
    UAdCL[j,]  <- Quad.root(AjUAd, BjUAd,  CjUAd)
 }
  
sci.table <- data.frame( UAdCL)  

},
# Bonferroni-adjustment


Bonf = 
{
BonCL <- matrix(rep(NA,side*n.comp),nr=n.comp)
for(j in 1:n.comp)
 {
                  AjBon <- (DMat[j,]%*%Mean.Treat)^2 - (cpBon^2)*Pooled.Var*DMat[j,]%*%M%*%DMat[j,]
                  BjBon <- -2*((CMat[j,]%*%Mean.Treat)*(DMat[j,]%*%Mean.Treat) - 
(cpBon^2)*Pooled.Var*CMat[j,]%*%M%*%DMat[j,])
                  CjBon <- (CMat[j,]%*%Mean.Treat)^2 - (cpBon^2)*Pooled.Var*CMat[j,]%*%M%*%CMat[j,]
    BonCL[j,]  <- Quad.root(AjBon, BjBon,  CjBon)
 }
  
sci.table <- data.frame(BonCL)  

},

# MtI: Sidak or Slepian for two-sided or one-sided CI

MtI = 
{
MtICL <- matrix(rep(NA,side*n.comp),nr=n.comp)

for(j in 1:n.comp)
 {

                  AjMtI <- (DMat[j,]%*%Mean.Treat)^2 - (cpMtI^2)*Pooled.Var*DMat[j,]%*%M%*%DMat[j,]
                  BjMtI <- -2*((CMat[j,]%*%Mean.Treat)*(DMat[j,]%*%Mean.Treat) - 
(cpMtI^2)*Pooled.Var*CMat[j,]%*%M%*%DMat[j,])
                  CjMtI <- (CMat[j,]%*%Mean.Treat)^2 - (cpMtI^2)*Pooled.Var*CMat[j,]%*%M%*%CMat[j,]
    MtICL[j,]  <- Quad.root(AjMtI, BjMtI,  CjMtI)
    
 } 

sci.table <- data.frame(MtICL)

},

# Plug in of ratio estimates

Plug = 
{
 PlugCL <- matrix(rep(NA,side*n.comp),nr=n.comp)

for(j in 1:n.comp)
 {
                  AjPlug <- (DMat[j,]%*%Mean.Treat)^2 - (Cplug^2)*Pooled.Var*DMat[j,]%*%M%*%DMat[j,]
                  BjPlug <- -2*((CMat[j,]%*%Mean.Treat)*(DMat[j,]%*%Mean.Treat) - 
(Cplug^2)*Pooled.Var*CMat[j,]%*%M%*%DMat[j,])
                  CjPlug <- (CMat[j,]%*%Mean.Treat)^2 - (Cplug^2)*Pooled.Var*CMat[j,]%*%M%*%CMat[j,]
   PlugCL[j,] <- Quad.root(AjPlug, BjPlug,  CjPlug)
 } 

sci.table <- data.frame(PlugCL)

}
)



if (alternative=="two.sided")
{
names(sci.table) <- c("lower","upper")
}

if (alternative=="less")
{
names(sci.table) <- c("upper")
}

if (alternative=="greater")
{
names(sci.table) <- c("lower")
}

if( any(CorrMat.plug<0) && method=="MtI" && alternative!="two.sided")
 {
  cat("Warning: At least one element of the estimated correlation matrix is negative,","\n",
  "therefore, according to Slepian inequality, the MtI method might yield incorrect estimates.","\n")
 }

if (sum(sci.table=="NSD")>0){NSD <- TRUE}
 else{NSD <- FALSE}

list(
estimate=gammaC.vec,
CorrMat.est=CorrMat.plug,
Num.Contrast=CMat,
Den.Contrast=DMat,
conf.int=sci.table,
NSD=NSD,
method=method,
alternative=alternative,
conf.level=conf.level
)

} # END OF sci.ratioI

