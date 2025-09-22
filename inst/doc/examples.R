## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(fig.width = 12, fig.height=8, fig.align="default")
knitr::opts_chunk$set(error = TRUE)

## ----hide=TRUE----------------------------------------------------------------
oldpar <- par(no.readonly=TRUE)
set.seed(123)
library(BGVAR)

## ----"eerData"----------------------------------------------------------------
data(eerData)

## ----"eerData2"---------------------------------------------------------------
names(eerData)

## ----"eerData3"---------------------------------------------------------------
colnames(eerData$UK)

## ----"US",echo=TRUE-----------------------------------------------------------
head(eerData$US)

## ----"convert",echo=TRUE------------------------------------------------------
bigX<-list_to_matrix(eerData)

## ----"convert2",echo=TRUE-----------------------------------------------------
colnames(bigX)[1:10]

## ----"tradeW",echo=TRUE-------------------------------------------------------
head(W.trade0012)

## ----"rownames.W"-------------------------------------------------------------
all(colnames(W.trade0012)==names(eerData))

## ----"rowSums.W"--------------------------------------------------------------
rowSums(W.trade0012)
diag(W.trade0012)

## ----"eerDatasmall", hide=TRUE------------------------------------------------
cN<-c("EA","US","RU")
eerData<-eerData[cN]
W.trade0012<-W.trade0012[cN,cN]
W.trade0012<-apply(W.trade0012,2,function(x)x/rowSums(W.trade0012))
W.list<-lapply(W.list,function(l){l<-apply(l[cN,cN],2,function(x)x/rowSums(l[cN,cN]))})

## ----"export excel", eval=FALSE-----------------------------------------------
# time <- as.character(seq.Date(as.Date("1995-01-01"),as.Date("2013-10-01"),by="quarter"))
# 
# for(cc in 1:length(eerData)){
#   x <- coredata(eerData[[cc]])
#   rownames(x) <- time
#   write.xlsx(x = x, file="./excel_eerData.xlsx", sheetName = names(eerData)[cc],
#              col.names=TRUE, row.names=TRUE, append=TRUE)
# }

## ----"import excel", eval=FALSE-----------------------------------------------
# eerData_read <- excel_to_list(file = "./excel_eerData.xlsx", first_column_as_time=TRUE, skipsheet=NULL, ...)

## ----"transform to matrix", eval=FALSE----------------------------------------
# eerData_matrix <- list_to_matrix(eerData_read)
# eerData_list <- matrix_to_list(eerData_matrix)

## ----"model.1",results="hide"-------------------------------------------------
 model.1<-bgvar(Data=eerData,
                W=W.trade0012,
                draws=100,
                burnin=100,
                plag=1,
                prior="NG",
                hyperpara=NULL, 
                SV=TRUE,
                thin=1,
                trend=TRUE,
                hold.out=0,
                eigen=1
                )

## ----"SV",results="hide"------------------------------------------------------
model.1$cc.results$sig$EA[,"EA.y","EA.y"]

## ----"ng.eigen",echo=TRUE-----------------------------------------------------
model.1$stacked.results$F.eigen[1:10]

## ----"print.model",echo=TRUE--------------------------------------------------
print(model.1)

## ----"summary.model"----------------------------------------------------------
 summary(model.1)

## ----"stats",echo=TRUE, results="hide"----------------------------------------
Fmat <- coef(model.1)
Smat <- vcov(model.1)
lik  <- logLik(model.1)

## ----"insample",fig.margin=TRUE,fig.width=6,fig.height=8,fig.cap="In-sample fit for euro area variables"----
yfit <- fitted(model.1)
plot(model.1, global=FALSE, resp="EA")

## ----"ssvs.1",echo=TRUE, results="hide"---------------------------------------
model.ssvs.1<-bgvar(Data=eerData,
                    W=W.trade0012,
                    draws=100,
                    burnin=100,
                    plag=1,
                    prior="SSVS",
                    hyperpara=NULL, 
                    SV=TRUE,
                    thin=1,
                    Ex=NULL,
                    trend=TRUE,
                    expert=list(save.shrink.store=TRUE),
                    hold.out=0,
                    eigen=1,
                    verbose=TRUE
                    )

## ----"Pips"-------------------------------------------------------------------
model.ssvs.1$cc.results$PIP$PIP.cc$EA

## ----"pips.avg"---------------------------------------------------------------
model.ssvs.1$cc.results$PIP$PIP.avg

## ----"var.weight"-------------------------------------------------------------
variable.list<-list();variable.list$real<-c("y","Dp","tb");variable.list$fin<-c("stir","ltir","rer")

## ----results="hide"-----------------------------------------------------------
# weights for first variable set tradeW.0012, for second finW0711
model.ssvs.2<-bgvar(Data=eerData,
                    W=W.list[c("tradeW.0012","finW0711")],
                    plag=1,
                    draws=100,
                    burnin=100,
                    prior="SSVS",
                    SV=TRUE,
                    eigen=1,
                    expert=list(variable.list=variable.list,save.shrink.store=TRUE),
                    trend=TRUE
                    )

## ----"ltir.estimate", results="hide"------------------------------------------
# does include ltir* only when ltir is missing domestically
model.ssvs.3<-bgvar(Data=eerData,
                    W=W.trade0012,
                    plag=1,
                    draws=100,
                    burnin=100,
                    prior="SSVS",
                    SV=TRUE,
                    eigen=1,
                    expert=list(Wex.restr="ltir",save.shrink.store=TRUE),
                    trend=TRUE,
                    )

## ----"print.model.ssvs.3"-----------------------------------------------------
 print(model.ssvs.3)

## ----"OC"---------------------------------------------------------------------
eerData2<-eerData
eerData2$OC<-eerData$US[,c("poil"),drop=FALSE] # move oil prices into own slot
eerData2$US<-eerData$US[,c("y","Dp", "rer" , "stir", "ltir","tb")] # exclude it from US m odel

## ----"OC.weights"-------------------------------------------------------------
OC.weights<-list()
OC.weights$weights<-rep(1/3, 3)
names(OC.weights$weights)<-names(eerData2)[1:3] # last one is OC model, hence only until 3
OC.weights$variables<-c(colnames(eerData2$OC),"y") # first entry, endog. variables, second entry weighted average of y from the other countries to proxy demand
OC.weights$exo<-"poil"

## ----"OC.weights2"------------------------------------------------------------
# other entities weights with same name as new oil country
OE.weights <- list(OC=OC.weights)

## ----"estimate.OC",results="hide"---------------------------------------------
model.ssvs.4<-bgvar(Data=eerData2,
                    W=W.trade0012,
                    plag=1,
                    draws=100,
                    burnin=100,
                    prior="SSVS",
                    SV=TRUE,
                    expert=list(OE.weights=OE.weights,save.shrink.store=TRUE),
                    trend=TRUE
                    )

## ----"aux"--------------------------------------------------------------------
aux1<-model.ssvs.1$cc.results$PIP$PIP.avg;aux1<-aux1[-nrow(aux1),1:6]
aux2<-model.ssvs.2$cc.results$PIP$PIP.avg;aux2<-aux2[-nrow(aux2),1:6]
aux3<-model.ssvs.3$cc.results$PIP$PIP.avg;aux3<-aux3[-nrow(aux3),1:6]
aux4<-model.ssvs.4$cc.results$PIP$PIP.avg;aux4<-aux4[-nrow(aux4),1:6]

## ----"heat1", fig.show="hold",out.width="25%",fig.cap="Heatmaps of PIPs."-----
heatmap(aux1,Rowv=NA,Colv=NA, main="Model 1", cex.main=2, cex.axis=1.7)
heatmap(aux2,Rowv=NA,Colv=NA, main="Model 2", cex.main=2, cex.axis=1.7)
heatmap(aux3,Rowv=NA,Colv=NA, main="Model 3", cex.main=2, cex.axis=1.7)
heatmap(aux4,Rowv=NA,Colv=NA, main="Model 4", cex.main=2, cex.axis=1.7)

## ----"shocks", results="hide"-------------------------------------------------
irf.chol<-irf(model.ssvs.1, n.ahead=24, expert=list(save.store=FALSE))

## ----"us.mp", results="hide"--------------------------------------------------
# US monetary policy shock - Cholesky
shockinfo_chol<-get_shockinfo("chol")
shockinfo_chol$shock<-"US.stir"
shockinfo_chol$scale<--1 # corresponds to 1 percentage point or 100bp
# US monetary policy shock - GIRF
shockinfo_girf<-get_shockinfo("girf")
shockinfo_girf$shock<-"US.stir"
shockinfo_girf$scale<--1 # corresponds to 1 percentage point or 100bp

## ----"shockinfo"--------------------------------------------------------------
shockinfo_chol
shockinfo_girf

## ----"us.mp.chol", results="hide"---------------------------------------------
irf.chol.us.mp<-irf(model.ssvs.1, n.ahead=24, shockinfo=shockinfo_chol, expert=list(save.store=TRUE))

## ----"us.mp2"-----------------------------------------------------------------
names(irf.chol.us.mp)

## ----"us.mp4", fig.margin=TRUE,out.width="80%",fig.cap="Responses of US country model"----
plot(irf.chol.us.mp, resp="US", shock="US.stir")

## ----"us.gdp", results="hide"-------------------------------------------------
# cholesky
shockinfo_chol       <- get_shockinfo("chol", nr_rows = 2)
shockinfo_chol$shock <- c("US.stir","US.y")
shockinfo_chol$scale <- c(1,1)
# generalized impulse responses
shockinfo_girf       <- get_shockinfo("girf", nr_rows = 2)
shockinfo_girf$shock <- c("US.stir","US.y")
shockinfo_girf$scale <- c(1,1)
# Recursive US GDP
irf.chol.us.y<-irf(model.ssvs.1, n.ahead=24, shockinfo=shockinfo_chol)
# GIRF US GDP
irf.girf.us.y<-irf(model.ssvs.1, n.ahead=24, shockinfo=shockinfo_girf)

## ----"us.gdp.plots",fig.cap="Comparison of responses Cholesky (left) and GIRF (right) to a negative GDP shock.",fig.show="hold",out.width="25%"----
plot(irf.chol.us.y, resp="US.y", shock="US.y")
plot(irf.girf.us.y, resp="US.y", shock="US.y")
plot(irf.chol.us.y, resp="US.rer", shock="US.y")
plot(irf.girf.us.y, resp="US.rer", shock="US.y")

## ----"global.gdp",results="hide",out.width="50%"------------------------------
shockinfo<-get_shockinfo("girf", nr_rows = 3)
shockinfo$shock<-c("EA.y","US.y","RU.y")
shockinfo$global<-TRUE
shockinfo$scale<--1
irf.global<-irf(model.ssvs.1, n.ahead=24, shockinfo=shockinfo)
plot(irf.global, resp=c("US.y","EA.y","RU.y"), shock="Global.y")

## ----hide=TRUE----------------------------------------------------------------
data("eerData")
eerData<-eerData[cN]
W.trade0012<-W.trade0012[cN,cN]
W.trade0012<-apply(W.trade0012,2,function(x)x/rowSums(W.trade0012))
# append expectations data to US model
temp <- cbind(USexpectations, eerData$US)
colnames(temp) <- c(colnames(USexpectations),colnames(eerData$US))
eerData$US <- temp

## ----"us.spf", results="hide"-------------------------------------------------
model.ssvs.eer<-bgvar(Data=eerData,
                      W=W.trade0012,
                      plag=1,
                      draws=100,
                      burnin=100,
                      prior="SSVS",
                      SV=TRUE)

## ----"us.spf.sign.spec"-------------------------------------------------------
shockinfo<-get_shockinfo("sign")
shockinfo<-add_shockinfo(shockinfo, shock="US.y", 
                         restriction="US.Dp", sign=">", horizon=1, prob=1, scale=1)
shockinfo<-add_shockinfo(shockinfo, shock="US.Dp",
                         restriction="US.y", sign="<", horizon=1, prob=1, scale=1)

## ----"us.spf.sign",message=FALSE, results="hide"------------------------------
irf.sign<-irf(model.ssvs.eer, n.ahead=24, shockinfo=shockinfo, 
              expert=list(MaxTries=100, save.store=FALSE, cores=NULL))

## ----"us.spf.sign2"-----------------------------------------------------------
irf.sign$rot.nr

## ----"us.spf.plots",fig.cap="Responses to AS (upper panel) and AD (lower panel) shock.",fig.show="hold",out.width="50%"----
plot(irf.sign, resp=c("US.y","US.Dp"), shock="US.y")
plot(irf.sign, resp=c("US.y","US.Dp"), shock="US.Dp")

## ----"us.spf.sign3",results="hide"--------------------------------------------
shockinfo<-get_shockinfo("sign")
shockinfo<-add_shockinfo(shockinfo, shock="US.stir_t+4",
                         restriction=c("US.Dp_t+4","US.stir","US.y_t+4","US.stir_t+4","US.Dp_t+4","US.y_t+4"),
                         sign=c("<","0","<","ratio.avg","ratio.H","ratio.H"),
                         horizon=c(1,1,1,5,5,5),
                         prob=1, scale=1)
irf.sign.zero<-irf(model.ssvs.eer, n.ahead=20, shockinfo=shockinfo, 
                   expert=list(MaxTries=100, save.store=TRUE))

## ----"eer.spf.plots",fig.cap="Rationality conditions I.",out.width="50%",fig.show="hold"----
# rationality condition: US.stir_t+4 on impact is equal to average of IRF of 
# US.stir between horizon 2 and 5
matplot(cbind(irf.sign.zero$IRF_store["US.stir_t+4",1,,1],
              irf.sign.zero$IRF_store["US.stir",1,,1]),
        type="l",ylab="",main="Short-term Interest Rate",lwd=2,xaxt="n", cex.main=2);
axis(side=1,at=c(1:5,9,13,17,21,25),label=c(0:4,8,12,16,20,24), cex.axis=1.7)
legend("topright",lty=c(1,2),c("expected","actual"),lwd=2,bty="n",col=c("black","red"))
segments(x0=2,y0=1,x1=5,y1=1,lwd=2,lty=3,col="grey")
points(1,1,col="grey",pch=19,lwd=4)
abline(v=c(2,5),lty=3,col="grey",lwd=2)
# rationality condition: US.y_t+4 on impact is equal to H-step ahead IRF 
# of US.y in horizon 5
matplot(cbind(irf.sign.zero$IRF_store["US.y_t+4",1,,1],
              irf.sign.zero$IRF_store["US.y",1,,1]),
        type="l",ylab="",main="Output",lwd=2,xaxt="n", cex.main=2)
axis(side=1,at=c(1:5,9,13,17,21,25),label=c(0:4,8,12,16,20,24), cex.axis=1.7)
legend("topright",lty=c(1,2),c("expected","actual"),lwd=2,bty="n",col=c("black","red"))
yy<-irf.sign.zero$IRF_store["US.y_t+4",1,1,1]
segments(x0=1,y0=yy,x1=5,y1=yy,lwd=2,lty=3,col="grey");abline(v=c(1,5),col="grey",lty=3)
points(1,yy,col="grey",pch=19,lwd=4);points(5,yy,col="grey",pch=19,lwd=4)

## ----"ea.data"----------------------------------------------------------------
data(monthlyData);monthlyData$OC<-NULL
names(monthlyData)
# list of weights of other entities with same name as additional country model
OE.weights = list(EB=EB.weights)
EA_countries <- c("AT", "BE", "DE","ES", "FI","FR")
                  # "IE", "IT", "NL", "PT","GR","SK","MT","CY","EE","LT","LV")

## ----"restrict_sample", hide=TRUE---------------------------------------------
monthlyData <- monthlyData[c(EA_countries,"EB")]
W<-W[EA_countries,EA_countries]
W<-apply(W,2,function(x)x/rowSums(W))
OE.weights$EB$weights <- OE.weights$EB$weights[names(OE.weights$EB$weights)%in%EA_countries]

## ----"ea.estimate", results="hide"--------------------------------------------
# estimates the model
model.ssvs<-bgvar(Data=monthlyData,
                  W=W,
                  draws=200,
                  burnin=200,
                  plag=1,
                  prior="SSVS",
                  eigen=1.05,
                  expert=list(OE.weights=OE.weights))

## ----"ea.sign"----------------------------------------------------------------
# imposes sign restrictions on the cross-section and for a global shock
# (long-term interest rates)
shockinfo<-get_shockinfo("sign")
for(cc in c("AT","BE","FR")){
  shockinfo<-add_shockinfo(shockinfo, shock=paste0(cc,".ltir"),
                           restriction=paste0(cc,c(".ip",".p")),
                           sign=c("<","<"), horizon=c(1,1), 
                           prob=c(0.5,0.5), scale=c(-100,-100),
                           global=TRUE)
}

## ----"global.restrictions"----------------------------------------------------
shockinfo

## ----"global.shock.irf",echo=TRUE,results="hide"------------------------------
irf.sign.ssvs<-irf(model.ssvs, n.ahead=24, shockinfo=shockinfo, expert=list(MaxTries=500))

## ----"ea.sign.verify"---------------------------------------------------------
irf.sign.ssvs$posterior[paste0(EA_countries[-c(3,12)],".ltir"),1,1,"Q50"]
irf.sign.ssvs$posterior[paste0(EA_countries,".ip"),1,1,"Q50"]
irf.sign.ssvs$posterior[paste0(EA_countries,".p"),1,1,"Q50"]

## ----"ea.sign.plots",fig.show="hold",out.width="25%",fig.cap="Output responses of selected euro area countries."----
plot(irf.sign.ssvs, resp=c("AT.ip"), shock="Global.ltir")
plot(irf.sign.ssvs, resp=c("BE.ip"), shock="Global.ltir")
plot(irf.sign.ssvs, resp=c("DE.ip"), shock="Global.ltir")
plot(irf.sign.ssvs, resp=c("ES.ip"), shock="Global.ltir")

## ----"fevd"-------------------------------------------------------------------
#calculates the LN GFEVD 
gfevd.us.mp=gfevd(model.ssvs.eer,n.ahead=24,running=TRUE,cores=4)$FEVD
# get position of EA 
idx<-which(grepl("EA.",dimnames(gfevd.us.mp)[[2]]))
own<-colSums(gfevd.us.mp["EA.y",idx,])
foreign<-colSums(gfevd.us.mp["EA.y",-idx,])

## ----"fevd.plot",fig.cap="FEVD of EA GDP.",out.width="50%"--------------------
barplot(t(cbind(own,foreign)),legend.text =c("own","foreign"))

## ----"fevd.struc"-------------------------------------------------------------
# calculates FEVD for variables US.y
fevd.us.y=fevd(irf.chol.us.mp, var.slct=c("US.y"))$FEVD
idx<-which(grepl("US.",rownames(fevd.us.y)))

## ----"fevd.struc.plot",fig.cap="FEVD of US GDP.",out.width="50%"--------------
barplot(fevd.us.y[idx,1,])

## ----"hd"---------------------------------------------------------------------
HD<-hd(irf.chol.us.mp)
# summing them up should get you back the original time series
org.ts<-apply(HD$hd_array,c(1,2),sum) # this sums up the contributions of all shocks + constant, initial conditions and residual component (last three entries in the third dimension of the array)

## ----"hd.plot",fig.cap="Historical decomposition of euro area GDP.",out.width="50%"----
matplot(cbind(HD$x[,1],org.ts[,1]),type="l",ylab="",lwd=2, cex.axis=1.7)
legend("bottomright",c("hd series","original"),col=c("black","red"),lty=c(1,2),bty="n",cex=2)

## ----"fcast.est", results="hide"----------------------------------------------
model.ssvs.h8<-bgvar(Data=eerData,
                     W=W.trade0012,
                     draws=500,
                     burnin=500,
                     plag=1,
                     prior="SSVS",
                     hyperpara=NULL, 
                     SV=TRUE,
                     thin=1,
                     trend=TRUE,
                     hold.out=8,
                     eigen=1
                     )

## ----"fcast.predict", results="hide"------------------------------------------
fcast <- predict(model.ssvs.h8, n.ahead=8, save.store=TRUE)

## ----"lps"--------------------------------------------------------------------
lps.h8 <- lps(fcast)
rmse.h8 <- rmse(fcast)

## ----"fcast.plot",fig.cap="Forecast plot.",out.width="50%"--------------------
plot(fcast, resp="US.Dp", cut=8)

## ----"cond.predict",results="hide"--------------------------------------------
# matrix with constraints
constr <- matrix(NA,nrow=fcast$n.ahead,ncol=ncol(model.ssvs.h8$xglobal))
colnames(constr) <- colnames(model.ssvs.h8$xglobal)
# set "US.Dp" for five periods on its last value
constr[1:5,"US.Dp"] <-model.ssvs.h8$xglobal[nrow(model.ssvs.h8$xglobal),"US.Dp"]
# compute conditional forecast (hard restriction)
cond_fcast <- predict(model.ssvs.h8, n.ahead=8, constr=constr, constr_sd=NULL)

## ----"cond.predict.sd",results="hide"-----------------------------------------
# add uncertainty to conditional forecasts
constr_sd <- matrix(NA,nrow=fcast$n.ahead,ncol=ncol(model.ssvs.h8$xglobal))
colnames(constr_sd) <- colnames(model.ssvs.h8$xglobal)
constr_sd[1:5,"US.Dp"] <- 0.001
# compute conditional forecast with soft restrictions
cond_fcast2 <- predict(model.ssvs.h8, n.ahead=8, constr=constr, constr_sd=constr_sd)

## ----"cond.plot.1",out.width="50%",fig.show="hold",fig.cap="Conditional forecast of US Inflation, top panel without uncertainty during the conditioning, bottom panel with uncertainty."----
plot(cond_fcast, resp="US.Dp", cut=10)
plot(cond_fcast2, resp="US.Dp", cut=10)

## ----eval=FALSE---------------------------------------------------------------
# # load dataset
# data(eerData)
# # Minnesota prior and two different weight matrices and no SV
# # weights for first variable set tradeW.0012, for second finW0711
# variable.list      <- list()
# variable.list$real <- c("y","Dp","tb")
# variable.list$fin  <- c("stir","ltir","rer")
# Hyperparm.MN <- list(a_i = 0.01, # prior for the shape parameter of the IG
#                      b_i = 0.01  # prior for the scale parameter of the IG
#                      )
# model.MN<-bgvar(Data=eerData,
#                   W=W.list[c("tradeW.0012","finW0711")],
#                   draws=200,
#                   burnin=200,
#                   plag=1,
#                   hyperpara=Hyperparm.MN,
#                   prior="MN",
#                   thin=1,
#                   eigen=TRUE,
#                   SV=TRUE,
#                   expert=list(variable.list=variable.list))
# # SSVS prior
# Hyperparm.ssvs <- list(tau0   = 0.1,  # coefficients: prior variance for the spike
#                                       # (tau0 << tau1)
#                        tau1   = 3,    # coefficients: prior variance for the slab
#                                       # (tau0 << tau1)
#                        kappa0 = 0.1,  # covariances: prior variance for the spike
#                                       # (kappa0 << kappa1)
#                        kappa1 = 7,    # covariances: prior variance for the slab
#                                       # (kappa0 << kappa1)
#                        a_1    = 0.01, # prior for the shape parameter of the IG
#                        b_1    = 0.01, # prior for the scale parameter of the IG
#                        p_i    = 0.5,  # prior inclusion probability of coefficients
#                        q_ij   = 0.5   # prior inclusion probability of covariances
#                        )
# model.ssvs<-bgvar(Data=eerData,
#                   W=W.trade0012,
#                   draws=100,
#                   burnin=100,
#                   plag=1,
#                   hyperpara=Hyperparm.ssvs,
#                   prior="SSVS",
#                   thin=1,
#                   eigen=TRUE)
# # Normal Gamma prior
# data(monthlyData)
# monthlyData$OC<-NULL
# Hyperparm.ng<-list(d_lambda   = 1.5,  # coefficients: prior hyperparameter for the NG-prior
#                    e_lambda   = 1,    # coefficients: prior hyperparameter for the NG-prior
#                    prmean     = 0,    # prior mean for the first lag of the AR coefficients
#                    a_1        = 0.01, # prior for the shape parameter of the IG
#                    b_1        = 0.01, # prior for the scale parameter of the IG
#                    tau_theta  = .6,   # (hyper-)parameter for the NG
#                    sample_tau = FALSE # estimate a?
#                    )
# model.ng<-bgvar(Data=monthlyData,
#                 W=W,
#                 draws=200,
#                 burnin=100,
#                 plag=1,
#                 hyperpara=Hyperparm.ng,
#                 prior="NG",
#                 thin=2,
#                 eigen=TRUE,
#                 SV=TRUE,
#                 expert=list(OE.weights=list(EB=EA.weights)))

## ----eval=FALSE---------------------------------------------------------------
#   # First example, a US monetary policy shock, quarterly data
#   library(BGVAR)
#   data(eerData)
#   model.eer<-bgvar(Data=eerData,W=W.trade0012,draws=500,burnin=500,plag=1,prior="SSVS",thin=10,eigen=TRUE,trend=TRUE)
# 
#   # generalized impulse responses
#   shockinfo<-get_shockinfo("girf")
#   shockinfo$shock<-"US.stir"; shockinfo$scale<--100
# 
#   irf.girf.us.mp<-irf(model.eer, n.ahead=24, shockinfo=shockinfo)
# 
#   # cholesky identification
#   shockinfo<-get_shockinfo("chol")
#   shockinfo$shock<-"US.stir"; shockinfo$scale<--100
# 
#   irf.chol.us.mp<-irf(model.eer, n.ahead=24, shockinfo=shockinfo)
#   # sign restrictions
#   shockinfo <- get_shockinfo("sign")
#   shockinfo <- add_shockinfo(shockinfo, shock="US.stir", restriction=c("US.y","US.Dp"),
#                              sign=c("<","<"), horizon=c(1,1), scale=1, prob=1)
#   irf.sign.us.mp<-irf(model.eer, n.ahead=24, shockinfo=shockinfo)
# 
#   # sign restrictions with relaxed cross-country restrictions
#   shockinfo <- get_shockinfo("sign")
#   # restriction for other countries holds to 75\%
#   shockinfo <- add_shockinfo(shockinfo, shock="US.stir", restriction=c("US.y","EA.y","UK.y"),
#                              sign=c("<","<","<"), horizon=1, scale=1, prob=c(1,0.75,0.75))
#   shockinfo <- add_shockinfo(shockinfo, shock="US.stir", restriction=c("US.Dp","EA.Dp","UK.Dp"),
#                              sign=c("<","<","<"), horizon=1, scale=1, prob=c(1,0.75,0.75))
#   irf.sign.us.mp<-irf(model.eer, n.ahead=24, shockinfo=shockinfo)
# 
#   # Example with zero restriction (Arias et al., 2018) and
#   # rationality conditions (D'Amico and King, 2017).
#   data("eerDataspf")
#   model.eer<-bgvar(Data=eerDataspf, W=W.trade0012.spf, draws=300, burnin=300,
#                    plag=1, prior="SSVS", eigen=TRUE)
#   shockinfo <- get_shockinfo("sign")
#   shockinfo <- add_shockinfo(shockinfo, shock="US.stir_t+4",
#                              restriction=c("US.Dp_t+4","US.stir","US.y_t+4"),
#                              sign=c("<","0","<"), horizon=1, prob=1, scale=1)
#   # rationality condition: US.stir_t+4 on impact is equal to average of
#   # IRF of US.stir between horizon 1 to 4
#   shockinfo <- add_shockinfo(shockinfo, shock="US.stir_t+4", restriction="US.stir_t+4",
#                              sign="ratio.avg", horizon=5, prob=1, scale=1)
#   # rationality condition: US.Dp_t+4 on impact is equal to IRF of US.Dp at horizon 4
#   shockinfo <- add_shockinfo(shockinfo, shock="US.stir_t+4", restriction="US.Dp_t+4",
#                              sign="ratio.H", horizon=5, prob=1, scale=1)
#   # rationality condition: US.y_t+4 on impact is equal to IRF of US.y at horizon 4
#   shockinfo <- add_shockinfo(shockinfo, shock="US.stir_t+4", restriction="US.y_t+4",
#                              sign="ratio.H", horizon=5, prob=1, scale=1)
#   # regulate maximum number of tries with expert settings
#   irf.ratio <- irf(model.eer, n.ahead=20, shockinfo=shockinfo,
#                    expert=list(MaxTries=10))

## ----hide=TRUE----------------------------------------------------------------
par(oldpar)

