library(KFAS)
ytot<-TimeSeries_UnemploymentRate <- read.csv("C:/Users/frigg/Downloads/TimeSeries_UnemploymentRate.csv")
time<-ytot[,1]
y<-ytot[,3]
# matplot(ytot[,-1],type="l",lwd=2)
# abline(v=85)
#
#plot tutte-----
# win.graph(width = 25,height = 15)
y<-ytot[85:208,3]
ty<-ts(y,start=c(2011,1),frequency = 12)
plot(ty,lwd=2.5,col=c("orange"), cex.lab=1.3,cex.main=1.5,
     ylab="% Unemployment rate"
     ,xlab="Date",main=paste("US Unemployment time series"),lty=1,ylim=c(2,18),)
y<-ytot[85:208,4]
ty<-ts(y,start=c(2011,1),frequency = 12)
lines(ty,col="royalblue1"q,lwd=2.5)
y<-ytot[85:208,5]
ty<-ts(y,start=c(2011,1),frequency = 12)
lines(ty,col="green3",lwd=2.5)
istr<-gsub("_"," ",names(TimeSeries_UnemploymentRate))
legend("topleft",c(istr[3:5]),cex=1.3,lty=1,lwd=3,col=c("orange","royalblue1","green3"))

#causal con for-----
win.graph(width = 10,height = 15)
par(mfrow=c(3,1))
delta<-numeric()
sdeltaup<-numeric()
sdeltadown<-numeric()
sign<-numeric()
perc<-numeric()
percup<-numeric()
percdown<-numeric()
a1<-matrix(0,ncol=124,nrow=3)
a2<-matrix(0,ncol=124,nrow=3)

for(i in 3:5){
y<-ytot[,i]
yy<-y[85:208]

library(KFAS)
n<-length(yy)
mod <- SSModel(yy ~ SSMtrend(2, Q=list(NA,NA)),H = NA)
fit1<-fitSSM(mod,c(1,1,1))
fit1$optim.out$par
dsmo1 <- KFS(fit1$model, smoothing = c("state", "disturbance"))

auxres_level <- rstandard(dsmo1, type = "state")
# plot(auxres_level)
w<-which.max(auxres_level[,1])
step <- yy
step[] <- 0
step[(w+1):length(step)] <- 1

aZ <- array(0, c(1, 1, length(yy)))
aZ[1, 1, (w+1):length(yy)] <- 1
aT <- array(1, c(1, 1, length(yy)))

marz<-yy*0
marz[w]<-1
mod <- SSModel(yy ~ step+marz+SSMtrend(2, Q=list(NA,NA))+
                 SSMcustom(Z = aZ, T = aT,R = matrix(0, 1, 0), Q = matrix(0, 0, 0),
                           a1 = 0, P = matrix(0), P1inf = matrix(1)),H = NA)

update2 <- function(par, model, ...) {
  model$Q[1, 1, ] <- exp(par[1]) # varianza disturbi di mu_t
  model$Q[2, 2, ] <- exp(par[2]) # varianza disturbi di beta_t
  model$H[1, 1, ] <- exp(par[3]) # varianza errore di osservazione
  model$T[5,5, (w+1):length(yy)] <- 1/(1+exp(-par[4])) # parametro di memoria delta
  model
}
mod$P1inf[5,5]<-0
mod$P1[5,5]<-30

fit2 <- fitSSM(mod, c(fit1$optim.out$par, 0), update2,hessian=T)
cov<-solve(fit2$optim.out$hessian[-3,-3])
del<-1/(1 + exp(-fit2$optim.out$par[4]))
a<-sqrt(cov[3,3])
sdeltaa<-(del^2)*exp(-fit2$optim.out$par[4])*a
fit2$optim.out$par
fit2$model$Q[2,2,]<-fit2$model$Q[1,1,]/1000
ssmo2 <- KFS(fit2$model, smoothing = "state")
livello <- ssmo2$alphahat[, "level"]
cambio_perm <- ssmo2$alphahat[, 1] * mod$Z[1, 1, ]
cambio_trans <- ssmo2$alphahat[, 5]* mod$Z[1, 5, ]
ao_marz <- ssmo2$alphahat[, 2]* mod$Z[1,2, ]
delta[i-1]<-del
sdeltaup[i-1]<-del+1.96*sdeltaa
sdeltadown[i-1]<-del-1.96*sdeltaa
(sign[i-1]<-ssmo2$alphahat[1, 1]/sqrt(ssmo2$V[1,1,1]))
(perc[i-1]<-cambio_perm[w+5]/yy[w-1])
(percup[i-1]<-((cambio_perm[w+5]+sqrt(ssmo2$V[1,1,1])*1.96))/yy[w-1])
(percdown[i-1]<-((cambio_perm[w+5]-sqrt(ssmo2$V[1,1,1])*1.96))/yy[w-1])

istr<-gsub("_"," ",names(TimeSeries_UnemploymentRate))
tot<-yy-cambio_trans-cambio_perm-ao_marz
ttot<-ts(tot,start=c(2011,1),frequency = 12)
totyy<-ts(yy,start=c(2011,1),frequency = 12)
m<-min(ttot)
M<-max(totyy)
a1[i-2,]<-ttot
a2[i-2,]<-totyy  
plot(ttot,type="l",lwd=2,col=c("red"),
     ylab="% Unemployment rate"
     ,xlab="Date",main=paste("Counterfactual:",istr[i]),lty=1,ylim=c(m,M),xlim=c(2016,2021.250))
lines(totyy,col="black",lwd=2)
legend("topleft",c("Real time series","Counterfactual"),lty=1,lwd=2,col=c("black","red"))
}
delta
sdeltaup
sdeltadown
perc
sign
percup
percdown

istr<-names(TimeSeries_UnemploymentRate)

library(plotly)
data<-data.frame(Data=time(ttot),Unemployment_rate=a1[1,],b=a1[2,],c=a1[3,],e=a2[1,],f=a2[2,],g=a2[3,])
fig<-plot_ly(data,x=~Data,mode = 'lines')
fig<-fig%>%add_trace(y=~Unemployment_rate,name="Real_High_school_graduates")
fig<-fig%>%add_trace(y=~b,name="Real_Some_college_or_associate_degree")
fig<-fig%>%add_trace(y=~c,name="Real_Bachelors_degree_and_higher")
fig<-fig%>%add_trace(y=~e,name="Counterfactual_High_school_graduates")
fig<-fig%>%add_trace(y=~f,name="Counterfactual_Some_college_or_associate_degree")
fig<-fig%>%add_trace(y=~g,name="Counterfactual_Bachelors_degree_and_higher")
fig


#causal con for,eff cov-----
# win.graph(width = 25,height = 15)
# par(mfrow=c(3,1))
delta<-numeric()
sign<-numeric()
perc<-numeric()
percup<-numeric()
percdown<-numeric()
for(i in 3:3){
  i=3
  y<-ytot[,i]
  yy<-y[85:208]
  
  library(KFAS)
  n<-length(yy)
  mod <- SSModel(yy ~ SSMtrend(2, Q=list(NA,NA)),H = NA)
  fit1<-fitSSM(mod,c(1,1,1))
  fit1$optim.out$par
  dsmo1 <- KFS(fit1$model, smoothing = c("state", "disturbance"))
  
  auxres_level <- rstandard(dsmo1, type = "state")
  # plot(auxres_level)
  w<-which.max(auxres_level[,1])
  step <- yy
  step[] <- 0
  step[(w+1):length(step)] <- 1
  
  aZ <- array(0, c(1, 1, length(yy)))
  aZ[1, 1, (w+1):length(yy)] <- 1
  aT <- array(1, c(1, 1, length(yy)))
  
  marz<-yy*0
  marz[w]<-1
  mod <- SSModel(yy ~ step+marz+SSMtrend(2, Q=list(NA,NA))+
                   SSMcustom(Z = aZ, T = aT,R = matrix(0, 1, 0), Q = matrix(0, 0, 0),
                             a1 = 0, P = matrix(0), P1inf = matrix(1)),H = NA)
  
  update2 <- function(par, model, ...) {
    model$Q[1, 1, ] <- exp(par[1]) # varianza disturbi di mu_t
    model$Q[2, 2, ] <- exp(par[2]) # varianza disturbi di beta_t
    model$H[1, 1, ] <- exp(par[3]) # varianza errore di osservazione
    model$T[5,5, (w+1):length(yy)] <- 1/(1+exp(-par[4])) # parametro di memoria delta
    model
  }
  mod$P1inf[5,5]<-0
  mod$P1[5,5]<-30
  
  fit2 <- fitSSM(mod, c(fit1$optim.out$par, 0), update2)
  fit2$optim.out$par
  fit2$model$Q[2,2,]<-fit2$model$Q[1,1,]/1000
  ssmo2 <- KFS(fit2$model, smoothing = "state")
  livello <- ssmo2$alphahat[, "level"]
  cambio_perm <- ssmo2$alphahat[, 1] * mod$Z[1, 1, ]
  cambio_trans <- ssmo2$alphahat[, 5]* mod$Z[1, 5, ]
  ao_marz <- ssmo2$alphahat[, 2]* mod$Z[1,2, ]
  dopo<-ssmo2$alphahat[n, 5]*delta^(1:50)
  
  ttot<-ts(c(cambio_perm,rep(cambio_perm[113],50))[1:136],start=c(2011,1),frequency = 12)
  ttotr<-ts(c(cambio_perm+cambio_trans,rep(cambio_perm[113],50)+dopo)[1:136],start=c(2011,1),frequency = 12)
  
  # win.graph(width = 25,height = 15)
  plot(ttot,type="l",lwd=2,col=c("red"), xaxt='n',cex.lab=1.3,cex.main=1.5,
       ylab="% Unemployment rate"
      ,xlab="Date",main=paste("Covid effect:",istr[i]),lty=1,ylim=c(0,15),xlim=c(2019,2022))
  lines(ttotr,lwd=2)
  legend("topleft",c("Permanent effect", "Permanent + Temporary effect"),lty=1,lwd=3,col=c("red","black"),cex=1.3)
  year=2019:2022
  axis(1, at=year)
  
  delta[i-1]<-1/(1 + exp(-fit2$optim.out$par[4]))
  (sign[i-1]<-ssmo2$alphahat[1, 1]/sqrt(ssmo2$V[1,1,1]))
  (perc[i-1]<-cambio_perm[w+5]/yy[w-1])
  (percup[i-1]<-((cambio_perm[w+5]+sqrt(ssmo2$V[1,1,1])*1.96))/yy[w-1])
  (percdown[i-1]<-((cambio_perm[w+5]-sqrt(ssmo2$V[1,1,1])*1.96))/yy[w-1])
  
  istr<-gsub("_"," ",names(TimeSeries_UnemploymentRate))
  tot<-yy-cambio_trans-cambio_perm-ao_marz
  ttot<-ts(tot,start=c(2011,1),frequency = 12)
  totyy<-ts(yy,start=c(2011,1),frequency = 12)
  m<-min(ttot)
  # M<-max(totyy)
  # plot(ttot,type="l",lwd=2,col=c("red"),
  #      ylab="% Unemployment rate"
  #      ,xlab="Date",main=paste("Conterfactual:",istr[i]),lty=1,ylim=c(m,M),xlim=c(2016,2021.250))
  # lines(totyy,col="black",lwd=2)
  # legend("topleft",c("Real time series","Conterfactual"),lty=1,lwd=2,col=c("black","red"))
}
delta
perc
sign
# percup
# percdown

#recessione 2008, maxslope-----
slomax<-numeric()
time[c(85,108)]
for(i in 3:5){
  y<-ytot[1:190,i]
  s<-smooth.spline(y,df=15)
  plot(y)
  lines(s$y)
  abline(v=c(81,105))
  s1<-s$y[1:108]
  s2<-s$y[101:190]
  x<-(1:24)
  l<-lm(s1[85:108]~x)
  slomax[i]<-l$coefficients[2]
  # slomax[i]<-(s1[108]-s1[85])/24
  w<-which(s2<min(s1))[1]
  wm<-which.min(s1)
  print(c(time[wm],time[w+100]))
  print(max(s1)/min(s1))
}


#preivisone lungo periodo, for----
h=208
ww<-numeric()
win.graph(width = 25,height = 15)
par(mfrow=c(3,1))
for(i in 3:5){
y<-ytot[,i]
yy<-y[85:h]
print(time[h])
n<-length(yy)
mod <- SSModel(yy ~ SSMtrend(2, Q=list(NA,NA)),H = NA)
fit1<-fitSSM(mod,c(1,1,1))
fit1$optim.out$par
dsmo1 <- KFS(fit1$model, smoothing = c("state", "disturbance"))

auxres_level <- rstandard(dsmo1, type = "state")
# plot(auxres_level)
w<-which.max(auxres_level[,1])
step <- yy
step[] <- 0
step[(w+1):length(step)] <- 1

aZ <- array(0, c(1, 1, length(yy)))
aZ[1, 1, (w+1):length(yy)] <- 1
aT <- array(1, c(1, 1, length(yy)))

marz<-yy*0
marz[w]<-1
mod <- SSModel(yy ~ step+SSMtrend(2, Q=list(NA,NA))+SSMcustom(Z = aZ, T = aT,
                                                              R = matrix(0, 1, 0), Q = matrix(0, 0, 0),
                                                              a1 = 0, P = matrix(0), P1inf = matrix(1))+
                 marz,H = NA)
update2 <- function(par, model, ...) {
  model$Q[1, 1, ] <- exp(par[1]) # varianza disturbi di mu_t
  model$Q[2, 2, ] <- exp(par[2]) # varianza disturbi di beta_t
  model$H[1, 1, ] <- exp(par[3]) # varianza errore di osservazione
  model$T[5,5, (w+1):length(yy)] <- 1/(1+exp(-par[4])) # parametro di memoria delta
  model
}
mod$P1inf[5,5]<-0
mod$P1[5,5]<-30

fit2 <- fitSSM(mod, c(fit1$optim.out$par, 0), update2)
delta<-1/(1 + exp(-fit2$optim.out$par[4]))
fit2$model$Q
fit2$model$Q[2,2,]<-fit2$model$Q[1,1,]/1000
ssmo2 <- KFS(fit2$model, smoothing = "state")
cambio_perm <- ssmo2$alphahat[, 1] * mod$Z[1, 1, ]
cambio_trans <- ssmo2$alphahat[, 5]* mod$Z[1, 5, ]
ao_marz<-ssmo2$alphahat[, 2]* mod$Z[1, 2, ]
slo<-cumsum(ssmo2$alphahat[, "slope"])

cm<-yy*0
cM<-yy*0
cm[(w+1):length(step)]<-cambio_perm[(w+1):length(step)]-sqrt(ssmo2$V[1,1,])[(w+1):length(step)]*0.67
cM[(w+1):length(step)]<-cambio_perm[(w+1):length(step)]+sqrt(ssmo2$V[1,1,])[(w+1):length(step)]*0.67


yx<-yy-cambio_perm-cambio_trans-ao_marz
mod <- SSModel(yx ~ SSMtrend(2, Q=list(NA,NA)),H = NA)
up <- function(par, model, ...) {
  model$Q[1, 1, ] <- exp(par[1]) # varianza disturbi di mu_t
  model$Q[2, 2, ] <- fit2$model$Q[2,2,] # varianza disturbi di beta_t
  model$H[1, 1, ] <- exp(par[2]) # varianza errore di osservazione
  model
}
fit3<-fitSSM(mod,c(1,1),up)
q1<-fit3$model$Q[1,1,]
fit3$model$Q[2,2,]<-fit2$model$Q[2,2,]
fit3$model$Q[1,1,]<-fit2$model$Q[1,1,]
dsmo1 <- KFS(fit3$model, smoothing = c("state", "disturbance"))
slope<-cumsum(dsmo1$alphahat[,"slope"])
lev<-(dsmo1$alphahat[,"level"])
# plot(dsmo1$alphahat[,"level"])
# plot(cumsum(dsmo1$alphahat[,"slope"]))
new=(n+85):(n+120+84)
pre<-predict(fit3$model,n.ahead = 120,interval = "prediction",level = 0.50)
ef_cov<-cambio_trans[n]*(delta^(2:(length(new)+1)))
tot<-c(slope+lev[1]+cambio_perm+cambio_trans+ao_marz,pre[,1]+cambio_perm[n]+ef_cov)
totm<-c(slope+lev[1]+cambio_perm+cambio_trans+ao_marz,pre[,2]+cambio_perm[n]+ef_cov)
totmax<-c(slomax[i]*(1:120)+lev[n]+cambio_perm[n]+ef_cov)
# totM<-c(slope+lev[1]+cambio_perm+cambio_trans+ao_marz,pre[,3]+cambio_perm[n]+ef_cov)


#plot
istr<-gsub("_"," ",names(TimeSeries_UnemploymentRate))
ttot<-ts(tot,start=c(2011,1),frequency = 12)
ttotmax<-ts(totmax,start=c(2021,5),frequency = 12)
ty<-ts(y[85:208],start=c(2011,1),frequency = 12)
tstima<-ts(slope+lev[1]+cambio_perm+cambio_trans+ao_marz,start=c(2011,1),frequency = 12)



plot(ttot,lwd=2,type="l",col="orange",ylab="% Unemployment rate"
     ,xlab="Date",lty=2,main=paste("Forecasting:",istr[i]),cex.lab=1.3,cex.main=1.5)
# lines(c(85:(n+84),new),totm,lwd=3,col="green",lty=2)
lines(ttotmax,lwd=2,col="coral4",lty=2)
lines(ty,type="l",lwd=1)
lines(tstima,col="cyan2",lwd=2,lty=2)
abline(h=(ty)[w-1],col="darkgrey",lty=3,lwd=2)
legend("topright",c("Time series","Trend estimate with UCM","Forecast with UCM","Optimistic forecast","Feb 2020 Level"),
       col=c("black","cyan2","orange","coral4","darkgrey"),lty=c(1,2,2,2,3),lwd=2,xjust = 1,inset=+0.05)
ww[i]<-which(tot<(ty)[w-1])[2]
print(tot[193])
}
time(ttot)[ww]

# totpa<-c(slope+lev[1]+cambio_perm+cambio_trans+ao_marz,pre[,1]+cambio_perm[n]+ef_cov-pa)
# totpb<-c(slope+lev[1]+cambio_perm+cambio_trans+ao_marz,pre[,1]+cambio_perm[n]+ef_cov+pb)

# 
# #cm
# ym<-yy-cm-ao_marz-slo
# mod <- SSModel(ym ~ SSMtrend(1, Q=NA)+SSMcustom(Z = aZ, T = aT,
#                                                               R = matrix(0, 1, 0), Q = matrix(0, 0, 0),
#                                                               a1 = 0, P = matrix(0), P1inf = matrix(1))
#                 ,H = NA)
# update2 <- function(par, model, ...) {
#   model$Q[1, 1, ] <- exp(par[1]) # varianza disturbi di mu_t
#   model$H[1, 1, ] <- exp(par[2]) # varianza errore di osservazione
#   model$T[2,2, (w+1):length(yy)] <- 1/(1+exp(-par[3])) # parametro di memoria delta
#   model
# }
# mod$P1inf[2,2]<-0
# mod$P1[2,2]<-30
# 
# fit2 <- fitSSM(mod, c(1,1,0.5), update2)
# delta<-1/(1 + exp(-fit2$optim.out$par[3]))
# ssmo2 <- KFS(fit2$model, smoothing = "state")
# cambio_transm <- ssmo2$alphahat[, 2]* mod$Z[1, 2, ]
# 
# 
# yxm<-ym-cambio_transm
# mod <- SSModel(yxm ~ SSMtrend(1, Q=NA),H = NA)
# up <- function(par, model, ...) {
#   model$Q[1, 1, ] <- exp(par[1]) # varianza disturbi di mu_t
#   model$H[1, 1, ] <- exp(par[2]) # varianza errore di osservazione
#   model
# }
# fit3<-fitSSM(mod,c(1,1),up)
# dsmo1 <- KFS(fit3$model, smoothing = c("state", "disturbance"))
# lev<-(dsmo1$alphahat[,"level"])
# new=(n+85):(n+120+84)
# pre<-predict(fit3$model,n.ahead = 120,interval = "prediction",level = 0.50)
# ef_cov<-cambio_transm[n]*(delta^(2:(length(new)+1)))
# totm<-c(slo+lev[1]+cm+cambio_transm+ao_marz,pre[,1]+cm[n]+ef_cov)
# 
# 
# #cM
# yM<-yy-cM-ao_marz
# mod <- SSModel(yM ~ SSMtrend(2, Q=list(NA,NA))+SSMcustom(Z = aZ, T = aT,
#                                                          R = matrix(0, 1, 0), Q = matrix(0, 0, 0),
#                                                          a1 = 0, P = matrix(0), P1inf = matrix(1))
#                ,H = NA)
# update2 <- function(par, model, ...) {
#   model$Q[1, 1, ] <- exp(par[1]) # varianza disturbi di mu_t
#   model$Q[2, 2, ] <- exp(par[2]) # varianza disturbi di beta_t
#   model$H[1, 1, ] <- exp(par[3]) # varianza errore di osservazione
#   model$T[3,3, (w+1):length(yy)] <- 1/(1+exp(-par[4])) # parametro di memoria delta
#   model
# }
# mod$P1inf[3,3]<-0
# mod$P1[3,3]<-30
# 
# fit2 <- fitSSM(mod, c(fit1$optim.out$par, 0), update2)
# delta<-1/(1 + exp(-fit2$optim.out$par[4]))
# fit2$model$Q
# # fit2$model$Q[2,2,]<-fit2$model$Q[1,1,]/1000
# ssmo2 <- KFS(fit2$model, smoothing = "state")
# cambio_transM <- ssmo2$alphahat[, 3]* mod$Z[1, 3, ]
# plot(cambio_transM)
# plot(yxM)
# yxM<-yM-cambio_transM
# mod <- SSModel(yxM ~ SSMtrend(2, Q=list(NA,NA)),H = NA)
# up <- function(par, model, ...) {
#   model$Q[1, 1, ] <- exp(par[1]) # varianza disturbi di mu_t
#   model$Q[2, 2, ] <- fit2$model$Q[2,2,] # varianza disturbi di beta_t
#   model$H[1, 1, ] <- exp(par[2]) # varianza errore di osservazione
#   model
# }
# fit3<-fitSSM(mod,c(1,1),up)
# q1<-fit3$model$Q[1,1,]
# # fit3$model$Q[2,2,]<-fit2$model$Q[2,2,]
# # fit3$model$Q[1,1,]<-fit2$model$Q[1,1,]
# dsmo1 <- KFS(fit3$model, smoothing = c("state", "disturbance"))
# slope<-cumsum(dsmo1$alphahat[,"slope"])
# lev<-(dsmo1$alphahat[,"level"])
# # plot(dsmo1$alphahat[,"level"])
# # plot(cumsum(dsmo1$alphahat[,"slope"]))
# new=(n+85):(n+120+84)
# pre<-predict(fit3$model,n.ahead = 120,interval = "prediction",level = 0.50)
# ef_cov<-cambio_transM[n]*(delta^(2:(length(new)+1)))
# totM<-c(slope+lev[1]+cM+cambio_transM+ao_marz,pre[,1]+cM[n]+ef_cov)

#
# plot(tot,type="l")
# lines(totm)
# lines(totM)
# abline(v=n)



#spline
# ef_cov<-cambio_trans[n]*(delta^(2:(length(new)+1)))
# tot<-c(pre[,1]+cambio_perm[n]+ef_cov)
# totM<-c(pre[,2]+cambio_perm[n]+ef_cov)
# totm<-c(pre[,3]+cambio_perm[n]+ef_cov)
# # plot(tot)
# # lines(totm)
# # lines(totM)
# # matplot(cbind(tot,totm,totM),type="l")
# 
# library(mgcv)
# a<-1:109
# r<-data.frame(yr=yy,a=a)
# s<-gam(yr~s(a),data=r)
# # s<-smooth.spline(yy,df=5)
# new=110:190
# a<-new
# pr<-predict(s,aa,se.fit=T)
# dev<-pr$se.fit
# ef_cov<-cambio_trans[n]*(delta^(2:(length(new)+1)))
# tot<-c(s$fitted.values+cambio_perm+cambio_trans+ao_marz,pr$fit+cambio_perm[n]+ef_cov)
# 
# tot_m<-c(s$fitted.values+cambio_perm+cambio_trans+ao_marz,pr$fit-dev+cambio_perm[n]+ef_cov)
# tot_M<-c(s$fitted.values+cambio_perm+cambio_trans+ao_marz,pr$fit+dev+cambio_perm[n]+ef_cov)
# 
# plot(c(100:208,209:289),tot,lwd=2,type="l",col="green4",lty=2,main=paste("serie",i))
# lines(c(100:208,209:289),tot_m,lwd=3,col="green",lty=2)
# lines(c(100:208,209:289),tot_M,lwd=3,col="green",lty=2)
# lines(y,type="l",lwd=1)
# lines(100:208,s$fitted.values+cambio_perm+cambio_trans+ao_marz,col="orange",lwd=2,lty=2)
# abline(h=s$y[95],col="gray",lty=2)





######--------------
#causl impact----
library(CausalImpact)
data<-y[85:208]
pre.period<-c(1,111)
post.period<-c(112,124)
impact <- CausalImpact(data, pre.period, post.period)
plot(impact)

#causal------
plot(y)
yy<-y[1:208]

library(KFAS)
n<-length(yy)
mod <- SSModel(yy ~ SSMtrend(2, Q=list(NA,NA)),H = NA)
fit1<-fitSSM(mod,c(1,1,1))
fit1$optim.out$par
dsmo1 <- KFS(fit1$model, smoothing = c("state", "disturbance"))
plot(yy,type="l")
lines(dsmo1$alphahat[,1],col="red")
lines(cumsum(dsmo1$alphahat[,2])+dsmo1$alphahat[1,1])


auxres_level <- rstandard(dsmo1, type = "state")
plot(auxres_level)
w<-which.max(auxres_level[,1])
step <- yy
step[] <- 0
step[(w+1):length(step)] <- 1

aZ <- array(0, c(1, 1, length(yy)))
aZ[1, 1, (w+1):length(yy)] <- 1
aT <- array(1, c(1, 1, length(yy)))

mod <- SSModel(yy ~ step+SSMtrend(2, Q=list(NA,NA))+
                 SSMcustom(Z = aZ, T = aT,R = matrix(0, 1, 0), Q = matrix(0, 0, 0),
                           a1 = 0, P = matrix(0), P1inf = matrix(1)),H = NA)
update2 <- function(par, model, ...) {
  model$Q[1, 1, ] <- exp(par[1]) # varianza disturbi di mu_t
  model$Q[2, 2, ] <- exp(par[2]) # varianza disturbi di beta_t
  model$H[1, 1, ] <- exp(par[3]) # varianza errore di osservazione
  model$T[4,4, (w+1):length(yy)] <- 1/(1+exp(-par[4])) # parametro di memoria delta
  model
}
mod$P1inf[4,4]<-0
mod$P1[4,4]<-30

fit2 <- fitSSM(mod, c(fit1$optim.out$par, 0), update2)
fit2$optim.out$par
# fit2$model$Q
# fit2$model$Q[,,]<-fit2$model$Q[1,1,]
ssmo2 <- KFS(fit2$model, smoothing = "state")
livello <- ssmo2$alphahat[, 2]
cambio_perm <- ssmo2$alphahat[, 1] * mod$Z[1, 1, ]
cambio_trans <- ssmo2$alphahat[, 4]* mod$Z[1, 4, ]
delta<-1/(1 + exp(-fit2$optim.out$par[4]))
cat("delta =", delta, "\n")
(sign<-ssmo2$alphahat[1, 1]/sqrt(ssmo2$V[1,1,1]))

plot(cambio_perm, main = "Cambiamento di livello permanente nel numero feriti (%)")
plot(cambio_trans, main = "Cambiamento di livello transitorio nel numero feriti (%)")
(perc<-cambio_perm[w+5]/yy[w-1])
plot(cambio_trans+cambio_perm)

plot(y[1:208],type="l")
lines(livello+cambio_trans+cambio_perm,col="red")
lines(cumsum(ssmo2$alphahat[, 3])+livello[1])
lines(y-cambio_trans-cambio_perm)

#lungo periodo----
h=208
yy<-y[1:h]
print(time[h])
n<-length(yy)
mod <- SSModel(yy ~ SSMtrend(2, Q=list(NA,NA)),H = NA)
fit1<-fitSSM(mod,c(1,1,1))
fit1$optim.out$par
dsmo1 <- KFS(fit1$model, smoothing = c("state", "disturbance"))

auxres_level <- rstandard(dsmo1, type = "state")
# plot(auxres_level)
w<-which.max(auxres_level[,1])
step <- yy
step[] <- 0
step[(w+1):length(step)] <- 1

aZ <- array(0, c(1, 1, length(yy)))
aZ[1, 1, (w+1):length(yy)] <- 1
aT <- array(1, c(1, 1, length(yy)))

marz<-yy*0
marz[195]<-1
mod <- SSModel(yy ~ step+SSMtrend(2, Q=list(NA,NA))+SSMcustom(Z = aZ, T = aT,
                                                              R = matrix(0, 1, 0), Q = matrix(0, 0, 0),
                                                              a1 = 0, P = matrix(0), P1inf = matrix(1))+
                 marz,H = NA)
update2 <- function(par, model, ...) {
  model$Q[1, 1, ] <- exp(par[1]) # varianza disturbi di mu_t
  model$Q[2, 2, ] <- exp(par[2]) # varianza disturbi di beta_t
  model$H[1, 1, ] <- exp(par[3]) # varianza errore di osservazione
  model$T[5,5, (w+1):length(yy)] <- 1/(1+exp(-par[4])) # parametro di memoria delta
  model
}
mod$P1inf[5,5]<-0
mod$P1[5,5]<-30

fit2 <- fitSSM(mod, c(fit1$optim.out$par, 0), update2)
delta<-1/(1 + exp(-fit2$optim.out$par[4]))
# fit2$optim.out$par
# cat("delta =", 1/(1 + exp(-fit2$optim.out$par[4])), "\n")
# fit2$model$Q
# fit2$model$Q[,,]<-fit2$model$Q[1,1,]
ssmo2 <- KFS(fit2$model, smoothing = "state")
cambio_perm <- ssmo2$alphahat[, 1] * mod$Z[1, 1, ]
cambio_trans <- ssmo2$alphahat[, 5]* mod$Z[1, 5, ]
ao_marz<-ssmo2$alphahat[, 2]* mod$Z[1, 2, ]
yy<-yy-cambio_perm-cambio_trans-ao_marz
# plot(yy)
mod <- SSModel(yy ~ SSMtrend(2, Q=list(0,NA)),H = NA)

update2 <- function(par, model, ...) {
  model$Q[1, 1, ] <- 0 # varianza disturbi di mu_t
  model$Q[2, 2, ] <- exp(par[1]) # varianza disturbi di beta_t
  model$H[1, 1, ] <- exp(par[2]) # varianza errore di osservazione
  model
}

fit2 <- fitSSM(mod, c(0.2,0.2, 0), update2)
fit2$model$Q[2,2,]<-fit2$model$Q[2,2,]/100
ssmo2 <- KFS(fit2$model, smoothing = "state")
lev<-ssmo2$alphahat[,1]
sl<-ssmo2$alphahat[,2]
# plot(sl)
# plot(lev)
yyy<-cbind(y[1:208],lev+cambio_perm+cambio_trans+ao_marz,lev)
matplot(yyy,type="l",lwd=2)
plot(y[1:208],type="l")
lines(lev+cambio_perm+cambio_trans+ao_marz,col="red")
lines(lev+ao_marz,col="orange")

#oppure con spline
library(splines)
s<-smooth.spline(yy,df=10)
new=209:250
pr<-predict(s,new)
ef_cov<-cambio_trans[208]^(2:(length(new)+1))
tot<-c(s$y+cambio_perm+cambio_trans+ao_marz,pr$y+cambio_perm[208]+ef_cov)
plot(c(1:208,new),tot,lwd=2,type="l",col="green4")
lines(y,type="l")
lines(s$y+cambio_perm+cambio_trans+ao_marz,col="orange",lwd=2)


#lungo periodo,no perm for----
h=208
ww<-numeric()
win.graph()
par(mfrow=c(2,2))
for(i in 3:5){
  y<-ytot[,i]
  yy<-y[85:h]
  print(time[h])
  n<-length(yy)
  mod <- SSModel(yy ~ SSMtrend(2, Q=list(NA,NA)),H = NA)
  fit1<-fitSSM(mod,c(1,1,1))
  fit1$optim.out$par
  dsmo1 <- KFS(fit1$model, smoothing = c("state", "disturbance"))
  
  auxres_level <- rstandard(dsmo1, type = "state")
  # plot(auxres_level)
  w<-which.max(auxres_level[,1])
  step <- yy
  step[] <- 0
  step[(w+1):length(step)] <- 1
  
  aZ <- array(0, c(1, 1, length(yy)))
  aZ[1, 1, (w+1):length(yy)] <- 1
  aT <- array(1, c(1, 1, length(yy)))
  
  marz<-yy*0
  marz[w]<-1
  ss<-marz
  mod <- SSModel(yy ~ ss+SSMtrend(2, Q=list(NA,NA))+SSMcustom(Z = aZ, T = aT,
                                                                R = matrix(0, 1, 0), Q = matrix(0, 0, 0),
                                                                a1 = 0, P = matrix(0), P1inf = matrix(1))+
                   marz,H = NA)
  update2 <- function(par, model, ...) {
    model$Q[1, 1, ] <- exp(par[1]) # varianza disturbi di mu_t
    model$Q[2, 2, ] <- exp(par[2]) # varianza disturbi di beta_t
    model$H[1, 1, ] <- exp(par[3]) # varianza errore di osservazione
    model$T[5,5, (w+1):length(yy)] <- 1/(1+exp(-par[4])) # parametro di memoria delta
    model
  }
  mod$P1inf[5,5]<-0
  mod$P1[5,5]<-30
  
  fit2 <- fitSSM(mod, c(fit1$optim.out$par, 0), update2)
  delta<-1/(1 + exp(-fit2$optim.out$par[4]))
  fit2$model$Q
  fit2$model$Q[2,2,]<-fit2$model$Q[1,1,]/100
  ssmo2 <- KFS(fit2$model, smoothing = "state")
  cambio_perm <- ssmo2$alphahat[, 1] * mod$Z[1, 1, ]
  cambio_trans <- ssmo2$alphahat[, 5]* mod$Z[1, 5, ]
  ao_marz<-ssmo2$alphahat[, 2]* mod$Z[1, 2, ]
  slo<-cumsum(ssmo2$alphahat[, "slope"])
  
  cm<-yy*0
  cM<-yy*0
  cm[(w+1):length(step)]<-cambio_perm[(w+1):length(step)]-sqrt(ssmo2$V[1,1,])[(w+1):length(step)]*0.67
  cM[(w+1):length(step)]<-cambio_perm[(w+1):length(step)]+sqrt(ssmo2$V[1,1,])[(w+1):length(step)]*0.67
  
  
  yx<-yy-cambio_perm-cambio_trans-ao_marz
  mod <- SSModel(yx ~ SSMtrend(2, Q=list(NA,NA)),H = NA)
  up <- function(par, model, ...) {
    model$Q[1, 1, ] <- exp(par[1]) # varianza disturbi di mu_t
    model$Q[2, 2, ] <- fit2$model$Q[2,2,] # varianza disturbi di beta_t
    model$H[1, 1, ] <- exp(par[2]) # varianza errore di osservazione
    model
  }
  fit3<-fitSSM(mod,c(1,1),up)
  q1<-fit3$model$Q[1,1,]
  # fit3$model$Q[2,2,]<-fit2$model$Q[2,2,]
  fit3$model$Q[1,1,]<-fit2$model$Q[1,1,]
  dsmo1 <- KFS(fit3$model, smoothing = c("state", "disturbance"))
  slope<-cumsum(dsmo1$alphahat[,"slope"])
  lev<-(dsmo1$alphahat[,"level"])
  # plot(dsmo1$alphahat[,"level"])
  # plot(cumsum(dsmo1$alphahat[,"slope"]))
  new=(n+85):(n+120+84)
  pre<-predict(fit3$model,n.ahead = 120,interval = "prediction",level = 0.90)
  ef_cov<-cambio_trans[n]*(delta^(2:(length(new)+1)))
  tot<-c(slope+lev[1]+cambio_perm+cambio_trans+ao_marz,pre[,1]+cambio_perm[n]+ef_cov)
  totm<-c(slope+lev[1]+cambio_perm+cambio_trans+ao_marz,pre[,2]+cambio_perm[n]+ef_cov)
  totM<-c(slope+lev[1]+cambio_perm+cambio_trans+ao_marz,pre[,3]+cambio_perm[n]+ef_cov)
  
  #plot
  plot(c(85:(n+84),new),tot,lwd=2,type="l",col="green4",lty=2,main=paste("serie",i))
  # lines(c(85:(n+84),new),totm,lwd=3,col="green",lty=2)
  # lines(c(85:(n+84),new),totM,lwd=3,col="green",lty=2)
  lines(y,type="l",lwd=1)
  lines(85:(n+84),slope+lev[1]+cambio_perm+cambio_trans+ao_marz,col="orange",lwd=2,lty=2)
  abline(h=(lev)[w],col="gray",lty=2)
  ww[i]<-which(tot<(lev)[w])[1]+85
}
ww
#spline
# ef_cov<-cambio_trans[n]*(delta^(2:(length(new)+1)))
# tot<-c(pre[,1]+cambio_perm[n]+ef_cov)
# totM<-c(pre[,2]+cambio_perm[n]+ef_cov)
# totm<-c(pre[,3]+cambio_perm[n]+ef_cov)
# # plot(tot)
# # lines(totm)
# # lines(totM)
# # matplot(cbind(tot,totm,totM),type="l")
# 
# library(mgcv)
# a<-1:109
# r<-data.frame(yr=yy,a=a)
# s<-gam(yr~s(a),data=r)
# # s<-smooth.spline(yy,df=5)
# new=110:190
# a<-new
# pr<-predict(s,aa,se.fit=T)
# dev<-pr$se.fit
# ef_cov<-cambio_trans[n]*(delta^(2:(length(new)+1)))
# tot<-c(s$fitted.values+cambio_perm+cambio_trans+ao_marz,pr$fit+cambio_perm[n]+ef_cov)
# 
# tot_m<-c(s$fitted.values+cambio_perm+cambio_trans+ao_marz,pr$fit-dev+cambio_perm[n]+ef_cov)
# tot_M<-c(s$fitted.values+cambio_perm+cambio_trans+ao_marz,pr$fit+dev+cambio_perm[n]+ef_cov)
# 
# plot(c(100:208,209:289),tot,lwd=2,type="l",col="green4",lty=2,main=paste("serie",i))
# lines(c(100:208,209:289),tot_m,lwd=3,col="green",lty=2)
# lines(c(100:208,209:289),tot_M,lwd=3,col="green",lty=2)
# lines(y,type="l",lwd=1)
# lines(100:208,s$fitted.values+cambio_perm+cambio_trans+ao_marz,col="orange",lwd=2,lty=2)
# abline(h=s$y[95],col="gray",lty=2)





#forecasting------
er<-numeric()
plot(200:208,y[200:208],type="l")
for(h in 203:207){
yy<-y[1:h]
print(time[h])
library(KFAS)
n<-length(yy)
mod <- SSModel(yy ~ SSMtrend(2, Q=list(NA,NA)),H = NA)
fit1<-fitSSM(mod,c(1,1,1))
fit1$optim.out$par
dsmo1 <- KFS(fit1$model, smoothing = c("state", "disturbance"))
# plot(yy,type="l")
# lines(dsmo1$alphahat[,1],col="red")
# lines(cumsum(dsmo1$alphahat[,2])+dsmo1$alphahat[1,1])
lev<-dsmo1$alphahat[,1]
sl<-dsmo1$alphahat[,2]
(pre<-lev[n]+sl[n])
y[n+1]
er[h-202]<-abs(pre-y[n+1])
points(h+1,pre,col="red",pch=20)
}
mean(er)#5:0.0596 2:1.34 3:0.163 4:0.023


er2<-numeric()
plot(200:208,y[200:208],type="l")
#MODELLIZZARE ANCHE PRIMA ONDATA
for(h in 203:207){
  yy<-y[1:h]
  print(time[h])
  n<-length(yy)
  mod <- SSModel(yy ~ SSMtrend(2, Q=list(NA,NA)),H = NA)
  fit1<-fitSSM(mod,c(1,1,1))
  fit1$optim.out$par
  dsmo1 <- KFS(fit1$model, smoothing = c("state", "disturbance"))
  
  auxres_level <- rstandard(dsmo1, type = "state")
  # plot(auxres_level)
  w<-which.max(auxres_level[,1])
  step <- yy
  step[] <- 0
  step[(w+1):length(step)] <- 1
  
  aZ <- array(0, c(1, 1, length(yy)))
  aZ[1, 1, (w+1):length(yy)] <- 1
  aT <- array(1, c(1, 1, length(yy)))
  
  mod <- SSModel(yy ~ step+SSMtrend(2, Q=list(NA,NA))+SSMcustom(Z = aZ, T = aT,
                                                                R = matrix(0, 1, 0), Q = matrix(0, 0, 0),
                                                                a1 = 0, P = matrix(0), P1inf = matrix(1)),H = NA)
  update2 <- function(par, model, ...) {
    model$Q[1, 1, ] <- exp(par[1]) # varianza disturbi di mu_t
    model$Q[2, 2, ] <- exp(par[2]) # varianza disturbi di beta_t
    model$H[1, 1, ] <- exp(par[3]) # varianza errore di osservazione
    model$T[4,4, (w+1):length(yy)] <- 1/(1+exp(-par[4])) # parametro di memoria delta
    model
  }
  mod$P1inf[4,4]<-0
  mod$P1[4,4]<-30
  
  fit2 <- fitSSM(mod, c(fit1$optim.out$par, 0), update2)
  delta<-1/(1 + exp(-fit2$optim.out$par[4]))
  # fit2$optim.out$par
  # cat("delta =", 1/(1 + exp(-fit2$optim.out$par[4])), "\n")
  # fit2$model$Q
  # fit2$model$Q[,,]<-fit2$model$Q[1,1,]
  ssmo2 <- KFS(fit2$model, smoothing = "state")
  cambio_perm <- ssmo2$alphahat[, 1] * mod$Z[1, 1, ]
  cambio_trans <- ssmo2$alphahat[, 4]* mod$Z[1, 4, ]
  lev<-ssmo2$alphahat[,2]
  sl<-ssmo2$alphahat[,3]
  (pre<-lev[n]+sl[n]+ssmo2$alphahat[n, 1]+delta*ssmo2$alphahat[n, 4])
  y[n+1]
  er2[h-202]<-abs(pre-y[n+1])
  points(h+1,pre,col="red",pch=20)
}
mean(er2)#5:0.0524 2:1.62 3:0.136 4:0.061
mean(er-er2)/(sd(er-er2)/sqrt(length(er)))


er3<-numeric()
plot(200:208,y[200:208],type="l")
for(h in 203:207){
  yy<-y[1:h]
  print(time[h])
  n<-length(yy)
  mod <- SSModel(yy ~ SSMtrend(1, Q=list(NA)),H = NA)
  fit1<-fitSSM(mod,c(1,1,1))
  fit1$optim.out$par
  dsmo1 <- KFS(fit1$model, smoothing = c("state", "disturbance"))
  
  auxres_level <- rstandard(dsmo1, type = "state")
  # plot(auxres_level)
  w<-which.max(auxres_level)
  step <- yy
  step[] <- 0
  step[(w+1):length(step)] <- 1
  
  aZ <- array(0, c(1, 1, length(yy)))
  aZ[1, 1, (w+1):length(yy)] <- 1
  aT <- array(1, c(1, 1, length(yy)))
  
  mod <- SSModel(yy ~ step+SSMtrend(1, Q=list(NA))+SSMcustom(Z = aZ, T = aT,
                                                                R = matrix(0, 1, 0), Q = matrix(0, 0, 0),
                                                                a1 = 0, P = matrix(0), P1inf = matrix(1)),H = NA)
  update2 <- function(par, model, ...) {
    model$Q[1, 1, ] <- exp(par[1]) # varianza disturbi di mu_t
    model$H[1, 1, ] <- exp(par[2]) # varianza errore di osservazione
    model$T[3,3, (w+1):length(yy)] <- 1/(1+exp(-par[3])) # parametro di memoria delta
    model
  }
  mod$P1inf[3,3]<-0
  mod$P1[3,3]<-30
  
  fit2 <- fitSSM(mod, c(fit1$optim.out$par, 0), update2)
  delta<-1/(1 + exp(-fit2$optim.out$par[3]))

  ssmo2 <- KFS(fit2$model, smoothing = "state")
  cambio_perm <- ssmo2$alphahat[, 1] * mod$Z[1, 1, ]
  cambio_trans <- ssmo2$alphahat[, 3]* mod$Z[1, 3, ]
  lev<-ssmo2$alphahat[,2]
  (pre<-lev[n]+ssmo2$alphahat[n, 1]+delta*ssmo2$alphahat[n, 3])
  y[n+1]
  er3[h-202]<-(pre-y[n+1])^2
  points(h+1,pre,col="red",pch=20)
}
mean(er3)#5:0.0700 2:1.48
mean(er-er3)/(sd(er-er3)/sqrt(length(er3)))
              

er4<-numeric()
plot(200:208,y[200:208],type="l")
for(h in 203:207){

  (pre<-lev[n]+sl[n]+delta*cambio_trans[n]+cambio_perm[n])
  y[n+1]
  er4[h-202]<-(pre-y[n+1])^2
  points(h+1,pre,col="red",pch=20)
}
mean(er4)#2:1.56
mean(er-er4)/(sd(er-er4)/sqrt(length(er)))

#forecasting,arima------
er<-numeric()
plot(200:208,y[200:208],type="l")
for(h in 203:207){
  yy<-y[1:h]
  print(time[h])
  library(KFAS)
  n<-length(yy)
  mod <- SSModel(yy ~ SSMarima(ar=1,ma=0,d=1,stationary = F,Q=NA),H = NA)
  diag(mod$P1)<-30
  diag(mod$P1inf)<-0
  fit1<-fitSSM(mod,c(1,1,1))
  fit1$optim.out$par
  dsmo1 <- KFS(fit1$model, smoothing = c("state", "disturbance"))
  # plot(yy,type="l")
  # lines(dsmo1$alphahat[,1],col="red")
  # lines(cumsum(dsmo1$alphahat[,2])+dsmo1$alphahat[1,1])
  lev<-dsmo1$alphahat[,1]
  sl<-dsmo1$alphahat[,2]
  (pre<-lev[n]+sl[n])
  y[n+1]
  er[h-202]<-(pre-y[n+1])^2
  points(h+1,pre,col="red",pch=20)
}
mean(er)#5:0.0596 2:1.34 3:0.163 4:0.023


er2<-numeric()
plot(200:208,y[200:208],type="l")
for(h in 203:207){
  yy<-y[1:h]
  print(time[h])
  n<-length(yy)
  mod <- SSModel(yy ~ SSMarima(ar=1,ma=0,d=1,stationary = F,Q=NA),H = NA)
  diag(mod$P1)<-30
  diag(mod$P1inf)<-0  
  fit1<-fitSSM(mod,c(1,1,1))
  fit1$optim.out$par
  dsmo1 <- KFS(fit1$model, smoothing = c("state", "disturbance"))
  
  auxres_level <- rstandard(dsmo1, type = "state")
  # plot(auxres_level)
  w<-which.max(auxres_level[])
  step <- yy
  step[] <- 0
  step[(w+1):length(step)] <- 1
  
  aZ <- array(0, c(1, 1, length(yy)))
  aZ[1, 1, (w+1):length(yy)] <- 1
  aT <- array(1, c(1, 1, length(yy)))
  
  mod <- SSModel(yy ~ step+SSMarima(ar=1,ma=0,d=1,stationary = F,Q=NA)+SSMcustom(Z = aZ, T = aT,
                                                                R = matrix(0, 1, 0), Q = matrix(0, 0, 0),
                                                                a1 = 0, P = matrix(0), P1inf = matrix(1)),H = NA)
  update2 <- function(par, model, ...) {
    model$Q[1, 1, ] <- exp(par[1]) # varianza disturbi di mu_t
    model$H[1, 1, ] <- exp(par[2]) # varianza errore di osservazione
    model$T[5,5 ,(w+1):length(yy)] <- 1/(1+exp(-par[3])) # parametro di memoria delta
    model
  }
  diag(mod$P1inf)<-0
  diag(mod$P1)<-30
  
  fit2 <- fitSSM(mod, c(fit1$optim.out$par), update2)
  delta<-1/(1 + exp(-fit2$optim.out$par[3]))
  # fit2$optim.out$par
  # cat("delta =", 1/(1 + exp(-fit2$optim.out$par[4])), "\n")
  # fit2$model$Q
  # fit2$model$Q[,,]<-fit2$model$Q[1,1,]
  ssmo2 <- KFS(fit2$model, smoothing = "state")
  cambio_perm <- ssmo2$alphahat[, 1] * mod$Z[1, 1, ]
  cambio_trans <- ssmo2$alphahat[, 5]* mod$Z[1, 5, ]
  lev<-ssmo2$alphahat[,2]
  sl<-ssmo2$alphahat[,3]
  (pre<-lev[n]+sl[n]+ssmo2$alphahat[n, 1]+delta*ssmo2$alphahat[n, 4])
  y[n+1]
  er2[h-202]<-(pre-y[n+1])^2
  points(h+1,pre,col="red",pch=20)
}
mean(er2)#5:0.0524 2:1.62 3:0.136 4:0.061
mean(er-er2)/(sd(er-er2)/sqrt(length(er)))


#google trend----
US_Jobs_GT <- read.csv("C:/Users/frigg/Downloads/US_Jobs_GT.csv")[1:208,2]
job_offers_GT <- read.csv("C:/Users/frigg/Downloads/job_offers_GT.csv")[1:208,2]
indeed_GT <- as.numeric(read.csv("C:/Users/frigg/Downloads/indeed_GT.csv")[1:208,2])
indeed_GT[is.na(indeed_GT)]=0
plot(indeed_GT,type="l")
plot(US_Jobs_GT,type="l")
plot(job_offers_GT,type="l")
Covid_USA

#recessione 2008,prev----
y<-ytot[,3]
p=1
plot(y,type="l")
er1<-numeric()
er5<-numeric()
er10<-numeric()

#da 90 la previsione a 100
for(h in 90:183){
yy<-y[1:h]
library(KFAS)
n<-length(yy)
mod <- SSModel(yy ~ SSMtrend(2, Q=list(NA,NA)),H = NA)
fit1<-fitSSM(mod,c(1,1,1))
fit1$optim.out$par
dsmo1 <- KFS(fit1$model, smoothing = c("state", "disturbance"))
# plot(yy,type="l")
# lines(dsmo1$alphahat[,1],col="red")
# lines(cumsum(dsmo1$alphahat[,2])+dsmo1$alphahat[1,1])

auxres_level <- rstandard(dsmo1, type = "state")
# plot(auxres_level)
mi<-which.max(auxres_level[,2])
ma<-which.min(auxres_level[,2])
cp1<-yy*0
cp2<-yy*0
cp1[(mi+2):n]<-1:(n-mi-1)
cp2[(ma+2):n]<-1:(n-ma-1)

mod <- SSModel(yy ~ cp1+cp2+SSMtrend(2, Q=list(NA,NA)),H = NA)
diag(mod$P1[1:2,1:2])<-30
diag(mod$P1inf[1:2,1:2])<-0

fit1<-fitSSM(mod,c(1,1,1))
fit1$optim.out$par
fit1$model$Q[2,2,]<-fit1$model$Q[1,1,]/100#????
dsmo1 <- KFS(fit1$model, smoothing = c("state", "disturbance"))
CP1<-dsmo1$alphahat[,1]*mod$Z[1,1,]
CP2<-dsmo1$alphahat[,2]*mod$Z[1,2,]
# plot(yy,type="l")
# lines(dsmo1$alphahat[,"level"]+CP1+CP2,col="red")
# lines(cumsum(dsmo1$alphahat[,"slope"])+dsmo1$alphahat[1,"level"]+CP1+CP2)
lev<-dsmo1$alphahat[,"level"]
sl<-dsmo1$alphahat[,"slope"]
cp<-dsmo1$alphahat[,1]+dsmo1$alphahat[,2]
CP<-(CP1+CP2)[h]
p=1
pr1<-lev[h]+sl[h]*p+cp[h]*p+CP
er1[h-89]<-(y[h+p]-pr1)
p=5
pr5<-lev[h]+sl[h]*p+cp[h]*p+CP
er5[h-89]<-(y[h+p]-pr5)
p=10
pr10<-lev[h]+sl[h]*p+cp[h]*p+CP
er10[h-89]<-(y[h+p]-pr10)
points(h+10,pr10,col="red",pch=20)
}
hist(er1)
hist(er15)
hist(er10)
mean(abs(er10))
mean(abs(er5))
mean(abs(er1))


#recessione 2008,prev lunghe----
y<-ytot[,3]
p=1
plot(y,type="l")
er20<-numeric()
er30<-numeric()
er50<-numeric()

#da 90 la previsione a 100
for(h in 90:143){
  yy<-y[1:h]
  library(KFAS)
  n<-length(yy)
  mod <- SSModel(yy ~ SSMtrend(2, Q=list(NA,NA)),H = NA)
  fit1<-fitSSM(mod,c(1,1,1))
  fit1$optim.out$par
  dsmo1 <- KFS(fit1$model, smoothing = c("state", "disturbance"))
  # plot(yy,type="l")
  # lines(dsmo1$alphahat[,1],col="red")
  # lines(cumsum(dsmo1$alphahat[,2])+dsmo1$alphahat[1,1])
  
  auxres_level <- rstandard(dsmo1, type = "state")
  # plot(auxres_level)
  mi<-which.max(auxres_level[,2])
  ma<-which.min(auxres_level[,2])
  cp1<-yy*0
  cp2<-yy*0
  cp1[(mi+2):n]<-1:(n-mi-1)
  cp2[(ma+2):n]<-1:(n-ma-1)
  
  mod <- SSModel(yy ~ cp1+cp2+SSMtrend(2, Q=list(NA,NA)),H = NA)
  diag(mod$P1[1:2,1:2])<-30
  diag(mod$P1inf[1:2,1:2])<-0
  
  fit1<-fitSSM(mod,c(1,1,1))
  fit1$optim.out$par
  fit1$model$Q[2,2,]<-fit1$model$Q[1,1,]/100#????
  dsmo1 <- KFS(fit1$model, smoothing = c("state", "disturbance"))
  CP1<-dsmo1$alphahat[,1]*mod$Z[1,1,]
  CP2<-dsmo1$alphahat[,2]*mod$Z[1,2,]
  # plot(yy,type="l")
  # lines(dsmo1$alphahat[,"level"]+CP1+CP2,col="red")
  # lines(cumsum(dsmo1$alphahat[,"slope"])+dsmo1$alphahat[1,"level"]+CP1+CP2)
  lev<-dsmo1$alphahat[,"level"]
  sl<-dsmo1$alphahat[,"slope"]
  cp<-dsmo1$alphahat[,1]+dsmo1$alphahat[,2]
  CP<-(CP1+CP2)[h]
  p=20
  pr20<-lev[h]+sl[h]*p+cp[h]*p+CP
  er20[h-89]<-(y[h+p]-pr20)
  p=30
  pr30<-lev[h]+sl[h]*p+cp[h]*p+CP
  er30[h-89]<-(y[h+p]-pr30)
  p=50
  pr50<-lev[h]+sl[h]*p+cp[h]*p+CP
  er50[h-89]<-(y[h+p]-pr5)
  points(h+50,pr50,col="red",pch=20)
}
hist(er20)
hist(er50)
hist(er30)
median(abs(er50))
median(abs(er20))
median(abs(er30))

q1a<-quantile(er1,0.25)
q1b<-quantile(er1,0.75)
q2a<-quantile(er5,0.25)
q2b<-quantile(er5,0.75)
q3a<-quantile(er10,0.25)
q3b<-quantile(er10,0.75)
q4a<-quantile(er20,0.25)
q4b<-quantile(er20,0.75)


q1a<-quantile(abs(er1),0.5)
q1b<-quantile(er1,0.75)
q2a<-quantile(abs(er5),0.5)
q2b<-quantile(er5,0.75)
q3a<-quantile(abs(er10),0.5)
q3b<-quantile(er10,0.75)
q4a<-quantile(abs(er20),0.5)
q4b<-quantile(er20,0.75)

plot(c(1,5,10,20),c(q1a,q2a,q3a,q4a))
plot(c(1,5,10,20),c(q1b,q2b,q3b,q4b))
la<-lm(c(q1a,q2a,q3a,q4a)~c(1,5,10,20))
lac<-la$coefficients
pa<-lac[1]+lac[2]*(1:120)

lb<-lm(c(q1b,q2b,q3b,q4b)~c(1,5,10,20))
lbc<-lb$coefficients
pb<-lbc[1]+lbc[2]*(1:120)
plot(pa)
