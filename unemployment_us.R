library(KFAS)
ytot<-TimeSeries_UnemploymentRate <- read.csv("C:/Users/frigg/Downloads/TimeSeries_UnemploymentRate.csv")
time<-ytot[,1]
#plot tutte serie-----
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
}


#previsone lungo periodo----
h=208
ww<-numeric()
win.graph(width = 25,height = 15)
par(mfrow=c(3,1))
for(i in 3:5){
  y<-ytot[,i]
  yy<-y[85:h]
  n<-length(yy)
  mod <- SSModel(yy ~ SSMtrend(2, Q=list(NA,NA)),H = NA)
  fit1<-fitSSM(mod,c(1,1,1))
  dsmo1 <- KFS(fit1$model, smoothing = c("state", "disturbance"))
  auxres_level <- rstandard(dsmo1, type = "state")
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
  
  #previsioni
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
  new=(n+85):(n+120+84)
  pre<-predict(fit3$model,n.ahead = 120,interval = "prediction",level = 0.50)
  ef_cov<-cambio_trans[n]*(delta^(2:(length(new)+1)))
  tot<-c(slope+lev[1]+cambio_perm+cambio_trans+ao_marz,pre[,1]+cambio_perm[n]+ef_cov)
  totmax<-c(slomax[i]*(1:120)+lev[n]+cambio_perm[n]+ef_cov)
  
  #plot:
  istr<-gsub("_"," ",names(TimeSeries_UnemploymentRate))
  #aggiungo le date ai vettori
  ttot<-ts(tot,start=c(2011,1),frequency = 12)
  ttotmax<-ts(totmax,start=c(2021,5),frequency = 12)
  ty<-ts(y[85:208],start=c(2011,1),frequency = 12)
  tstima<-ts(slope+lev[1]+cambio_perm+cambio_trans+ao_marz,start=c(2011,1),frequency = 12)
  
  plot(ttot,lwd=2,type="l",col="orange",ylab="% Unemployment rate"
       ,xlab="Date",lty=2,main=paste("Forecasting:",istr[i]),cex.lab=1.3,cex.main=1.5)
  lines(ttotmax,lwd=2,col="coral4",lty=2)
  lines(ty,type="l",lwd=1)
  lines(tstima,col="cyan2",lwd=2,lty=2)
  abline(h=(ty)[w-1],col="darkgrey",lty=3,lwd=2)
  legend("topright",c("Time series","Trend estimate with UCM","Forecast with UCM","Optimistic forecast","Feb 2020 Level"),
         col=c("black","cyan2","orange","coral4","darkgrey"),lty=c(1,2,2,2,3),lwd=2,xjust = 1,inset=+0.05)
}
#causal impact----
library(readxl)
library(CausalImpact)
#CPI
cpi<- read_excel("C:/Users/frigg/Desktop/cpi.xlsx", 
                col_names = FALSE)
cpi<-as.numeric(cpi)
#Costo degli impiegati
wage <- read_excel("C:/Users/frigg/Desktop/wage.xlsx", 
                   col_names = FALSE)

wage<-wage$...1
wage<-rep(wage,each=3)
wage[124]<-wage[123]
#Guadagno degli impiegati
ind1 <- read.csv("C:/Users/frigg/Desktop/ind_high.csv", header=FALSE, sep=";")
ind1<-ind1[1,]
ind1[1]<-633
ind1<-as.numeric(ind1)
ind1<-rep(ind1,each=3)
ind1[124]<-ind1[123]
ind2 <- read.csv("C:/Users/frigg/Desktop/ind_some.csv", header=FALSE, sep=";")
ind2<-ind2[1,]
ind2[1]<-736
ind2<-as.numeric(ind2)
ind2<-rep(ind2,each=3)
ind2[124]<-ind2[123]
ind3 <- read.csv("C:/Users/frigg/Desktop/ind_bach.csv", header=FALSE, sep=";")
ind3<-ind3[1,]
ind3[1]<-1150
ind3<-as.numeric(ind3)
ind3<-rep(ind3,each=3)
ind3[124]<-ind3[123]
ind<-cbind(ind1,ind2,ind3)
matplot(ind,type="l")
plot(wage)
plot(cpi)
library(ggplot2)
for(i in 3:5){
  y<-ytot[,i]
  time.points <- seq.Date(as.Date("2011-01-01"), to=as.Date("2021-04-01"), length.out = 124)
  yy<-y[85:208]
  ii<-ind[,i-2]
  data <- zoo(cbind(yy,cpi,wage, ii), time.points)
  pre.period<-as.Date(c("2011-01-01", "2020-02-1"))
  post.period<-as.Date(c("2020-03-01", "2021-04-1"))
  impact <- CausalImpact(data, pre.period, post.period)
  istr<-gsub("_"," ",names(TimeSeries_UnemploymentRate))
  istr[5]<-"Bachelor's degree and higher"
  imp<-plot(impact,"original")+ggtitle(paste("Real vs Counterfactual:" ,istr[i]))+xlab("Date") + ylab("% Unemployment rate")
  plot(imp)
}








