BTC <- read.csv()
ETH <- read.csv()
XRP <- read.csv()
gold <- read.csv()
corna<- read.csv()



#まず単位根検定して定常かどうか調べた後にVARモデルやる

day <- as.Date(BTC$Date)
day<-day[160:366]
day<-day[1:206]
par(mar=c(5,4,5,4))
plot(day,BTC$X24h.Open..USD.[161:366],
     ylab="USD",ylim=c(0,13000),
     type="l",col="red")
par(new=T)
plot(day,corna$new_cases,yaxt="n",
     ylab="",
     ylim=c(0,90000),
     type="l",col="blue")
axis(side=4)
mtext(side=4,text="new cases")

plot(day,ETH$X24h.Open..USD.,
     ylab="",ylim=c(0,400),
     type="l",col="blue")
axis(side=4)

#corna BTC value esitmated
data <- data.frame(Vec)
result1<- lm(V2~ cornaVec,data=data)
summary(result1)
res1<- zoo(residuals(result1),day)
plot.zoo(res1,xaxt="n",xlab = "")
axis.Date(side=1,
          at = seq(as.Date("2020-01-13"),
                   as.Date("2020-08-05"),"days"),
          format = "%m %d")
mtext(side=1,text="month day")
#曜日ダミーを作る
data$mon<-numeric(206)
data$tue<-numeric(206)
data$wed<-numeric(206)
data$tur<-numeric(206)
data$fri<-numeric(206)
data$sat<-numeric(206)
data$sun<-numeric(206)
data$halving<-numeric(206)
data$halving[120]<-1
k<-0
while(7*k+1<=206){
        data$mon[7*k+1]<-1
        k<-k+1
}
k<-0
while(7*k+2<=206){
        data$tue[7*k+2]<-1
        k<-k+1
}
k<-0
while(7*k+3<=206){
        data$wed[7*k+3]<-1
        k<-k+1
}
k<-0
while(7*k+4<=206){
        data$tur[7*k+4]<-1
        k<-k+1
}
k<-0
while(7*k+5<=206){
        data$fri[7*k+5]<-1
        k<-k+1
}
k<-0
while(7*k+6<=206){
        data$sat[7*k+6]<-1
        k<-k+1
}
k<-0
while(7*k+7<=206){
        data$sun[7*k+7]<-1
        k<-k+1
}
result2<- lm(V2~ cornaVec+halving,data=data)
summary(result2)
res2<- zoo(residuals(result2),day)
plot.zoo(cbind(res1,res2),xaxt="n",xlab = "",
         plot.type = "single",lty=c(3,1),
         col = c("red","blue"))

#１期間前jlkjlk残差の
plot(res2,lag(res2,k=1))

library(lmtest)
dwtest(result2)
bgtest(result2)

#FGLSでの推定
library(nlme)
result3<- gls(V2~ cornaVec+halving+sun,corr=corARMA(p=2),data=data)
summary(result3)

#NEwwey test result2でできない
library(sandwich)
library(lmtest)

coeftest(result1,vcov=NeweyWest)



#定常時系列分析
plot(ETH$X24h.Open..USD.)
acf(ETH$X24h.Open..USD.)
pacf(ETH$X24h.Open..USD.)

plot(BTC$X24h.Open..USD.)
acf(BTC$X24h.Open..USD.)
pacf(BTC$X24h.Open..USD.)


#ARの分析
arx1BTC<- ar(BTC$X24h.Open..USD.,method = "mle")#最尤推定
arx2BTC<- ar(BTC$X24h.Open..USD.,method = "ols")#最小二乗
arx1BTC
arx2BTC
arx1BTC$aic
arx2BTC$aic

arx1Covi<- ar(corna$new_cases,method = "mle")
arx2Covi<- ar(corna$new_cases,method = "ols")#最小二乗
arx1Covi
arx2Covi
arx1Covi$aic
arx2Covi$aic
#係数を調べる
Arima(BTC$X24h.Open..USD.,order = c(1,0,0),method="CSS-ML",include.mean = FALSE)

#Ljung-BOx検定
Box.test(arx1BTC$resid,type="Ljung")

#ARでAICを調べること
arx1$aic
#ラグが1でAICが最小になる
arx2$aic



#ARMA の検定
install.packages("forecast", dependencies = TRUE)




#ARMAでAICの情報量基準で検定
auto.arima(BTC$X24h.Open..USD., ic="aic", stepwise=T, trace=T)
auto.arima(corna$new_cases, ic="aic", stepwise=T, trace=T)

#最適なARIMAモデル で推定
fit = Arima(BTC$X24h.Open..USD.,order = c(2,1,4),include.mean = FALSE)
par(mfrow=c(2,2),cex=0.7)
summary(fit)
#AICを比べる2,1,4など、時間があったら、一番小さいのが最もいいモデル

#Ljung-Box検定
library(forecast)
library(fArma)
#単位根検定のライブラリインストール
library(fUnitRoots)
library(FinTS)

#VARでのコロナとビットコインの価格の分析
cornaVec<-as.vector(corna$new_cases)
BTCVec<-as.vector(BTC$X24h.Open..USD.)
Vec<- cbind(cornaVec,BTCVec[161:366])
unitrootTest(cornaVec,type="c",lags=1)
unitrootTest(BTCVec,type="c",lags=1)
unitrootTest(BTCVec,type="c",lags=4)# t: 0.0752 で１０パーセント以下、非定常かも
unitrootTest(BTCVec,type="c",lags=6)
dayCovid<-as.Date(corna$date)
par(mar=c(5,4,5,4))
plot(day,corna$new_cases,
     ylab="cases",ylim=c(0,90000),
     type="l",col="red")

#VARの次数決定
install.packages("vars", dependencies = TRUE)
library(vars) 
VARselect(Vec,lag.max = 5,type="const")
#式の推定
var1<-VAR(Vec,p=1,type="const")
summary(var1)

CovidBit<- cbind(corna$new_cases,BTC$X24h.Open..USD.[161:366])
var2<-VAR(CovidBit,p=1,type="const")
summary(var2)
#インパルス応答関数をかく

ip<-irf(var2,impulse = c("y1"),
        response = c("y1","y2"),
        n.ahead = 5,boot=TRUE)
plot(ip)

#Granger因果推定
causality(var2,cause = "y1")
causality(var2,cause = "y2")

#ADF検定でタンイコン単位紺
library(fUnitRoots)
unitrootTest(BTC$X24h.Open..USD.)
unitrootTest(BTC$X24h.Open..USD.,type = "c",lags = 8)
unitrootTest(corna$new_cases,type = "c",lags = 3)
#Phillipse Perron検定
library(urca)
pp_btc<-ur.pp(BTC$X24h.Open..USD.,type = "Z-tau",
              model="constant",lags = "long")
summary(pp_btc)#Value of test-statistic, type: Z-tau  is: -2.2517 で棄却出来ず

#KPSS検体で単位根があるかどうか
kpss_btc<-ur.kpss(BTC$X24h.Open..USD.,type="mu",lags = "long")
summary(kpss_btc)#Value of test-statistic is: 0.2875 臨海値0.463なので単位根がないを棄却出来ない

kpss_corna<-ur.kpss(corna$new_cases,type="mu",lags = "long")
summary(kpss_corna)
#Value of test-statistic is: 1.1818  単位根はある
#時間があったら共和分検定でコロナとビットコインの特殊な関係を探る
VecBtcCor<- cbind(BTCVec[161:366],cornaVec)
test_po<- ca.po(VecBtcCor,demean = "const")
summary(test_po)
#5%有意水準の臨海値33.713統計量12.43で棄却出来ず、共和分過程にあるとは言えない

