CRIX <- read.csv()
Fed <- read.csv()
USD_EUR <- read.csv()
USD_GBP <- read.csv()
FTSE<- read.csv()
goldfuture <- read.csv()
goldcash<-read.csv()
Sheet<-read.csv()
#価格と日付だけでベクトルに治す

install.packages("tidyr")
install.packages("base R")
install.packages("tidyverse")
install.packages('openxlsx')
install.packages("ggfortify")
library(openxlsx)
library(ggfortify)
library(dplyr, warn.conflicts = FALSE)
#日付をdayオブジェクトにかえる
Fed$Date<-as.Date(Fed$Date, format = "%Y/%m/%d")
FTSE$day<-as.Date(FTSE$day, format = "%Y/%m/%d")
goldcash$X2020.9.21<-as.Date(goldcash$X2020.9.21, format = "%Y/%m/%d")
goldfuture$日付け<-as.Date(goldfuture$日付け, format = "%Y/%m/%d")
Sheet$日付<-as.Date(Sheet$日付, format = "%Y/%m/%d")
USD_EUR$日付け<-as.Date(USD_EUR$日付け, format = "%Y/%m/%d")
USD_GBP$日付け<-as.Date(USD_GBP$日付け, format = "%Y/%m/%d")

#埋めていく
#期間のすべての日付がk
#期間の一部の日付がj
k<-1
j<-1
while(k<=1828){
  if(Sheet$日付[k]==Fed$Date[j]){
    Sheet$fed[k]<-Fed$Price[j]
    j<-j+1
  }
  k<-k+1
}
#FTSEはjを２にする
k<-1
j<-2
while(k<=1828){
  if(Sheet$日付[k]==FTSE$day[j]){
    Sheet$FTSE[k]<-FTSE$終値[j]
    j<-j+1
  }
  k<-k+1
}
k<-1
j<-1
while(k<=1828){
  if(Sheet$日付[k]==goldcash$X2020.9.21[j]){
    Sheet$金現物[k]<-goldcash$X1912.21[j]
    j<-j+1
  }
  k<-k+1
}
#金の先物
k<-1
j<-2
while(k<=1828){
  if(Sheet$日付[k]==goldfuture$日付け[j]){
    Sheet$金先物[k]<-goldfuture$終値[j]
    j<-j+1
  }
  k<-k+1
}
#どるゆーろ
k<-1
j<-2
while(k<=1828){
  if(Sheet$日付[k]==USD_EUR$日付け[j]){
    Sheet$ドルユーロ[k]<-USD_EUR$終値[j]
    j<-j+1
  }
  k<-k+1
}
#どるぽんど
k<-1
j<-2
while(k<=1828){
  if(Sheet$日付[k]==USD_GBP$日付け[j]){
    Sheet$ドルポンド[k]<-USD_GBP$終値[j]
    j<-j+1
  }
  k<-k+1
}
write.csv(Sheet, "/Users/manbubble/Downloads/計量経済レポート/garchSample/data.csv",fileEncoding = "CP932")
sh<-read.csv("/Users/manbubble/Downloads/計量経済レポート/garchSample/data.csv", fileEncoding = "CP932")

condtion<-(sh$FTSE!=0)
dataFTSEzero<-sh[condtion,]
write.csv(dataFTSEzero, "/Users/manbubble/Downloads/計量経済レポート/garchSample/dataFTSEzero.csv",fileEncoding = "CP932")

conditonGoldC<-(dataFTSEzero$金現物!=0)
datazero2<-dataFTSEzero[conditonGoldC,]
write.csv(datazero2, "/Users/manbubble/Downloads/計量経済レポート/garchSample/datazero.csv",fileEncoding = "CP932")

#fedのゼロをさくじょ
conditonLast<-(datazero2$fed!=0)
dataLast<-datazero2[conditonLast,]
write.csv(dataLast, "/Users/manbubble/Downloads/計量経済レポート/garchSample/dataLast.csv",fileEncoding = "CP932")

ExdataCom<-read.csv("/Users/manbubble/Downloads/計量経済レポート/garchSample/Exdata.csv",fileEncoding = "CP932")
ExdataCom<-ExdataCom[,c(2,3,4,5,6,7)]
ExdataMatSec<-as.matrix(ExdataCom)

Second_spec_rugarch <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1),external.regressors=ExdataMatSec),
  mean.model=list(armaOrder=c(2,0), include.mean=TRUE,external.regressors=ExdataMatSec),
  distribution.model = "norm"
)

#外生変数の対数差処理
EXdataLogDiff<-diff(log(ExdataMatSec))
#対数差でegarchやる student-T分布
Egarch_spec_std<- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1, 1),external.regressors=EXdataLogDiff),
  mean.model=list(armaOrder=c(2,0), include.mean=TRUE,external.regressors=EXdataLogDiff),
  distribution.model = "std"
)
Egarch_spec_norm<- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1, 1),external.regressors=EXdataLogDiff),
  mean.model=list(armaOrder=c(2,0), include.mean=TRUE,external.regressors=EXdataLogDiff),
  distribution.model = "norm"
)
#CRIXの対数差をとる
CrixLogDiffVec<-diff(log(CrixVec))
#対数差とったあとegarch
garchE_logdiff_std<-ugarchfit(
  spec = Egarch_spec_std, data = CrixLogDiffVec, solver='hybrid'
)
garchE_logdiff_norm<-ugarchfit(
  spec = Egarch_spec_norm, data = CrixLogDiffVec, solver='hybrid'
)


#対数差でgjrGarch std、normどっちも
Gjrgarch_spec_std<- ugarchspec(
  variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1),external.regressors=EXdataLogDiff),
  mean.model=list(armaOrder=c(2,0), include.mean=TRUE,external.regressors=EXdataLogDiff),
  distribution.model = "std"
)
Gjrgarch_spec_norm<- ugarchspec(
  variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1),external.regressors=EXdataLogDiff),
  mean.model=list(armaOrder=c(2,0), include.mean=TRUE,external.regressors=EXdataLogDiff),
  distribution.model = "norm"
)
Gjrgarch_std<-ugarchfit(
  spec = Gjrgarch_spec_std, data = CrixLogDiffVec, solver='hybrid'
)

Gjrgarch_norm<-ugarchfit(
  spec = Gjrgarch_spec_norm, data = CrixLogDiffVec, solver='hybrid'
)

#標準化した残差
resid_garchSimple_logdiff_simple<-residuals(Simplegarch_norm)/sigma(Simplegarch_norm)
resid_garchSimple_logdiff_std<-residuals(Simplegarch_std)/sigma(Simplegarch_std)
resid_garchE_logdiff_std<-residuals(garchE_logdiff_std)/sigma(garchE_logdiff_std)
resid_garchE_logdiff_norm<-residuals(garchE_logdiff_norm)/sigma(garchE_logdiff_norm)
resid_Gjr_logdiff_std<-residuals(Gjrgarch_std)/sigma(Gjrgarch_std)
resid_Gjr_logdiff_norm<-residuals(Gjrgarch_norm)/sigma(Gjrgarch_norm)
d<-data.frame(
  Egarch_std=resid_garchE_logdiff_std,
  Gjrgarch_std=resid_Gjr_logdiff_std,
  Garch_std=resid_garchSimple_logdiff_std
)
autoplot(ts(d),facets=T,ylab = "",main="標準化偏差")

#AICの比較
infocriteria(garchE_logdiff_std)["Akaike",]
infocriteria(garchE_logdiff_norm)["Akaike",]
infocriteria(Gjrgarch_std)["Akaike",]
infocriteria(Gjrgarch_norm)["Akaike",]
infocriteria(Simplegarch_std)["Akaike",]
infocriteria(Simplegarch_norm)["Akaike",]

#ボラティリティの変動
autoplot(sigma(garchE_logdiff_std),facets=T,ylab="")
autoplot(sigma(Gjrgarch_std),facets=T,ylab="")


#シンプルなガーちを変化量でやる
Simplegarch_spec_std<- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1),external.regressors=EXdataLogDiff),
  mean.model=list(armaOrder=c(2,0), include.mean=TRUE,external.regressors=EXdataLogDiff),
  distribution.model = "std"
)
Simplegarch_spec_norm<- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1),external.regressors=EXdataLogDiff),
  mean.model=list(armaOrder=c(2,0), include.mean=TRUE,external.regressors=EXdataLogDiff),
  distribution.model = "norm"
)
Simplegarch_std<-ugarchfit(
  spec = Simplegarch_spec_std, data = CrixLogDiffVec, solver='hybrid'
)
Simplegarch_norm<-ugarchfit(
  spec = Simplegarch_spec_norm, data = CrixLogDiffVec, solver='hybrid'
)
#単位根検定
install.packages("fUnitRoots")
library(fUnitRoots)
unitrootTest(CrixVec,type = "c",lags = 8)
unitrootTest(ExdataCom$fed,type = "c",lags = 8)
unitrootTest(ExdataCom$FTSE,type = "c",lags = 8)
unitrootTest(ExdataCom$金現物,type = "c",lags = 8)
unitrootTest(ExdataCom$金先物,type = "c",lags = 8)
unitrootTest(ExdataCom$ドルユーロ,type = "c",lags = 8)
unitrootTest(ExdataCom$ドルポンド,type = "c",lags = 8)








garchSec <- ugarchfit(
  spec = Second_spec_rugarch, data = CrixVec, solver='hybrid'
)



#Crixをlogにしたやつでgarch
garchSecCrixLog<-ugarchfit(
  spec = Second_spec_rugarch, data = CrixlogVec, solver='hybrid'
)
coef(garchSecCrixLog)


#金さきものと　FTSEが特殊な文字列なので後で直す
Exdata<-dataLast[,c(4,5,7,8)]
Exdata<-Exdata[2:1227,]
ExdataMat<-as.matrix(Exdata)



CrixLastdata<-dataLast[,c(2,3)]
CrixLastdata<-CrixLastdata[1:1226,]
CrixLastdata<-CrixLastdata[,c(2)]
CrixVec<-as.vector(CrixLastdata,mode = "numeric")

#ログでCrix処理
CrixlogVec<-numeric(1226)
k<-1
while(k<=1226){
  CrixlogVec[k]<-log(CrixLastdata[k])
  k<-k+1
}




#expで処理、inf
ExdataMatSecLog<-matrix(0, nrow=1226, ncol=6) 
x<-1
k<-1
while(x<=6){
  while(k<=1226){
    ExdataMatSecLog[k,x]<-exp(ExdataMatSec[k,x])
    k<-k+1
  }
  k<-1
  x<-x+1
}





#complete(FTSE$日付け<-seq.Date(min(FTSE$日付け), max(FTSE$日付け), by="day"))

#seq.Date(min(FTSE$日付け), max(FTSE$日付け), by="day")

#complete(FTSE, FTSE$日付け <- full_seq(date, 1))

#complete(FTSE$day<-seq.Date(min(FTSE$日付け), max(FTSE$日付け), by="day"))

#typeof(goldfuture$日付け)




mod_fGarch <- garchFit(
  formula = ~  garch(1, 1)+exp(),
  #こんなかにcrix以外のやついれる
  data = sim_garch[,"garch"],
  include.mean = T,
  
  trace = F
)


install.packages("fGarch")
install.packages("rugarch")
library(rugarch)
# ARMA + GARCHモデル
# モデルの構造の設定
spec_rugarch <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1),external.regressors=ExdataMat),
  mean.model=list(armaOrder=c(2,0), include.mean=TRUE,external.regressors=ExdataMat),
  distribution.model = "norm"
)

#やりたいこと:external.regressorsにexp(λ0 +λ1Fedt−1 +λ2USDEURt−1 +λ3USDGBPt−1 +λ4FTSEt−1+λ GoldFuture +λ GoldCash)を入れたい
#できないこと:expをとった式の定義の仕方がわからない、external.regressorsは行列オブジェクトなので前持って定義したモデルは入らないと思われる

#外部のオブジェクトは行列で指定
#CRIXのログを取る
# モデルの推定
garch <- ugarchfit(
  spec = spec_rugarch, data = CrixVec, solver='hybrid'
)



#分散の式はgarch、つまったら聞く
#平均のしきは普通にARでやればいい、価格の階差を前期と前前期のCrixの価格と
