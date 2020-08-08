cvdataApp <- read.csv()
cvdataSuvey <- read.csv()
install.packages('openxlsx')
library(openxlsx)
effectData<-read.xlsx()

dataE <- data.frame(effectData)

cvdataSuvey<-cbind(cvdataSuvey[])
cvdataSuvey<-na.omit(cvdataSuvey)
dataOmitNa<-na.omit(dataE)
dataProfit<-cbind(dataE[4],dataE[5],dataE[7])
dataProfit<-na.omit(dataProfit)


k<-1
while(k<=127){
  if(dataProfit$A4.公費かそうでないか.[k]==1){
    dataProfit$public[k]<-1
  }else{
    dataProfit$public[k]<-0
  }
  k<-k+1
}
while(k<=127){
  if(dataProfit$A4.公費かそうでないか.[k]==1){
    dataProfit$other[k]<-0
  }else{
    dataProfit$other[k]<-1
  }
  k<-k+1
}

#単純な重回帰
colnames(dataProfit)<-c("pr2010", "pr2015","publicOrOthe","public","other")
OLS<-lm(pr2015~pr2010+public+(1-public),data=dataProfit)
summary(OLS)

dataPatent<-cbind(cvdataSuvey[12],cvdataSuvey[13],cvdataSuvey[15],cvdataSuvey[16],cvdataSuvey[17],cvdataSuvey[18],cvdataSuvey[140],cvdataSuvey[158]
                  ,cvdataSuvey[236],cvdataSuvey[237],cvdataSuvey[238],cvdataSuvey[241],cvdataSuvey$C11)
colnames(dataPatent)<-c("リーダーかパートナーか", "企業か研究所か","現在の稼働状況","publicOrOther","助成金をどこから","助成金がなかった場合のステータス"
                      ,"特許証人か","５年間の平均従業員数","2010年の従業員数","2015年の従業員数","2010年の研究開発費","2015年の研究開発費","助成金の合計")


dataPatent<-na.omit(dataPatent[,c(1,4,7,13)])#176変数
dataPatent<-na.omit(dataPatent)
dataPatent2<-na.omit(dataPatent[,c(1,4,7,13)])
k<-1
while(k<=176){
  if(dataPatent$publicOrOther[k]==1){
    dataPatent$public[k]<-1
  }else{
    dataPatent$public[k]<-0
  }
  k<-k+1
}


colnames(dataPatent)<-c("readerOrPartner","publicOrOther","patentTrue","Subsidy")

#ロジットで、公的資金を受けられるかどうかGLMでいったん
#logit <- glm(public ~  other+ pr2010, data = dataProfit, family = binomial(link = "logit"))
#summary(logit)



logit2 <- lrm.fit(public ~  other+ pr2010,data = dataProfit)
summary(logit2)

#ロジットで特許説明
k<-1
while(k<=176){
  if(dataPatent$patentTrue[k]==1||dataPatent$patentTrue[k]==3){
    dataPatent$patent[k]<-1
  }else{
    dataPatent$patent[k]<-0
  }
  k<-k+1
}

k<-1
while(k<=176){
  if(dataPatent$readerOrPartner[k]==1){
    dataPatent$reader[k]<-1
  }else{
    dataPatent$reader[k]<-0
  }
  k<-k+1
}

#順序プロビット
k<-1
while(k<=176){
  if(dataPatent$patentTrue[k]==2||dataPatent$patentTrue[k]==4){
    dataPatent$patentTrue2[k]<-3
  }else if(dataPatent$patentTrue[k]==3){
    dataPatent$patentTrue2[k]<-2
  }else{
    dataPatent$patentTrue2[k]<-1
  }
  k<-k+1
}
dataPatent$patentTrue2 <- ordered(dataPatent$patentTrue2,c(1,2,3))

h <- polr( as.ordered(patentTrue2) ~  factor(public) + factor(reader) , data=dataPatent, method = "probit")
summary(h)

#順序ロジット
#共同研究の目的
dataPatent2<-cbind(cvdataSuvey[16],cvdataSuvey[140],cvdataSuvey$B11,cvdataSuvey$B8_k1_Q,cvdataSuvey$B8_k2_Q,cvdataSuvey$B8_k5_Q,cvdataSuvey$B8_k6_Q,cvdataSuvey$B8_k8_Q,
                   cvdataSuvey$B8_k9_Q)
dataPatent2<-na.omit(dataPatent2)
k<-1
while(k<=261){
  if(dataPatent2$C4_k2_Q[k]==2||dataPatent2$C4_k2_Q[k]==4){
    dataPatent2$patent[k]<-3
  }else if(dataPatent2$C4_k2_Q[k]==3){
    dataPatent2$patent[k]<-2
  }else{
    dataPatent2$patent[k]<-1
  }
  k<-k+1
}
dataPatent2$public<-ifelse(
  dataPatent2$A4==1,
  1,0
)

dataPatent2$communiEvery<-ifelse(
  dataPatent2$`cvdataSuvey$B11`==1,
  1,0
)
dataPatent2$communiWeek<-ifelse(
  dataPatent2$`cvdataSuvey$B11`==2,
  1,0
)
dataPatent2$communiMonth<-ifelse(
  dataPatent2$`cvdataSuvey$B11`==3,
  1,0
)
dataPatent2$communiLess<-ifelse(
  dataPatent2$`cvdataSuvey$B11`==4,
  1,0
)
#共同研究の目的

dataPatent2$CoReserach<-ifelse(
  dataPatent2$`cvdataSuvey$B8_k1_Q`==1,
  1,0
)
dataPatent2$PurposPatent<-ifelse(
  dataPatent2$`cvdataSuvey$B8_k5_Q`==2,
  1,0
)
dataPatent2$ResearchBook<-ifelse(
  dataPatent2$`cvdataSuvey$B8_k8_Q`==3,
  1,0
)
dataPatent2$Purchase<-ifelse(
  dataPatent2$`cvdataSuvey$B8_k9_Q`==4,
  1,0
)


logPatentGet <- polr( as.ordered(patent)~factor(public) + factor(communiEvery)+factor(CoReserach),data=dataPatent2, method = "logistic")
summary(logPatentGet)






h_log2 <- polr( as.ordered(patentTrue2)~factor(publicOrOther) + factor(readerOrPartner) , data=dataPatent, method = "logistic")
summary(h_log2)


library(mlogit)
library(AER)
h_log <- polr( as.ordered(patentTrue2)~factor(publicOrOther) + factor(readerOrPartner) , data=dataPatent, method = "logistic")
summary(h_log)

#条件つきロジット

#均一分散かどうかを検定