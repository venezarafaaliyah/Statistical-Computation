#loading data from library
library("datasets")
data()
data(pressure)
help(pressure)
tail(pressure)

#data management
objects(package: datasets)
help(Orange)
data("Orange")
head(Orange)

getwd()
setwd("MyDirectory")

#writting data
write.table(Orange, "orange1.txt")
write.table(Orange, "orange2.txt",col.names=F, row.names = F)
write.table(Orange, "orange3.txt", sep="\t")
write.csv(Orange, "orange4.csv")

data=read.table("orange1.txt",header=T)
head(data)

names(data)=c("Pohon", "Usia", "Lingkar Pohon")
head(data)

library(readxl)
data_excel=read_excel("E:/SEM 4/KOMSTAT/praktikum.xlsx")
data_excel

data_csv=read.csv("E:/SEM 4/KOMSTAT/orange4.csv",sep=";",header=TRUE)
data_csv
library(foreign)
data_spss=read.spss("E:/SEM 4/KOMSTAT/supermarket_ok.sav", use.value.label=TRUE, to.data.frame=TRUE)
head(data_spss)
getwd()

data=read.csv("E:/SEM 4/KOMSTAT/praktikum.csv");data
x = data$usia
y = data$tinggi
x
y
kovarians=function(x,y){
  n=length(x)
  m=length(y)
  sx=matrix(nrow=n,ncol=1)
  sy=matrix(nrow=m,ncol=1)
  xbar=sum(x)/n
  ybar=sum(y)/n
  for(i in 1:n){
    sx[i]=(x[i]-xbar)
    sy[i]=(y[i]-ybar)
    
  }
  J=sx*sy
  JK=sum(J)
  kovv=JK/(n-1)
  list(kovarians=kovv)
}
kovarians(x,y)
cov(x,y)

korelasi=function(x,y){
  n=length(x);n
  ssxy=matrix(nrow=n,ncol=1);ssxy
  ssx=matrix(nrow=n,ncol=1);ssx
  ssy=matrix(nrow=n,ncol=1);ssy
  ssy2=matrix(nrow=n,ncol=1);ssy2
  ssx2=matrix(nrow=n,ncol=1);ssx2
  for(i in 1:n){
    ssxy[i]=(x[i]*y[i])
    ssx[i]=(x[i])
    ssy[i]=(y[i])
    ssx2[i]=(x[i])^2
    ssy2[i]=(y[i])^2
  }
  jka=sum(ssxy);jka
  jkb=sum(ssx);jkb
  jkc=sum(ssy);jkc
  jkd=sum(ssx2);jkd
  jke=sum(ssy2);jke
  kor=(n*jka-jkb*jkc)/((sqrt(n*jkd-(jkb)^2))*(sqrt(n*jke-(jkc)^2)))
  list(korelasi=kor)
}
korelasi(x,y)
cor(x,y)
