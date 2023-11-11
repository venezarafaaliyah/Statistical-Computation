Data = read.csv("E:/SEM 4/KOMSTAT/koding - week 3.csv", sep=";");Data
berat=Data$berat
usia=Data$usia
JK=Data$JK

#Bootstrap Regression
regresi=lm(berat~usia);regresi
bootreg = function(x,y,n,R)
{
  N=length(x)
  Reg=matrix(nrow=R, ncol=1)
  B1=matrix(nrow=R, ncol=1)
  B2=matrix(nrow=R, ncol=1)
  for(i in 1:R){
    no=c(1:N)
    P=sample(no,n,replace=TRUE)
    X=x[P]
    Y=y[P]
    Reg=lm(Y~X)
    B1[i]=Reg$coefficients[1]
    B2[i]=Reg$coefficients[2]
  }
  a=as.vector(B1)
  b=as.vector(B2)
  rata.bootstrap1=mean(a)
  rata.bootstrap2=mean(b)
  sd_boot1 = sd(a)
  sd_boot2 = sd(b)
  N_boot = R
  
  BB1 = rata.bootstrap1 - 1.96*(sd_boot1/sqrt(R))
  BA1 = rata.bootstrap1 + 1.96*(sd_boot1/sqrt(R))
  BB2 = rata.bootstrap2 - 1.96*(sd_boot2/sqrt(R))
  BA2 = rata.bootstrap2 + 1.96*(sd_boot2/sqrt(R))
  if(0 >= BB1 & 0 <= BA1)
  {
    cat("Conclusion = Failed to Reject H0", "\n")
  }
  else
  {
    cat("Conclusion = Reject H0", "\n")
  }
  if(0 >= BB2 & 0 <= BA2)
  {
    cat("Conclusion = Failed to Reject H0", "\n")
  }
  else
  {
    cat("Conclusion = Reject H0", "\n")
  }
  cat("==============================","\n")
  cat("Intercept =", rata.bootstrap1, "\n")
  cat("==============================","\n")
  cat("Slope =", rata.bootstrap2, "\n")
  cat("==============================","\n")
  hist(a)
  hist(b)
  par(mfrow=c(1:2))
  list(intercept=rata.bootstrap1,Slope = rata.bootstrap2, Upper.Bound1=BA1,Lower.Bound1=BB1,Upper.Bound2=BA2,Lower.Bound2=BB2)
}
bootreg(sales,promotion,20,10000)
bootreg(usia,berat,10,10000)

#Normality Test
residual=regresi$residuals;residual
hist(residual)
library(nortest)
normal.test_ad=ad.test(residual)
normal.test_sf=sf.test(residual)
normal.test_ad$p.value
normal.test_sf$p.value

jacknife = function(data){
  N=length(data)
  rata=matrix(nrow=N, ncol=1)
  for(i in 1:N)
  {
    data_baru=data[-i]
    rata[i]=mean(data_baru)
  }
  rata.jack=mean(rata)
  list(rata.jacknife=rata.jack)
}
jacknife(berat)

#Bootstrap Regression Ganda
bootreg_ganda = function(x1,x2,y,n,R)
{
  N=length(x1)
  Reg=matrix(nrow=R, ncol=1)
  B1=matrix(nrow=R, ncol=1)
  B2=matrix(nrow=R, ncol=1)
  B3=matrix(nrow=R, ncol=1)
  for(i in 1:R){
    no=c(1:N)
    P=sample(no,n,replace=TRUE)
    X1=x1[P]
    X2=x2[P]
    Y=y[P]
    Reg=lm(Y~X1+X2-1)
    B1[i]=Reg$coefficients[1]
    B2[i]=Reg$coefficients[2]
    B3[i]=Reg$coefficients[3]
  }
  a=as.vector(B1)
  b=as.vector(B2)
  c=as.vector(B3)
  rata.bootstrap1=mean(a)
  rata.bootstrap2=mean(b)
  rata.bootstrap3=mean(c)
  sd_boot1 = sd(a)
  sd_boot2 = sd(b)
  sd_boot3 = sd(c)
  N_boot = R
  
  BB1 = rata.bootstrap1 - 1.96*(sd_boot1/sqrt(R))
  BA1 = rata.bootstrap1 + 1.96*(sd_boot1/sqrt(R))
  BB2 = rata.bootstrap2 - 1.96*(sd_boot2/sqrt(R))
  BA2 = rata.bootstrap2 + 1.96*(sd_boot2/sqrt(R))
  BB3 = rata.bootstrap3 - 1.96*(sd_boot3/sqrt(R))
  BA3 = rata.bootstrap3 + 1.96*(sd_boot3/sqrt(R))
  if(0 >= BB1 & 0 <= BA1)
  {
    cat("Conclusion = Failed to Reject H0", "\n")
  }
  else
  {
    cat("Conclusion = Reject H0", "\n")
  }
  if(0 >= BB2 & 0 <= BA2)
  {
    cat("Conclusion = Failed to Reject H0", "\n")
  }
  else
  {
    cat("Conclusion = Reject H0", "\n")
  }
  if(0 >= BB3 & 0 <= BA3)
  {
    cat("Conclusion = Failed to Reject H0", "\n")
  }
  else
  {
    cat("Conclusion = Reject H0", "\n")
  }
  cat("==============================","\n")
  cat("Intercept =", rata.bootstrap1, "\n")
  cat("==============================","\n")
  cat("Slope1 =", rata.bootstrap2, "\n")
  cat("==============================","\n")
  cat("Slope2 =", rata.bootstrap3, "\n")
  cat("==============================","\n")
  cat("y=",rata.bootstrap1,"+",rata.bootstrap2,"x1","+",rata.bootstrap3,"x2","\n")
  hist(a)
  hist(b)
  hist(c)
  par(mfrow=c(1:3))
  list(intercept=rata.bootstrap1,Slope1 = rata.bootstrap2,Slope2 = rata.bootstrap3, Upper.Bound1=BA1,Lower.Bound1=BB1,Upper.Bound2=BA2,Lower.Bound2=BB2, Upper.Bound3=BA3,Lower.Bound3=BB3)
}
bootreg_ganda(berat,usia,JK,10,100)
