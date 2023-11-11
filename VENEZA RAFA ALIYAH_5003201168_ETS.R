#VENEZA RAFA ALIYAH - 5003201168 - IUP CLASS Q - COMPUTATIONAL STATISTIC

#1
V=c(rep(c(1:3),times=20),1);V
#2
A=matrix(c(1,1,3,5,2,6,-2,-1,-3), ncol=3,nrow=3,byrow=T);A   
B=matrix(c(1,0,2,0,2,0,1,0,3), ncol=3,nrow=3,byrow=T)
#3
A = function(i){
  A=matrix(ncol=1, nrow=i)
  for(i in 1:i){
  A[i]=(((2^i)/i)+((3^i)/(i^2)))
  }
  sum.A=sum(A)
  list(A=sum.A)
}
A(25)
#4
data=read.csv("C:/Users/ACER/Downloads/dataETS.csv");data
sales=data$Sales
outlet=data$Outlet
promotion=data$Promotion
region=data$Region
#a
Test_1=function(data1,data2){
  n=length(data1)
  region1=matrix(ncol=1,nrow=n)
  region2=matrix(ncol=1,nrow=n)
  region3=matrix(ncol=1,nrow=n)
  region4=matrix(ncol=1,nrow=n)
    if(data2 = 1){
      for(i in 1:n){
        region1[i]=data1[i]
      }
    }
    if else(data2 = 2){
      for(i in 1:n){
        region2[i]=data2[i]
      }
    }
    if else(data2 = 3){
      for(i in 1:n){
        region3[i]=data3[i]
      }
    }
    else{
      for(i in 1:n){
        region4[i]=data4[i]
      }
    }

  mean.region1=mean(region1)
  mean.region2=mean(region2)
  mean.region3=mean(region3)
  mean.region4=mean(region4)
  if(mean1 > mean4){
    cat("mean of sales in region 1 is greater than in region 4","\n")
  }
  else{
    cat("mean of sales in region 1 is lesser than in region 4","\n")
  }
  if(mean2 > mean4){
    cat("mean of sales in region 2 is greater than in region 4","\n")
  }
  else{
    cat("mean of sales in region 2 is lesser than in region 4","\n")
  }
  if(mean3 > mean4){
    cat("mean of sales in region 3 is greater than in region 4","\n")
  }
  else{
    cat("mean of sales in region 3 is lesser than in region 4","\n")
  }
  list(mean1=mean.region1, mean2=mean.region2, mean3=mean.region3, mean4=mean.region4)
}
Test_1(sales,region)

#b
bootstrap_regresi=function(y,x1,x2,n,R){
  N=length(y)
  Reg=matrix(nrow=R, ncol=1)
  B1=matrix(nrow=R,ncol=1)
  B2=matrix(nrow=R,ncol=1)
  B3=matrix(nrow=R,ncol=1)
  for(i in 1:R){
    no=c(1:N)
    P=sample(no,n,replace = F)
    X1=x1[P]
    X2=x2[P]
    Y=y[P]
    Reg=lm(Y~X1+X2)
    B1[i]=Reg$coefficients[1]
    B2[i]=Reg$coefficients[2]
    B3[i]=Reg$coefficients[3]
  }
  a=as.vector(B2)
  b=as.vector(B3)
  rata.bootstrap1=mean(B1)
  rata.bootstrap2=mean(B2)
  rata.bootstrap3=mean(B3)
  sd_boot1=sd(B2)
  sd_boot2=sd(B3)
  N_boot=R
  BB1=rata.bootstrap2-1.92*(sd_boot1/sqrt(N_boot))
  BA1=rata.bootstrap2+1.92*(sd_boot1/sqrt(N_boot))
  BB2=rata.bootstrap2-1.92*(sd_boot2/sqrt(N_boot))
  BA2=rata.bootstrap2+1.92*(sd_boot2/sqrt(N_boot))
  cat("======================================","\n")
  cat("intercept=",rata.bootstrap1,"b1=",rata.bootstrap2,"b2=",rata.bootstrap3,"\n")
  cat("======================================","\n")
  cat("y=",rata.bootstrap1,"+",rata.bootstrap2,"+",rata.bootstrap3,"\n")
  cat("======================================","\n")
  cat("H0: B1 = 0","\n")
  cat("H1: B1 != 0","\n")
  
  if(0>=BB1 & 0<=BA1){
    cat("Decision = failed to Reject H0","\n")
    cat("Conclusion = Outlet is not significantly influence the sales","\n")
  }
  else{
    cat("Decision =  Reject H0","\n")
    cat("Conclusion = Outlet is significantly influence the sales","\n")
  }
  cat("H0: B2 = 0","\n")
  cat("H1: B2 != 0","\n")
  
  if(0>=BB2 & 0<=BA2){
    cat("Decision = failed to Reject H0","\n")
    cat("Conclusion = Promotion is not significantly influence the sales","\n")
  }
  else{
    cat("Decision =  Reject H0","\n")
    cat("Conclusion = Promotion is significantly influence the sales","\n")
  }
  hist(a)
  hist(b)
  par(mfrow=c(1:2))
  list(b0=rata.bootstrap1,b1=rata.bootstrap2,b2=rata.bootstrap3, BB1=BB1,BA1=BA1,BB2=BB2,BA2=BA2)
}
bootstrap_regresi(sales,outlet,promotion,20,1000)


#4A JAWABAN DARREN

testing <- function(data1,data2)
{
  data1_new = as.vector(subset(data1,data2 == 4))
  data2_new = as.vector(subset(data1,data2 < 4))
  
  mean_data1 = mean(data1_new)
  mean_data2 = mean(data2_new)
  
  if(mean_data2 > mean_data1){
    cat("Result = True","\n")
  }
  else{
    cat("Result = False","\n")
  }
  list(mean_region4 = mean_data1,mean_region123 = mean_data2)
}
