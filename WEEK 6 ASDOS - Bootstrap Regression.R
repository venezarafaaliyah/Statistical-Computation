#H0 : mu0 = 270
#H1 : mu0 != 270
data=read.csv("E:/SEM 4/KOMSTAT/data_sales.csv");data
sales=data$Sales;sales
boot(sales,10000,6,270)
mean(sales)
attach(data) #agar variable gausah di define dulu
data_miskin=read.csv("E:/SEM 4/KOMSTAT/datamiskin.csv",sep=";");data_miskin
attach(data_miskin)
Y1=data_miskin$kemiskinan_Y1
X1=data_miskin$rata.rata.lama.sekolah_X1
X2=data_miskin$kepadatan.penduduk_X2
bootstrap(Y1,X1,10,10000)
bootstrap(Y1,X2,10,10000)
bootstrap(X1,X2,10,10000)

regresi =lm(Y1~X1-1)
regresi
summary(regresi)


bootreg = function(x,y,n,R)
{
  N=length(x)
  Reg=matrix(nrow=R, ncol=1)
  B=matrix(nrow=R, ncol=1)
  for(i in 1:R){
    no=c(1:N)
    P=sample(no,n,replace=TRUE)
    X=x[P]
    Y=y[P]
    Reg=lm(Y~X-1)
    B[i]=Reg$coefficients[1]
    
  }
  
  rata.bootstrap=mean(B)
  
  sd_boot = sd(B)
  N_boot = R
  
  BB = rata.bootstrap - 1.96*(sd_boot/sqrt(R))
  BA = rata.bootstrap + 1.96*(sd_boot/sqrt(R))
  
  list(rata.bootstrap=rata.bootstrap, BA=BA,BB=BB)
}
bootreg(X1,Y1,10,10000)
bootreg(X2,Y1,10,10000)
bootreg(X2,X1,10,10000)

bootreg_ganda = function(x1,x2,y,n,R)
{
  N=length(x1)
  Reg=matrix(nrow=R, ncol=1)
  B1=matrix(nrow=R, ncol=1)
  B2=matrix(nrow=R, ncol=1)
  for(i in 1:R)
  {
    no=c(1:N)
    P=sample(no,n,replace=TRUE)
    X1=x1[P]
    X2=x2[P]
    Y=y[P]
    Reg=lm(Y~X1+X2-1)
    B1[i]=Reg$coefficients[1]
    B2[i]=Reg$coefficients[2] 
    }
  
  rata.bootstrap1=mean(as.vector(B1))
  rata.bootstrap2=mean(as.vector(B2))
  sd_boot_B1 = sd(as.vector(B1))
  sd_boot_B2 = sd(as.vector(B2))
  N_boot = R
  
  BB1 = rata.bootstrap1 - 1.96*(sd_boot_B1/sqrt(R))
  BA1 = rata.bootstrap1 + 1.96*(sd_boot_B1/sqrt(R))
  
  BB2 = rata.bootstrap2 - 1.96*(sd_boot_B2/sqrt(R))
  BA2 = rata.bootstrap2 + 1.96*(sd_boot_B2/sqrt(R))
  list(b1=rata.bootstrap1, b2=rata.bootstrap2, BA1=BA1,BB1=BB1,BA2=BA2,BB2=BB2)
}
bootreg_ganda(X1,X2,Y1,10,100)
regresi=lm(Y1~X1+X2-1);regresi
