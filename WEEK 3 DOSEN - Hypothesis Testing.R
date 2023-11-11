#HYPOTHESIS TESTING

#Miu > 70, Using Upper Tail
data = read.csv("E:/SEM 4/KOMSTAT/nilai - week 3.csv", sep=";");data
kuis = data$Kuis1;kuis
xbar= mean(kuis);xbar
mu0 = 70; mu0
s = sd(kuis);s
n=length(kuis);n
t=(xbar-mu0)/(s/sqrt(n));t

alpha = 0.05
df=n-1;df
t.alpha = qt(1-alpha, df=n-1);t.alpha
pval = pt(t, df=n-1, lower.tail=FALSE) ;pval

#reject H0 if t ??? t?? or p value < alpha -> dont reject H0

#Miu < 65, Using lower tail
ETS = data$ETS;ETS
xbar2= mean(ETS);xbar2
mu02 = 65; mu02
s2 = sd(ETS);s2
n2=length(ETS);n2
t2 = (xbar2-mu02)/(s2/sqrt(n2));t2 

alpha = 0.05
df2=n2-1;df2
t.alpha = qt(1-alpha, df=n2-1)
-t.alpha

pval2 = pt(t, df=n2-1) ; pval2

#Miu is not equal to 85, using two tailed
tugas = data$Tugas;tugas
xbar3= mean(tugas);xbar3
mu03 = 85; mu03
s3 = sd(tugas);s3
n3=length(tugas);n3
t = (xbar3-mu03)/(s3/sqrt(n3));t

alpha = 0.05
t.half.alpha = qt(1-alpha/2, df=n3-1)
c(-t.half.alpha, t.half.alpha)

pval3 = 2*pt(t, df=n3-1);pval3

#LOOPING

library(readxl)
Data = read.csv("E:/SEM 4/KOMSTAT/koding - week 3.csv", sep=";");Data
Data_berat = Data[,1];Data_berat
koding(Data_berat)
Data_usia=Data[,2];Data_usia
Data_JK=Data[,3];Data_JK

koding1(Data_berat,Data_usia)
koding2(Data_berat,Data_JK)
koding2 = function(data1, data2)
{
  
  n = length(data1)
  kategori = matrix(nrow=n, ncol=1)
  
  for (i in 1:n){

    if (data1[i] <= 70 && data2[i] == 1 )
    {
      kategori[i] = "kurus"
    }
    if else(data1[i] > 70 && data2[i] == 1)
    {
      kategori[i] = "normal"
    }
    else (data1[i] != 70 && data2[i] == 1)
    {
      kategori[i] = " "
    }
  }
  kategori = kategori
  data.lengkap = as.data.frame(cbind(data1, data2, kategori))
  list (data.lengkap = data.lengkap)
  
}
koding2(Data_berat,Data_JK)
koding1 = function(data1, data2)
{
  
  n = length(data1)
  kategori = matrix(nrow=n, ncol=1)
  
  for (i in 1:n)
    
  {
    if (data1[i] <= 70 && data2[i] >= 40)
    {
      kategori[i] = "kurus"
    }
    else
    {
      kategori[i] = "normal"
    }
  }
  
  kategori = kategori
  data.lengkap = as.data.frame(cbind(data1, data2, kategori))
  list (data.lengkap = data.lengkap)
  
}
