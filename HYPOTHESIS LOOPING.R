library(readxl)
Data = read.csv("E:/SEM 4/KOMSTAT/koding - week 3.csv", sep=";");Data
Data_berat = Data[,1];Data_berat
zt = function(data, mu0, alpha, test)
{
  
  n = length(data)
  xbar = mean(data)
  sigma=sd(data)
  z=(xbar-mu0)/(sigma/sqrt(n))

  if(test == 1){
    pval = pt(t, df=n-1, lower.tail=FALSE)
    .alpha = qt(1-alpha, df=n-1)
    if(pval<alpha && t >= t.alpha){
      "reject H0"
    }
    else{
      "do not reject H0"
    }
  }
  
  else (test == lowertailed){
    pval = pt(t, df=n-1)
    t.alpha = qt(1-alpha, df=n-1)
    if(pval<alpha && t <= -t.alpha){
      "reject H0"
    }
    else{
      "do not reject H0"
    }
  }
  
  else (test == twotailed){
    t.half.alpha = qt(1-alpha/2, df=n-1)
    pval = 2*pt(t, df=n-1)
    if(pval<alpha && (t <= -t.half.alpha | t >= t.half.alpha)){
      "reject H0"
    }
    else{
      "do not reject H0"
    }
  }
}
zt(Data_berat,70,0.05,uppertailed)

uppertailed = function(data,mu0,alpha){
  n = length(data)
  xbar = mean(data)
  s=sd(data)
  t=(xbar-mu0)/(s/sqrt(n))
  df=n-1
  pval = pt(t, df=n-1, lower.tail=FALSE)
  t.alpha = qt(1-alpha, df=n-1)
  if(pval<alpha && t >= t.alpha){
    "reject H0"
  }
  else{
    "do not reject H0"
  }
}
uppertailed(Data_berat,70,0.05)

lowertailed = function(data,mu0,alpha){
  n = length(data)
  xbar = mean(data)
  s=sd(data)
  t=(xbar-mu0)/(s/sqrt(n))
  df=n-1
  pval = pt(t, df=n-1)
  t.alpha = qt(1-alpha, df=n-1)
  if(pval<alpha && t <= -t.alpha){
    "reject H0"
  }
  else{
    "do not reject H0"
  }
}
lowertailed(Data_berat,70,0.05)

twotailed = function(data,mu0,alpha){
  n = length(data)
  xbar = mean(data)
  s=sd(data)
  t=(xbar-mu0)/(s/sqrt(n))
  df=n-1
  t.half.alpha = qt(1-alpha/2, df=n-1)
  pval = 2*pt(t, df=n-1)
  if(pval<alpha && (t <= -t.half.alpha | t >= t.half.alpha)){
    "reject H0"
  }
  else{
    "do not reject H0"
  }
}
twotailed(Data_berat,70,0.05)
