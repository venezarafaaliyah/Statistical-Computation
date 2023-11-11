library(nortest)
data=data.normal(100,0,1)
normality = function (data,n,R,alpha){
  sd_data=sd(data)
  normal=matrix(nrow=R, ncol=1)
  for( i in 1:length(data)){
    no=c(1:length(data))
    P=sample(no,n,replace=TRUE)
    X=data[P]
    pval=ad.test(X)$p.value
    normal[i] = (1/sqrt(2*pi))*exp(((X/sd_data)^2)/-2)
  }
  rata_normal=mean(normal)
  test=matrix(nrow=R,ncol=1)
  for(i in 1:length(data)){
  if (pval <= alpha){
    test[i]=0
  }
  else {
    test[i]=1
  }
  }
  jumlah=sum(test)
  prop=jumlah/R
  list(proportion=prop,pvalue=pval,normal=rata_normal)
}
normality(data,20,100,0.05)
ad.test(sales)

monte_carlo=function(n,alpha,R){
  pval=matrix(nrow=R,ncol=1)
  code_ad=matrix(nrow=R, ncol=1)
  code_ks=matrix(nrow=R, ncol=1)
  code_sw=matrix(nrow=R, ncol=1)
  for(i in 1:R){
    data=rnorm(n,0,1)
    library(nortest)
    ad_test=ad.test(data)
    pvalue_ad=ad_test$p.value
    ks_test=lillie.test(data)
    pvalue_ks=ks_test$p.value
    sw_test=sf.test(data)
    pvalue_sw=sw_test$p.value
  if(pvalue_ad < alpha){
    code_ad[i]=0
  }
  else{
    code_ad[i]=1
  }
  if(pvalue_ks < alpha){
    code_ks[i]=0
  }
  else{
    code_ks[i]=1
  }
  if(pvalue_sw < alpha){
    code_sw[i]=0
  }
  else{
    code_sw[i]=1
  }
  }

result_ad=sum(code_ad)/R
result_ks=sum(code_ks)/R
result_sw=sum(code_sw)/R

list(result.ad = result_ad, result.ks = result_ks, result.sf = result_sw)
}
monte_carlo(100,0.05,100)

monte_carlo_new=function(a,b,alpha,r){
  pval=matrix(nrow=r,ncol=1)
  code_ad=matrix(nrow=r,ncol=1)
  code_ks=matrix(nrow=r,ncol=1)
  code_sw=matrix(nrow=r,ncol=1)
  for (i in 1:r)
  {
    data=rnorm(100,0,1)
    a=a
    b=b
    data_new=c(data,a,b)
    library(nortest)
    pval_ad=ad.test(data_new)$p.value
    pval_ks=lillie.test(data_new)$p.value
    pval_sw=sf.test(data_new)$p.value
    
    if(pval_ad<alpha)
    {
      code_ad[i]=0
    }
    else
    {
      code_ad[i]=1
    }
    if(pval_ks<alpha)
    {
      code_ks[i]=0
    }
    else
    {
      code_ks[i]=1
    }
    if(pval_sw<alpha)
    {
      code_sw[i]=0
    }
    else{
      code_sw[i]=1
    }
  }
  result_ad=sum(code_ad)/r
  result_ks=sum(code_ks)/r
  result_sw=sum(code_sw)/r
  
  list(result.ad=result_ad,result.ks=result_ks,result.sw=result_sw)
}
monte_carlo(5,6,0.05,100)

