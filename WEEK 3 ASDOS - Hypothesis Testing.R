#Lower Tail Test of Population Mean with Known Variance
xbar = 9900             
mu0 = 10000             
sigma = 120             
n = 30                 
z = (xbar-mu0)/(sigma/sqrt(n));z

alpha = 0.05 
z.alpha = qnorm(1-alpha);-z.alpha 

pval = pnorm(z);pval
pval{
  if(pval>alpha){
    print("Gagal Tolak H0")
  }
  else{
    print("Tolak H0")
  }
}
#Upper Tail Test of Population Mean with Known Variance

xbar = 2.1              
mu0 = 2                
sigma = 0.25           
n = 35 
z = (xbar-mu0)/(sigma/sqrt(n)) ;z 

alpha = 0.05 
z.alpha = qnorm(1-alpha);z.alpha 

pval = pnorm(z, lower.tail=FALSE) ;pval
pval{
  if(pval>alpha){
    print("Gagal Tolak H0")
  }
  else{
    print("Tolak H0")
  }
}

#Two-Tailed Test of Population Mean with Known Variance
xbar = 14.6           
mu0 = 15.4            
sigma = 2.5             
n = 35                  
z = (xbar-mu0)/(sigma/sqrt(n));z
alpha = 0.05 
z.half.alpha = qnorm(1-alpha/2) 
c(-z.half.alpha, z.half.alpha) 
pval = 2*pnorm(z);pval
pval{
  if(pval>alpha){
    print("Gagal Tolak H0")
  }
  else{
    print("Tolak H0")
  }
}

# Lower Tail Test of Population Mean with Unknown Variance
xbar = 9900             
mu0 = 10000             
s = 125                
n = 30               
t = (xbar-mu0)/(s/sqrt(n));t
alpha = 0.05 
t.alpha = qt(1-alpha, df=n-1);-t.alpha
pval{
  if(pval>alpha){
    print("Gagal Tolak H0")
  }
  else{
    print("Tolak H0")
  }
}

# Upper Tail Test of Population Mean with Unknown Variance
xbar = 2.1             
mu0 = 2                 
s = 0.3                 
n = 35                  
t = (xbar-mu0)/(s/sqrt(n)) ;t

alpha =0.05 
t.alpha = qt(1-alpha, df=n-1);t.alpha
pval{
  if(pval>alpha){
    print("Gagal Tolak H0")
  }
  else{
    print("Tolak H0")
  }
}
#Two-Tailed Test of Population Mean with Unknown Variance
xbar = 14.6            
mu0 = 15.4              
s = 2.5                
n = 35                
t = (xbar-mu0)/(s/sqrt(n));t

alpha = 0.05 
t.half.alpha = qt(1-alpha/2, df=n-1) 
c(-t.half.alpha, t.half.alpha) 
pval{
  if(pval>alpha){
    print("Gagal Tolak H0")
  }
  else{
    print("Tolak H0")
  }
}
#                           LATIHAN SOAL                              #

#1 - Lower Tail Test of Population Mean with Known Variance

#H0 : mu0 >= 16
#H0 : mu0 < 16
mu0=16.43;mu0
sigma=0.8;sigma
n=15;n
xbar=16;xbar
z=(xbar-mu0)/(sigma/sqrt(n)); z

alpha=0.05;alpha
z.alpha=qnorm(1-alpha);-z.alpha
pval = pnorm(z);pval
{
  if(pval>alpha)
  {
    print('Failed to Reject H0')
  }
  else
  {
    print('Reject H0')
  }
}

#2 -  Upper Tail Test of Population Mean with Known Variance

#H0 : mu0 <= 40
#H0 : mu0 > 40

n = 20;n
sigma = 45;sigma
mu0 = 40;mu0
alpha = 0.05

z = (xbar-mu0)/(sigma/sqrt(n));z
z.alpha = qnorm(1-alpha);z.alpha 

pval = pnorm(z, lower.tail=FALSE) ;pval
pval{
  if(pval>alpha){
    print("Gagal Tolak H0")
  }
  else{
    print("Tolak H0")
  }
}

#3 - Upper Tail Test of Population Mean with Known Variance

#H0 : mu0 <= 100
#H1 : mu0 > 100

n = 16;n
xbar = 108;xbar
s = 12;s
mu0 = 100;mu0
alpha = 0.05

z = (xbar-mu0)/(sigma/sqrt(n));z
z.alpha = qnorm(1-alpha);z.alpha 

pval = pnorm(z, lower.tail=FALSE) ;pval
pval{
  if(pval>alpha){
    print("Gagal Tolak H0")
  }
  else{
    print("Tolak H0")
  }
}

#4 - Lower Tail Test of Population Mean with Unknown Variance

#H0 : mu0 >= 5 
#H0 : mu0 < 5
a=c(4,3,2,3,1,7,2,1,1,2);a
sigma=sd(a);sigma
mu0=5;mu0
xbar=mean(a);xbar
n=length(a);n
t = (xbar-mu0)/(sigma/sqrt(n));t
alpha=0.05
t.alpha = qt(1-alpha, df=n-1) 
-t.alpha
pval = pt(t, df=n-1);pval
pval{
  if(pval>alpha)
  {
    print('Failed to Reject H0')
  }
  else
  {
    print('Reject H0')
  }
}

#5 - Two-Tailed Test of Population Mean with Unknown Variance

#H0 : mu0 = 8
#H0 : mu0 != 8
n = 35
xbar = 7.91
mu0 = 8
sigma = sqrt(0.03)
alpha = 0.01
t = (xbar-mu0)/(s/sqrt(n));t
t.half.alpha = qt(1-alpha/2, df=n-1) 
c(-t.half.alpha, t.half.alpha)
pval = 2* pt(t, df=n-1);pval
pval{
  if(pval>alpha)
  {
    print('Failed to Reject H0')
  }
  else
  {
    print('Reject H0')
  }
}

# 6  - Lower Tail Test of Population Mean with Known Variance
# Ho : mu >= 50,000
# H1 : mu < 50,000
xbar = 46500
mu0 = 50000
sigma = 8000
n = 28
z = (xbar-mu0)/(sigma/sqrt(n));z

alpha = 0.05
z.alpha = qnorm(1-alpha)
-z.alpha

pval = pnorm(z);pval

# 7 -  Lower Tail Test of Population Mean with Known Variance

#H0 : mu0 >= 19
#H0 : mu0 < 19
n = 40
xbar = 18.1
mu0 = 19
sigma = 1.3
alpha = 0.05
z = (xbar-mu0)/(sigma/sqrt(n));z
z.alpha = qnorm(1-alpha);-z.alpha 
pval = pnorm(z);pval
pval{
  if(pval>alpha){
    print("Gagal Tolak H0")
  }
  else{
    print("Tolak H0")
  }
}
# 8 - Two-Tailed Test of Population Mean with Known Variance

# H0 : mu0 = 100
# H1 : mu0 != 100
sigma = 20
mu0 = 100
n = 12
xbar = 95
z = (xbar-mu0)/(sigma/sqrt(n);z
alpha = 0.01
z.half.alpha = qnorm(1-alpha/2)
c(-z.half.alpha, z.half.alpha)
pval = 2*pnorm(z);pval
pval{
  if(pval>alpha)
  {
    print('Gagal Tolak H0')
  }
  else
  {
    print('Tolak H0')
  }
}

# 9 - Upper Tail Test of Population Mean with Known Variance

# H0 : mu0 <= 4.5
# H0 : mu0 > 4.5

n = 49
xbar = 5.1
mu0 = 4.5
sigma = 1.2
alpha = 0.05
z = (xbar-mu0)/(sigma/sqrt(n));z
z.alpha = qnorm(1-alpha);z.alpha 
pval = pnorm(z);pval
pval{
  if(pval>alpha){
    print("Gagal Tolak H0")
  }
  else{
    print("Tolak H0")
  }
}
# 10 - Two-Tailed Test of Population Mean with Unknown Variance

# H0 : mu0 = 10
# H0 : mu0!= 10

data = c(12, 4, 15, 3, 11, 8, 6, 8);data
xbar = mean(data)        
mu0 = 10             
s = sd(mean)             
n = length(data)                
t = (xbar-mu0)/(s/sqrt(n));t

alpha = 0.05 
t.half.alpha = qt(1-alpha/2, df=n-1) 
c(-t.half.alpha, t.half.alpha) 
pval{
  if(pval>alpha){
    print("Gagal Tolak H0")
  }
  else{
    print("Tolak H0")
  }
}

# 11 - Upper Tail Test of Population Mean with Unknown Variance 
# h0 : mu0 <= 4
# h1 : mu0 > 4
data = c(5, 4, 7, 3, 6, 4, 5, 3, 6, 3, 8, 5)
xbar = mean(data);xbar
mu0 = 4
s = sd(data)
n = length(data)
t = (xbar-mu0)/(s/sqrt(n))
t

alpha = 0.05
t.alpha = qt(1-alpha, df=n-1)
t.alpha

pval = pt(t, df=n-1, lower.tail=FALSE)
pval
{
  if(pval>alpha)
  {
    print("Gagal Tolak H0")
  }
  else
  {
    print("Tolak H0")
  }
}

#12 - Upper Tail Test of Population Mean with known Variance 

#H0 : miu <= 71.121
#H1 : miu > 71.121
xbar = 69.110
sigma = 1.489
n = 41
mu0 = 71.121

z = (xbar-mu0)/(sigma/sqrt(n)) ;z 

alpha = 0.05 
z.alpha = qnorm(1-alpha);z.alpha 

pval = pnorm(z, lower.tail=FALSE) ;pval
pval{
  if(pval>alpha){
    print("Gagal Tolak H0")
  }
  else{
    print("Tolak H0")
  }
}
#13 - Upper Tail Test of Population Mean with Unknown Variance 
data =c(1.11, 1.07, 1.11, 1.07, 1.12, 1.08, 0.98, 0.98, 1.02, 0.95, 0.95);data
xbar=mean(data);xbar
s = sd(data);s
n=length(data);n
mu0=1
#H0 : miu <= 1
#H1 : miu > 1

t = (xbar-mu0)/(s/sqrt(n));t

alpha = 0.05 
t.alpha = qt(1-alpha, df=n-1);t.alpha
pval = pt(t, df=n-1, lower.tail=FALSE) ; pval
pval{
  if(pval>alpha){
    print("Gagal Tolak H0")
  }
  else{
    print("Tolak H0")
  }
}
