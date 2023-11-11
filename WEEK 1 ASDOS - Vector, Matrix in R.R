color=c('red','green','yellow')
print(color)
print(class(color))

V1=as.vector(seq(1,10));V1
V2=as.vector(seq(11,20));V2
V1=c(1:10);V1
V4=V1+20;V4
V3=V1-V2;V3

dim(V1)
length(V1)

t(V1)
dim(t(V1))
dim(t(t(V1)))

TV=t(V1)
t(TV)

V2=4*V1
V1
V2
V1*V2

v1=seq(1,10);v1
v2=seq(1,4);v2

outer.prod=v1%*%t(v2)
outer.prod

V1%*%t(V1)

v1=seq(1,10);v1
v2=seq(11,20);v2

in.prod=t(v1)%*%v2
in.prod  

V1=as.vector(seq(1,10))
V2=as.vector(seq(11,20))
V3=V1+V2;V3

Xc=cbind(V1,V2,V3);Xc
Xr=rbind(V1,V2,V3);Xr

dim(Xc)
dim(Xr)

V=V1;V
one=rep(1, length(V));one

sum.V=t(one)%*% V;sum.V
mean.V=sum.V*(1/length(V));mean.V
mean.V=t(one)%*%V*(1/length(V))
mean.V=t(one)%*%V/length(V);mean.V
mean(V)

var.V = (sum((V - mean.V)^2))/(length(V)-1);var.V
var(V)

matrix(1:12, nrow=3)
matrix(1:12, nrow=3, byrow=T)
matrix(1, nrow=2, ncol=2)
matrix(1,2,2)
matrix(1:12, 3, 4)
matrix(0, nrow=5, ncol=5)

x=matrix(1:10, 2,5, byrow=T);x
dim(x)
col(x)
row(x)

x=matrix(1:12,3,4);x
x[row(x)==col(x)]

k=matrix(1:10,2,5);k
k[1:2,3:4]

Xij <- matrix(seq(1:40), ncol=4);Xij
dim(Xij)
rownames(Xij)=paste("S",seq(1,dim(Xij)[1]), sep="")
colnames(Xij)=paste("V",seq(1,dim(Xij)[2]), sep="")
Xij

t(Xij)

set.seed(42)
Xij <- matrix(sample(seq(0,9),40,replace=TRUE), ncol=4);Xij
rownames(Xij)<-paste("S",seq(1,dim(Xij)[1]),sep="")
colnames(Xij)<-paste("V",seq(1,dim(Xij)[2]),sep="")
print(Xij)
(Xij+4)/3

round((Xij+4)/3)
V=V1;V
t(Xij)*V

dim(Xij)
n=dim(Xij)[1];n
one=rep(1,n);one
X.means=t(one)%*%Xij/n;X.means
colMeans(Xij)

X=matrix(1:4,2,2);X
X.inv=solve(X);X.inv
X.diff=Xij-one%*%X.means
X.diff
a=c(10,20,15,43,76,41)
a
e=factor(c(10,20,15,43,76,41))
e
b=factor(c("m","f","m","f","m","f"))
b
d=c("m","f","m","f","m","f")
d
c=c(2,5,8,3,6,1)
myframe=data.frame(a,b,c)
myframe
colnames(myframe)=c("Age","Sex","Siblings")
myframe

myframe[,1]
myframe["Age"]
myframe$Age
myframe[3,3]=2
myframe
myframe[,-2]

read.csv("E:/SEM 4/KOMSTAT/orange4.csv",sep=";")
