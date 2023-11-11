M=matrix(c(1:10),nrow=5,ncol=2,dimnames=list(c('a','b','c','d','e'),c('A','B')))

M
M[1,]
M[,1]
M[3,2]
M["e","A"]

A <- matrix(sample(seq(0,2), 9, replace = TRUE), ncol=3);A
B <- matrix(11:19,3,3);B
A[row(A) == col(A)]
B[row(B) == col(B)]

C = A +5;C
C*2.5
round(C*2.5)
A*C
A%*%C
A
C=A+5;C
solve(C)

library(readxl)
sabun = read_excel("E:/SEM 4/KOMSTAT/LATIHAN SOAL/sabun.xlsx")
