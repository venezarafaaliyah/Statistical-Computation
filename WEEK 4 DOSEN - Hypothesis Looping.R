library(readxl)
Data = read.csv("E:/SEM 4/KOMSTAT/koding - week 3.csv", sep=";");Data
Data_berat = Data[,1];Data_berat
uji_hipotesa = function(data, mu0, pilihan, alpha)
{
  
  n = length(data)
  sigma = sd(data)
  xbar = mean(data)
  z = (xbar-mu0)/(sigma/sqrt(n))
  
  if (pilihan == 1)	#lower test
  {
    z.alpha = qnorm(1-alpha)
    pval = pnorm(z)
  }
  else if (pilihan == 2)	#upper test
  {
    z.alpha = qnorm(1-alpha)
    pval = pnorm(z, lower.tail=FALSE)
  }
  else	#not equal
  {
    z.alpha = qnorm(1-alpha/2)
    pval = 2*pnorm(z)
  }
  
  if (pval <= alpha)
  {
    print("keputusan : tolak H0")
  }
  else
  {
    print("keputusan : gagal tolak H0")
  }
  
  list (z.hitung = z, z.tabel = z.alpha, pvalue = pval)
}
uji_hipotesa(Data_berat,70,0.05,1)

uji_hipotesa2 = function(data, mu0, pilihan, alpha)
{
  
  n = length(data)
  sigma = sd(data)
  xbar = mean(data)
  z = (xbar-mu0)/(sigma/sqrt(n))
  
  if (pilihan == 1)	#lower test
  {
    z.alpha = qnorm(1-alpha)
    pval = pnorm(z)
  }
  else if (pilihan == 2)	#upper test
  {
    z.alpha = qnorm(1-alpha)
    pval = pnorm(z, lower.tail=FALSE)
  }
  else	#not equal
  {
    z.alpha = qnorm(1-alpha/2)
    pval = 2*pnorm(z)
  }
  
  if (pval <= alpha)
  {
  cat("==================================","\n")
  cat("pvalue=",pval,"\n")
  cat("Keputusan : Tolak H0","\n")
  }
  else
  {
    cat("==================================","\n")
    cat("pvalue=",pval,"\n")
    cat("keputusan : gagal tolak H0","\n")
  }
  
  list (z.hitung = z, z.tabel = z.alpha, pvalue = pval)
}
uji_hipotesa2(Data_berat,70,0.05,1)

#1 - uppertailed
data1 = read.csv("E:/SEM 4/KOMSTAT/nilai - week 3.csv", sep=";");data1
uji_hipotesa(data1$Kuis1,75,2,0.05)
uji_hipotesa2(data1$Kuis1,75,2,0.05)
#2 - lowertailed
uji_hipotesa(data1$ETS,65,1,0.05)
uji_hipotesa2(data1$ETS,65,1,0.05)
#3 - twotailed
uji_hipotesa(data1$Tugas,80,3,0.05)
uji_hipotesa2(data1$Tugas,80,3,0.05)
#4 alpha - 0.01
uji_hipotesa2(data1$Kuis1,75,2,0.1)
uji_hipotesa2(data1$ETS,65,1,0.1)
uji_hipotesa2(data1$Tugas,80,3,0.1)

#Bootstrap

boot = function(data, R, m, Uji)
{
  
  N = length(data)
  rata_sample = matrix(nrow=R, ncol=1)
  
  for(i in 1:R)
  {
    data_sample = sample(data,m,replace=T)
    rata_sample[i] = mean(data_sample)
  }
  rata_bootstrap = mean(rata_sample)
  
  cat("===============================","\n")
  cat("hasil rata-rata bootstrap=",rata_bootstrap,"\n")
  cat("===============================","\n")
  
  sd_boot = sd(rata_sample)
  N_boot = R
  
  BB = rata_bootstrap - 1.96*(sd_boot/sqrt(N_boot))
  BA = rata_bootstrap + 1.96*(sd_boot/sqrt(N_boot))
  
  if(Uji >= BB & Uji <= BA)
  {
    cat("Kesimpulan = Gagal tolak H0", "\n")
  }
  else
  {
    cat("Kesimpulan = tolak H0", "\n")
  }
  
  hist(as.vector(rata_sample))
  list(rata_bootstrap=rata_bootstrap, sd = sd_boot, BB,BA)
}
boot(Data$berat,100000,6,75)


