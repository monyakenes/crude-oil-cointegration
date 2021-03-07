#crude oil
library(urca)
library(forecast)
install.packages("tidyverse")
library(tidyverse)
install.packages("readxl")
require(readxl)
install.packages("MTS")
require(MTS)
setwd("C:/Users/monya/Desktop/ch2")
da = read_excel("sp500_data.xlsx")
dim(da)
head(da)

#com_ = da[,2]
#con_ = da[,3]
fin_ = da[,4]
inf_ = da[,5]

#com = ts(log(na.omit(as.numeric(unlist(com_[[1]])))))
#con = ts(log(na.omit(as.numeric(unlist(con_[[1]])))))
fin = ts(log(na.omit(as.numeric(unlist(fin_[[1]])))))
inf = ts(log(na.omit(as.numeric(unlist(inf_[[1]])))))

z = cbind(fin, inf)
zt = diffM(log(z,1))
apply(z, 2, adfTest)
apply(zt, 2, adfTest)


lags = VARorder(z, maxp = 10, output = T)
lags

m1=ar(diff(com),method = "mle")
m1$order
adfTest(com,lags=8,type="c")
m2=ar(diff(con),method="mle")
m2$order
adfTest(con,lags=3,type="c")
m3=ar(diff(fin),method="mle")
m3$order
adfTest(con,lags=0,type="c")
m4=ar(diff(inf),method="mle")
m4$order
adfTest(con,lags=8,type="c")
 
ab = ca.jo(z, K = 3, ecdet = c("none"))
summary(ab)

ac = ca.jo(z, K = 3, ecdet = c("none"), spec = c("transitory"))
summary(ac)

ad = ca.jo(z, K = 3, ecdet = c("none"), type = c("trace"), spec = c("transitory"))
summary(ad)

