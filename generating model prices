

> setwd("E:/MFIN¿Î³Ì/7009 financial engineer/paper/DATA")
> library("readxl")
> DBS1 <- read_excel("mdata1819.xlsx")
> DBS1 <- as.data.frame(DBS1)
> 
#############generate BS model prices###############################
> BS <- function(S, K, T, r, sig, type) {
  +     if (T == 0 & type == "C") return(max(0, S - K))
  +     if (T == 0 & type == "P") return(max(0, K - S))
  +     
    +     d1 <- (log(S / K) + (r + sig^2 / 2) * T) / (sig * sqrt(T))
    +     d2 <- d1 - sig * sqrt(T)
    +     value <- S * pnorm(d1) - exp(-r * T) * K * pnorm(d2)
    +     if (type == "P") return(exp(-r * T) * K + value - S)
    +     return(value)
    + }
> 
  > a <- rep(0, 48)
> 
  > for (j in 1:48){ 
    +     S= DBS1[j,4]
    +     K= DBS1[j,2]
    +     T= DBS1[j,6]
    +     r= DBS1[j,5]
    +     type= DBS1[j,7]
    +     sig = 0.01676655
    +     a[j]= BS(S, K, T, r, sig, type )
    + }
> a1=matrix(a,ncol=1)
> DBS1[,8]=a1
> 

#############generate CH model prices###############################
  > CH <- function(S, K, T, r, sig, type, delta, lamb) {
    +     if (T == 0 & type == "C") return(max(0, S - K))
    +     if (T == 0 & type == "P") return(max(0, K - S))
    +     
      +     d1 <- (log(S / K) + (r + lamb-delta+sig^2 / 2) * T) / (sig * sqrt(T))
      +     d2 <- d1 - sig * sqrt(T)
      +     value <- S * pnorm(d1) * exp(-delta * T) - K * pnorm(d2)* exp(-(lamb + r)* T) + S *(1- exp(-delta * T))
      +     if (type == "P") return(K * exp(-r * T )* (1- exp(-lamb* T) ) + K* exp(-(lamb + r)* T)* pnorm(-d2) -S* exp(-delta * T)* pnorm(-d1))
      +     return(value)
      + }
> 
  > b <- rep(0, 48)
> for (j in 1:48){ 
  +     S= DBS1[j,4]
  +     K= DBS1[j,2]
  +     T= DBS1[j,6]
  +     r= DBS1[j,5]
  +     type= DBS1[j,7]
  +     sig=0.006011652
  +     delta=0.001609609
  +     lamb= 0.001474509
  +     b[j] = CH(S, K, T, r, sig, type, delta, lamb) 
  + }
> 
  > bb=matrix(b,ncol=1)
> DBS1[,9]=bb
> 
#############generate CHbear model prices###############################

  > CH1<- function(S, K, T, r, sig, type, lamb) {
    +     if (T == 0 & type == "C") return(max(0, S - K))
    +     if (T == 0 & type == "P") return(max(0, K - S))
    +     
      +     d1 <- (log(S / K) + (r + lamb +sig^2 / 2) * T) / (sig * sqrt(T))
      +     d2 <- d1 - sig * sqrt(T)
      +     value <- S * pnorm(d1) - K * pnorm(d2)* exp(-(lamb + r)* T) 
      +     if (type == "P") return(K * exp(-r * T )* (1- exp(-lamb* T) ) + K* exp(-(lamb + r)* T)* pnorm(-d2) -S* pnorm(-d1))
      +     return(value)
      + }
> 
  > b1 <- rep(0, 48)
> for (j in 1:48){ 
  +     S= DBS1[j,4]
  +     K= DBS1[j,2]
  +     T= DBS1[j,6]
  +     r= DBS1[j,5]
  +     type= DBS1[j,7]
  +     sig=0.0154735851
  +     lamb= 0.0003912434
  +     b1[j] = CH1(S, K, T, r, sig, type, lamb) 
  + }
> 
  > bb1=matrix(b1,ncol=1)
> DBS1[,10]=bb1
> 
#############generate CHbull model prices###############################

  > CH2 <- function(S, K, T, r, sig, type, delta) {
    +     if (T == 0 & type == "C") return(max(0, S - K))
    +     if (T == 0 & type == "P") return(max(0, K - S))
    +     
      +     d1 <- (log(S / K) + (r -delta+sig^2 / 2) * T) / (sig * sqrt(T))
      +     d2 <- d1 - sig * sqrt(T)
      +     value <- S * pnorm(d1) * exp(-delta * T) - K * pnorm(d2)* exp(- r* T) + S *(1- exp(-delta * T))
      +     if (type == "P") return(K* exp(-r* T)* pnorm(-d2) -S* exp(-delta * T)* pnorm(-d1))
      +     return(value)
      + }
> 
  > b2 <- rep(0, 48)
> for (j in 1:48){ 
  +     S= DBS1[j,4]
  +     K= DBS1[j,2]
  +     T= DBS1[j,6]
  +     r= DBS1[j,5]
  +     type= DBS1[j,7]
  +     sig=0.0148109256
  +     delta = 0.0005845522
  +     b2[j] = CH2(S, K, T, r, sig, type, lamb) 
  + }
> 
  > bb2=matrix(b2,ncol=1)
> DBS1[,11]=bb2
> 
  > write.table(DBS1,file="DBS1.csv",sep = ",",row.names=FALSE)
