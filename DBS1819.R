
R version 4.1.3 (2022-03-10) -- "One Push-Up"
Copyright (C) 2022 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64 (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

> setwd("E:/MFIN¿Î³Ì/7009 financial engineer/paper/DATA")
> library("readxl")
> DBS1 <- read_excel("mdata1819.xlsx")
> DBS1 <- as.data.frame(DBS1)
> 
  > View(DBS1)
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
  > b=0
> g=function(sig) {
  +     a <- matrix(rep(0, 48), ncol = 1)
  +     for (j in 1:48){ 
    +         S= DBS1[j,4]
    +         K= DBS1[j,2]
    +         T= DBS1[j,6]
    +         r= DBS1[j,5]
    +         type= DBS1[j,7]
    +         p= DBS1[j,3]
    +         a[j,1] = ( (BS(S, K, T, r, sig, type )-p)/p)^2
    +         b= a[j,1]+b
    +     }
  +     g = b
  + }
> optimize(g, interval=c(0,1.5))
$minimum
[1] 0.01608858

$objective
[1] 3.882655

> 
  > b=0
> g=function(sig) {
  +     a <- matrix(rep(0, 48), ncol = 1)
  +     for (j in 1:48){ 
    +         S= DBS1[j,4]
    +         K= DBS1[j,2]
    +         T= DBS1[j,6]
    +         r= DBS1[j,5]
    +         type= DBS1[j,7]
    +         p= DBS1[j,3]
    +         a[j,1] = ( BS(S, K, T, r, sig, type )-p)^2
    +         b= a[j,1]+b
    +     }
  +     g = b
  + }
> optimize(g, interval=c(0,1.5))
$minimum
[1] 0.01676655

$objective
[1] 0.003544683

> 
  > 
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
  > b=0
> g=function(x) {
  +     sig <- x[1]
  +     delta <- x[2]
  +     lamb <- x[3]
  +     
    +     a <- matrix(rep(0, 48), ncol = 1)
    +     for (j in 1:48){ 
      +         S= DBS1[j,4]
      +         K= DBS1[j,2]
      +         T= DBS1[j,6]
      +         r= DBS1[j,5]
      +         type= DBS1[j,7]
      +         p= DBS1[j,3]
      +         a[j,1] = ( CH(S, K, T, r, sig, type, delta, lamb )-p)^2
      +         b= a[j,1]+b
      +     }
    +     return(b)
    + }
> init <- c(sig=0, delta=0, lamb=0)
> optim(par = init, fn = g)
$par
sig       delta        lamb 
0.006011652 0.001609609 0.001474509 

$value
[1] 0.002673166

$counts
function gradient 
146       NA 

$convergence
[1] 0

$message
NULL

> 
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
  > b=0
> g=function(y) {
  +     sig <- y[1]
  +     lamb <- y[2]
  +     
    +     a <- matrix(rep(0, 48), ncol = 1)
    +     for (j in 1:48){ 
      +         S= DBS1[j,4]
      +         K= DBS1[j,2]
      +         T= DBS1[j,6]
      +         r= DBS1[j,5]
      +         type= DBS1[j,7]
      +         p= DBS1[j,3]
      +         a[j,1] = ( CH1(S, K, T, r, sig, type, lamb )-p)^2
      +         b= a[j,1]+b
      +     }
    +     return(b)
    + }
> init1 <- c(sig=0, lamb=0)
> optim(par = init1, fn = g)
$par
sig         lamb 
0.0154735851 0.0003912434 

$value
[1] 0.003496082

$counts
function gradient 
73       NA 

$convergence
[1] 0

$message
NULL

> 
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
  > b=0
> g=function(z) {
  +     sig <- z[1]
  +     delta <- z[2]
  +     
    +     a <- matrix(rep(0, 48), ncol = 1)
    +     for (j in 1:48){ 
      +         S= DBS1[j,4]
      +         K= DBS1[j,2]
      +         T= DBS1[j,6]
      +         r= DBS1[j,5]
      +         type= DBS1[j,7]
      +         p= DBS1[j,3]
      +         a[j,1] = ( CH2(S, K, T, r, sig, type, delta )-p)^2
      +         b= a[j,1]+b
      +     }
    +     return(b)
    + }
> init2 <- c(sig=0, delta =0)
> optim(par = init2, fn = g)
$par
sig        delta 
0.0148109256 0.0005845522 

$value
[1] 0.003442851

$counts
function gradient 
75       NA 

$convergence
[1] 0

$message
NULL

> 
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
    +     
      +     a1=matrix(a,ncol=1)
      +     DBS1[,8]=a1
      +     
        + 9090
      + uuuu
      + qwqwqwqqqq
      + 5=0
      + qw1234..
      + }
Error: object 'uuuu' not found
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