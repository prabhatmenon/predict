###############################
## Prabhat Menon - PREDICT 413, Winter 2016
## Assignment 1: 

require(fBasics)

### Problem 1 ###

da=read.table("C:/Prabhat/GoogleDrive/PREDICT413/d-nflx3dx0913.txt",header=T)
head(da)
basicStats(da$nflx)
basicStats(da$vwretd)
basicStats(da$ewretd)
basicStats(da$sprtrn)
rtn=log(da[,3:6]+1)   ### Compute log returns
basicStats(rtn$nflx)
basicStats(rtn$vwretd)
basicStats(rtn$ewretd)
basicStats(rtn$sprtrn)
t.test(rtn$nflx)
d1=density(rtn$nflx)
d2=density(rtn$sprtrn)
par(mfcol=c(1,2))
plot(d1$x,d1$y,xlab='returns',ylab='density',main='Netflix',type='l')
plot(d2$x,d2$y,xlab='returns',ylab='density',main='SP',type='l')

### Problem 2 ###

ge=rtn1$ge
t.test(ge)
tm3=skewness(ge)/sqrt(6/length(ge))
tm3
kurtosis(ge)
tk=kurtosis(ge)/sqrt(24/length(ge))
tk

### Problem 3 ###

# See Chapter 7 (Exponential Smoothing), Hyndeman text



