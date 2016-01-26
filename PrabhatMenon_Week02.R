###############################
## Prabhat Menon - PREDICT 413, Winter 2016
## Assignment 2: Further explore STL and TS patterns
### Problem 1 ###

# Load the package.
library(fBasics)
par(mfrow=c(1,1))

# User defined function based on Assignment 1 solution by Leslie Cervantes, TA
print.summary <- function(dat,vars) {
  all.stats <- t(basicStats(dat[,vars]))
  summary.stats <- all.stats[,c('Mean', 'Median', 'Stdev','Skewness','Kurtosis','Minimum','Maximum')]
  print(summary.stats,digits=5)
}

# Function to test H_0: symmetric distribution, using the Skewness statistic
test.symmetry <- function(dat, lev=0.05){
  result = "Reject H_0. Distribution is not symmetric around mean..."
  stat <- skewness(dat)/sqrt(6/length(dat))
  pval  <- 2*(1-pnorm(abs(stat)))
  if (pval <= lev)
    result = "Fail to Reject H_0. Distribution is symmetric around mean..."

  print(result)
  print(paste("P-value = ",pval, ", Kurtosis Statistic = ",stat))
}

# Function to test H_0: Zero excess Kurtosis, using the Kurtosis statistic
test.kurtosis_zero <- function(dat, lev=0.05){
  result = "Reject H_0. Excess Kurtosis is not zero..."
  stat <- kurtosis(dat)/sqrt(24/length(dat))
  pval  <- 2*(1-pnorm(abs(stat)))

  if (pval <= lev)
    result = "Fail to Reject H_0. Excess Kurtosis is zero..."

  print(result)
  print(paste("P-value = ",pval, ", Kurtosis Statistic = ",stat))
}


da1=read.table("C:/Prabhat/GoogleDrive/PREDICT413/m-ge3dx8113.txt",header=T)
head(da1)
summary(da1)

print.summary(da1, c('ge','vwretd','ewretd','sprtrn'))

rtn1=log(da1[,3:6]+1)  # Log transform returns
print.summary(rtn1, c('ge','vwretd','ewretd','sprtrn'))

t.test(rtn1$ge)
d1=density(rtn1$ge)
d2=density(rtn1$sprtrn)
plot(d1$x,d1$y,type='l',xlab='returns',ylab='density',main='GE stock')
plot(d2$x,d2$y,type='l',xlab='returns',ylab='density',main='SP index')

### Problem 2 ###

rtn=read.table("C:/Prabhat/GoogleDrive/PREDICT413/d-nflx3dx0913.txt",header=T)
lrtn = log(1+rtn)
nflx=lrtn$nflx
summary(nflx)

test.symmetry(nflx)
test.kurtosis_zero(nflx)

t.test(nflx)

### Problem 3 ###
require(fpp)

summary(ukcars)
par(mfrow=c(1,1))
plot(ukcars)

fit <- stl(ukcars, t.window = 15, s.window = "periodic" ,robust = TRUE)
seasonal.ts = fit$time.series[,"seasonal"]
trend.ts = fit$time.series[,"trend"]
remainder.ts = fit$time.series[,"remainder"]
season.adj.ts = (ukcars-seasonal.ts)

plot(season.adj.ts,ylab="seasonally Adjusted", xlab="years") # "Qtly UK passenger car production ('000s) 1977-2005"
lines(ukcars, col="grey", lty=2)
legend("topleft",lty=2, col=c("black","grey"),c("Adjusted","Data"))

#seas2.ts <- window(season.adj.ts,start=1977,end=2000)  #training set (1977-2000)
#plot(seas2.ts)

# fit4 <- holt(season.adj.ts,damped=TRUE)
# plot(fit4, lty=2)
# print(paste("Method=",fit4$method))
# print(paste("RMSE=",sqrt(fit4$model$mse)))
# accuracy(fit4)

# Holt-Winter Seasonally adjusted Addive Damped trend
fcast <- hw(season.adj.ts, h=2, seasonal="additive", damped=TRUE,level=0.05)
plot(fcast,ylab="forecasted", xlab="years")

# Holt linear method
fcast2 <- holt(season.adj.ts, alpha=0.8, beta=0.2, initial="simple", h=5)
plot(fcast2,ylab="forecasted", xlab="years")

fit.ets <- ets(season.adj.ts)
plot(fit.ets)
summ = rbind(accuracy(fit.ets),accuracy(fcast2),accuracy(fcast))
rownames(summ) <- c("ETS", substr(paste("Method=",fcast2$method),9,9+20),
                    substr(paste("Method=",fcast$method),9,9+20))
summ[,c(-1,-7)]
