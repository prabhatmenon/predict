#### Problems 1-3 ###
#### UM Consumer Sentiment ###

require(fBasics)
require(xts)
par(mfrow=c(1,1))

da=read.table("m-umcsent.txt",header=T)
head(da)
tail(da)

# Plot the monthly consumer sentiment series.
# build an XTS object
tmseries = xts(da$VALUE,as.Date(paste(da[,1],da[,2],da[,3],sep="-")))
colnames(tmseries)=c("VALUE")
head(tmseries)

par(mfrow=c(2,1))
plot(tmseries,xlab="year",major.format="%Y",ylab="sentiment",main="UM Consumer Sentiment")

# from solution
sentiment <- ts(da$VALUE,start=c(1978,1),frequency=12)
plot(sentiment,main='Customer Sentiment')

# (b) Is there a unit root in the monthly sentiment series? Why?

suppressMessages(library(fpp))
adf.test(sentiment)

#(c ) Consider the change series of the sentiment,
# i.e. the first differenced data. Test the hypothesis that the
# expected change of the sentiment is zero versus the alternative
# that the expected change is non-zero.

chg <- diff(sentiment)
t.test(chg)
# Fail to reject null hypothesis that change in sentiment is 0

# (d) Focus on the change series.
# Test the null hypothesis H0 : ρ1 = ρ2 = · · · = ρ12 = 0 versus the alternative Ha : ρi != 0 for some i ∈ [1, 12].
# Draw your conclusion.
Box.test(chg,lag=12,type='Ljung')
# Reject null hypothesis that correlations are equal


#2. Again, consider the change series of consumer sentiment of Problem 1.
#(a) Use the command ar with method mle to find the order.

m1 <- ar(chg,method="mle")
print(m1$order)
# (b) Build an AR model based on the selected order for the change series. Perform model checking to validate the fitted model. Write down the model.
m2=arima(chg,order=c(5,0,0),include.mean=F)
print(m2)

tsdiag(m2) ### Model checking

# (c ) Does the model imply the existence of business cycles in consumer sentiment? Why?

p1=c(1,-m2$coef)
r1=polyroot(p1)
r1
# There are complex roots which suggest business cycles

# (d) Obtain 1-step to 4-step ahead point and 95% interval forecasts for the change series of consumer sentiment at the forecast origin August 1, 2013 (the last data point).

### prediction 1 to 4-step ahead
m2p=predict(m2,4)
#Predictions
m2p$pred

#Standard Errors
m2p$se

#Lower Confidence Level
lcl=m2p$pred-1.96*m2p$se
lcl

#Upper Confidence Level
ucl=m2p$pred+1.96*m2p$se
ucl

# 3. Consider, again, the monthly change series of consumer sentiment of Problem 1.
#(a) Simplify the fitted AR model of Problem 2 by removing parameter estimates with t-ratio less than 1.2 in absolute. [Use the fixed subcommand.]

# Find the paramaters with t-ration less than 1.2 in absolute
m2$coef/c(sqrt(m2$var.coef[1,1]),sqrt(m2$var.coef[2,2]),sqrt(m2$var.coef[3,3]),sqrt(m2$var.coef[4,4]),sqrt(m2$var.coef[5,5]))
# ar1 and ar4 have t-values less than 1.2

c1=c(0,NA,NA,0,NA)
m3=arima(chg,order=c(5,0,0),include.mean=F,fixed=c1)
m3

# b) Is the model adequate? Why?
tsdiag(m3) ### Model checking

# Appears adequate as we cannot reject null hypothesis of no serial autocorrelations due to all p-values being larger than 0.05
# (c ) Compare the simplified model with the AR model built in Problem 2. In terms of in-sample fitting, which model is preferred? Why?

# In terms of in-sample AIC, the simplified model seems slightly better.
# (d) Does the simplified model imply the existence of business cycles? Why?

p1=c(1,-m3$coef)
r1=polyroot(p1)
r1

# There are complex roots which suggest business cycles

# e) Use backtest to compare the two AR models. You may start the forecast origin at t =380. Which model is preferred? Why?

options(warn=-1)
source('backtest.R')
backtest(m2,chg,380,h=1,inc.mean=F)
backtest(m3,chg,380,h=1,inc.mean=F,fixed=c1)
# Simplified model again performs slightly better, according to out-of-sample RMSE.


