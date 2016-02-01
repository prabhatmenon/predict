set.seed(10)

phi <- c(-0.6,0,0.3,0.6,0.9,1)  # Phi values for AR(1) series
T <- 100  #T time series entries

e <- rnorm(T) #Create T random values {0,1}

mts = NULL   # Initialize an Empty muti-timeseries object
for(z in phi)
{
  y = ts(numeric(T))  # Initialize a TS object with T, 0 entries
  for(i in 2:T)
      y[i] <- z*y[i-1] + e[i]
  mts = cbind(mts,y)
}

colnames(mts)=phi  # Set column names with corresponding Phi values
plot.ts(mts,ylab="phi",main="AR(1) model for various Phi")
par(mfrow=c(3,1))
# Plot select ACF plots
acf(mts[,"-0.6"],main=paste("Phi=-0.6"))
acf(mts[,"0"],main=paste("Phi=0"))
acf(mts[,"1"],main=paste("Phi=1"))
