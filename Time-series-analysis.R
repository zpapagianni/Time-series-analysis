library(tseries)
library(tidyverse)

ts <- read_csv(filepath,show_col_types = FALSE)
tsData<-as.ts(ts)
plot.ts(tsData)
summary(ts)
##Before removing trend
acf(tsData) # autocorrelation
## Based the ACF we should start with 
pacfRes <- pacf(tsData)

##Diagnosis for trend
trModel<- lm(tsData ~ time(tsData))
plot.ts(tsData)
grid(nx = NA,
     ny = NULL,
     lty = 2, col = "gray", lwd = 2)
abline(reg=trModel)
##Remove trend
trend <- trModel$fitted.values
d.data <- tsData - trend
plot(d.data , type = "line")


#Estimate frequency, interpret the seasonality, and use seasonal differencing to remove this
##component from the data.
old_par <- par(mfrow = c(1, 2), mar=c(5, 4, 3, 2))
##ACF and PACF plots
acf(d.data) # autocorrelation
## Based the ACF we should start with 
pacfRes <- pacf(d.data) 

##Periodogram
spec<-spectrum(d.data,log="no")
spec.df <- data.frame(spec=spec$spec, freq=spec$freq)
f<-spec$freq[which.max(spec$spec)]
T<-round(1/f)

diff.ts=diff(d.data,lag=28)
old_par <- par(mfrow = c(3, 1))
plot(diff.ts)
acf(diff.ts)
pacf(diff.ts)

#Dickey-Fuller Test 
adf.test(diff.ts, k=1)

plot(resid(trModel), type="l")  # resid(trModel) contains the de-trended series.
hist(trModel$residuals,
     col = 'red',
     xlab = 'Error',
     main = 'Histogram of Residuals',
     freq = FALSE)
lines(density(model$residuals))

##Diagnosis for Seasonality
spectrum(tsData$x) 

