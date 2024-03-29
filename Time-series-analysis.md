Time series Analysis
================

## Preparation of the data

We begin by loading our data and convert it to a time series using the
`as.ts()` function. Then we plot our data.

``` r
ts <- read_csv(filepath,show_col_types = FALSE)
tsData<-as.ts(ts) ##convert to time series data
plot.ts(tsData)
grid(nx = NA,
     ny = NULL,
     lty = 2, col = "gray", lwd = 2)
```

<img src="Time-series-analysis_files/figure-gfm/unnamed-chunk-2-1.png" width="75%" style="display: block; margin: auto;" />
It’s very hard to distinguish if there is a trend or seasonality by
looking at the time series graph.We proceed by looking at the ACF plot.

``` r
acf(tsData) # autocorrelation
```

<img src="Time-series-analysis_files/figure-gfm/unnamed-chunk-3-1.png" width="75%" style="display: block; margin: auto;" />
We can observe that there is a trend and seasonal component in our time
series.The slow decrease in the ACF as the lags increase is due to the
trend component, while the periodic sinoid shape is due the seasonality.

## Parameters for the linear trend component

We fit a linear model by regressing our series `ts` against time and
plot the regression line. The upward trend becomes clearer in the new
plot.

``` r
##Linear regression
trModel<- lm(tsData ~ time(tsData))
##Plot time series with trending line
plot.ts(tsData)
grid(nx = NA,
     ny = NULL,
     lty = 2, col = "gray", lwd = 2)
abline(reg=trModel)
```

<img src="Time-series-analysis_files/figure-gfm/unnamed-chunk-4-1.png" width="65%" style="display: block; margin: auto;" />

We remove the trend by computing the first difference.The fitted values
will be the estimates `t` for the trend component. In the new graph
below we can observe a shift; the mean of our values are around the
x-axis so there’s no need to compute another difference again.

``` r
##Removing the trend
trend <- trModel$fitted.values
d.data <- tsData - trend
##Plot the data
plot(d.data)
grid(nx = NA,
     ny = NULL,
     lty = 2, col = "gray", lwd = 2)
```

<img src="Time-series-analysis_files/figure-gfm/unnamed-chunk-5-1.png" width="65%" style="display: block; margin: auto;" />
The parameters for the linear trend component are:

|              |          x |
|--------------|-----------:|
| (Intercept)  | -0.9175246 |
| time(tsData) |  0.0010170 |

## Seasonal component

To interpret the seasonality and determine the frequency, we plot the
ACF and PACF after removing the linear trend.
<img src="Time-series-analysis_files/figure-gfm/unnamed-chunk-7-1.png" width="65%" style="display: block; margin: auto;" /><img src="Time-series-analysis_files/figure-gfm/unnamed-chunk-7-2.png" width="65%" style="display: block; margin: auto;" />
Looking at the ACF plot, we observe that there is a periodic trend with
positive and negative spikes following each other. The highest peak
occurs at lag 28 and there is another less significant peak at lag
14.The PACF is also kind of periodic and has some significant spikes
from lag 3 to 7 and some small spikes coming after lag 15. Then, it cuts
off after lag 24.

We are going to perform spectral analysis to get an idea of the
frequency and the periodic behavior.To learn where the dominant peak is
located, we assign a data frame to the spectrum output.We find the
frequency `f` at which the corresponding spectral density is largest and
then we compute the period `T` corresponding to `f` by `T=1/f`. There is
a large spike at frequency `0.00357`, which comes out to a cycle with a
period of 1/0.00357 ∼ 28 .

``` r
spec<-spectrum(d.data)
```

<img src="Time-series-analysis_files/figure-gfm/unnamed-chunk-8-1.png" width="65%" style="display: block; margin: auto;" />

``` r
spec.df <- data.frame(spec=spec$spec, freq=spec$freq)
frequency<-spec$freq[spec$spec==max(spec$spec)]
period<-round(1/frequency)
```

| frequency | period |
|----------:|-------:|
| 0.0357333 |     28 |

We determine that there is a period of 28 lags which we will use to
remove the seasonal component from the original data. In order to do
this we compute the difference *y*<sub>*t*</sub> − *S*<sub>*t*</sub>,
where *S*<sub>*t*</sub> = *S*<sub>28</sub>.

``` r
diff.ts=diff(d.data, lag=T)  #lag difference 
```

<img src="Time-series-analysis_files/figure-gfm/unnamed-chunk-11-1.png" width="55%" style="display: block; margin: auto;" /><img src="Time-series-analysis_files/figure-gfm/unnamed-chunk-11-2.png" width="55%" style="display: block; margin: auto;" />
In the ACF, there are two significant spikes at lag 1 and 2 and a less
significant at lag 3.Then we observe a decreasing trend in values,
although there is a cluster of less significant negative and positive
spikes around 14 and 28. This indicates a pattern around multiples of
14. The auto correlations in PACF are smaller in scale.There is a clear
spike at lag 1, 2 and 3 and a cluster of negative spikes around lag 14.
The spikes in the PACF around lag 14 and 28 combined with spikes around
the same lags in the ACF indicates that a four-weekly seasonal component
may exist. Moreover, the spikes in low lags in the PACF in combination
with the decreasing trend of the ACF plot suggests a possible non
seasonal AR.

## Dickey-Fuller test

After removing the trending and seasonal components, we’re going to test
if the time series is stationary.We’re using a lag order of k=1,for the
simple Dickey-Fuller test.

``` r
adf.test(diff.ts, k=1)
```

    ## Warning in adf.test(diff.ts, k = 1): p-value smaller than printed p-value

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  diff.ts
    ## Dickey-Fuller = -42.094, Lag order = 1, p-value = 0.01
    ## alternative hypothesis: stationary

We see that the p-value is (smaller than) 0.01, which suggests that we
can reject the null hypothesis and conclude that the time series is
stationary.However, we can’t claim that this is evidence in support of a
particular alternative hypothesis (e.g. stationarity)

## Suitability of test

The Dickey–Fuller test that we used above is used to test the null
hypothesis, which is that a unit root is present in an autoregressive
time series model.However, it can not detect other forms of
nonstationarity as the alternative hypothesis is different depending on
which version of the test is used.  
Our time series is clearly seasonal and more complicated than is
captured by a simple AR(1) model. A more appropriate test would be the
augmented Dickey Fuller test which was developed for general ARMA(p, q)
models with unknown orders. This seems to be closer to our case and thus
more appropriate.
