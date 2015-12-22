---
output:
  md_document:
    variant: markdown_github
---

<!-- README.md is generated from README.Rmd. Please edit that file -->






## Package contents

The `weathermetrics` package provides the following functions to calculate or convert between several weather metrics:

- **`celsius.to.fahrenheit`:** Convert a vector of temperatures in degrees Celsius to degrees Fahrenheit
- **`fahrenheit.to.celsius`:** Convert a vector of temperatures in degrees Fahrenheit to degrees Celsius
- **`dewpoint.to.humidity`:** Calculate a vector of relative humidity values from vectors of air temperature and dew point temperature
- **`humidity.to.dewpoint`:** Calculate a vector of dew point temperatures from vectors of air temperature and relative humidity 
- **`heat.index`:** Calculate a vector of heat index values
from vectors of air temperature and either dew point temperature or relative humidity

All algorithms are adapted for R from the algorithms used by the United States National Weather Service's [online heat index calculator](http://www.wpc.ncep.noaa.gov/html/heatindex.shtml) (accessed December 18, 2015). 

## Converting or calculating weather metrics

### Converting between Celsius and Fahrenheit

This package includes two functions to convert between Celsius and Fahrenheit, `celsius.to.fahrenheit` and `fahrenheit.to.celsius`. As an example of how to use these functions, the `lyon` dataset that comes with this package gives air temperature (`lyon$TemperatureC`) and dew point temperature (`lyon$DewpointC`), both in degrees Celsius, for Lyon, France, for the week of June 18, 2000. 

To convert to degrees Fahrenheit, use the `celsius.to.fahrenheit` function:


```r
data(lyon)
lyon$T_F <- celsius.to.fahrenheit(lyon$TemperatureC)
lyon$DP_F <- celsius.to.fahrenheit(lyon$DewpointC)
lyon
#>         Date TemperatureC DewpointC  T_F DP_F
#> 1 2000-06-18           22        13 71.6 55.4
#> 2 2000-06-19           24        13 75.2 55.4
#> 3 2000-06-20           24         9 75.2 48.2
#> 4 2000-06-21           21        10 69.8 50.0
#> 5 2000-06-22           23        13 73.4 55.4
#> 6 2000-06-23           20         8 68.0 46.4
#> 7 2000-06-24           16        11 60.8 51.8
```

As a second example, the `norfolk` dataset includes temperature (`norfolk$TemperatureF`) and dew point temperature (`norfolk$TemperatureF`) in degrees Fahrenheit in Norfolk, VA, for the week of March 12, 2006.

To calculate temperature and dew point temperature in degrees Celsius, use the `fahrenheit.to.celsius` function:


```r
data(norfolk)
norfolk$T_C <- fahrenheit.to.celsius(norfolk$TemperatureF)
norfolk$DP_C <- fahrenheit.to.celsius(norfolk$DewpointF)
norfolk
#>         Date TemperatureF DewpointF   T_C  DP_C
#> 1 2006-03-12           70        56 21.11 13.33
#> 2 2006-03-13           74        59 23.33 15.00
#> 3 2006-03-14           63        40 17.22  4.44
#> 4 2006-03-15           50        19 10.00 -7.22
#> 5 2006-03-16           47        32  8.33  0.00
#> 6 2006-03-17           45        39  7.22  3.89
#> 7 2006-03-18           44        16  6.67 -8.89
```

### Calculating relative humidity and dew point temperature

The `weathermetrics` package includes two functions for converting between air temperature, dew point temperature, and relative humidity:
`dewpoint.to.humidity` and `humidity.to.dewpoint`. 

For example, the `lyon` dataset includes daily values of both air temperature (`lyon\TemperatureC`) and dew point temperature (`lyon$DewpointC`) in Lyon, France, for the week of June 18, 2000.  Since this dataset includes both air temperature and dew point temperature, you can calculate relative humidity using the `dewpoint.to.humidity` function:


```r
data(lyon)
lyon$RH <- dewpoint.to.humidity(t = lyon$TemperatureC,
                                dp = lyon$DewpointC,
                                temperature.metric = "celsius")
lyon
#>         Date TemperatureC DewpointC       RH
#> 1 2000-06-18           22        13 56.78893
#> 2 2000-06-19           24        13 50.28890
#> 3 2000-06-20           24         9 38.56789
#> 4 2000-06-21           21        10 49.54916
#> 5 2000-06-22           23        13 53.43049
#> 6 2000-06-23           20         8 46.07966
#> 7 2000-06-24           16        11 72.40572
```

You can specify whether air temperature and dew point temperature inputs are in degrees Fahrenheit or Celsius using the `temperature.metric` option (possible values are `'fahrenheit'` and `'celsius'`). If input values for temperature and dew point temperature are in different metrics (i.e., one is in degrees Fahrenheit and the other in degrees Celsius), you must convert one of the inputs using either `celsius.to.fahrenheit` or `fahrenheit.to.celsius` before you can input the values to the `dewpoint.to.humidity` function.

As an example of calculating dew point temperature, the `newhaven` dataset gives daily values of air temperature in degrees Fahrenheit (`newhaven$TemperatureF`) and relative humidity in % (`newhaven$Relative.Humidity`) for New Haven, CT, for the week of October 19, 2008. Since this dataset includes values for both temperature and relative humidity, you can calculate dew point temperature using the `humidity.to.dewpoint` function:


```r
data(newhaven)
newhaven$DP <- humidity.to.dewpoint(t = newhaven$TemperatureF,
                                    rh = newhaven$Relative.Humidity,
                                    temperature.metric = "fahrenheit")
newhaven
#>         Date TemperatureF Relative.Humidity    DP
#> 1 2008-10-19           46                57 31.47
#> 2 2008-10-20           48                55 32.45
#> 3 2008-10-21           48                68 37.84
#> 4 2008-10-22           46                60 32.75
#> 5 2008-10-23           44                64 32.48
#> 6 2008-10-24           44                67 33.63
#> 7 2008-10-25           56                84 51.18
```

Relative humidity must be input as %, and you must specify the metric of air temperature using the `temperature.metric` option (possible values: `'fahrenheit'` or `'celsius'`). The dew point temperature will be calculated using the same metric as the air temperature input to the function. If you wish to get dew point temperature in a different metric than air temperature, you can use on of the functions that converts between Celsius and Fahrenheit. For example:


```r
data(newhaven)
newhaven$DP <- humidity.to.dewpoint(t = newhaven$TemperatureF,
                                    rh = newhaven$Relative.Humidity,
                                    temperature.metric = "fahrenheit")
newhaven$DP_C <- fahrenheit.to.celsius(newhaven$DP)
newhaven
#>         Date TemperatureF Relative.Humidity    DP  DP_C
#> 1 2008-10-19           46                57 31.47 -0.29
#> 2 2008-10-20           48                55 32.45  0.25
#> 3 2008-10-21           48                68 37.84  3.24
#> 4 2008-10-22           46                60 32.75  0.42
#> 5 2008-10-23           44                64 32.48  0.27
#> 6 2008-10-24           44                67 33.63  0.91
#> 7 2008-10-25           56                84 51.18 10.66
```

Calculations between air temperature, relative humidity, and dew point temperature are based on algorithms used by the United States National Weather Service's [online heat index calculator](http://www.wpc.ncep.noaa.gov/html/heatindex.shtml) (accessed December 18, 2015). These are approximations rather than exact conversions.

### Calculating heat index

The `weathermetrics` package includes a function, `heat.index`, that allows you to calculate a vector of heat index values from vectors of air temperature and either dew point temperature or relative humidity. For example, the `suffolk` dataset gives daily values of air temperature in degrees Fahrenheit (`suffolk$TemperatureF`) and relative humidity in % (`suffolk$Relative.Humidity`) for Suffolk, VA, for the week of July 12, 1998. To calculate daily heat index values for this dataset, use the `heat.index` function:


```r
data(suffolk)
suffolk$HI <- heat.index(t = suffolk$TemperatureF,
                         rh = suffolk$Relative.Humidity,
                         temperature.metric = "fahrenheit",
                         output.metric = "fahrenheit")
suffolk
#>         Date TemperatureF Relative.Humidity HI
#> 1 1998-07-12           72                69 72
#> 2 1998-07-13           73                66 73
#> 3 1998-07-14           74                74 75
#> 4 1998-07-15           78                86 80
#> 5 1998-07-16           78               100 81
#> 6 1998-07-17           80                98 89
#> 7 1998-07-18           81                78 86
```

You must specify whether the air temperature input to the function is in degrees Celsius or Fahrenheit using the `temperature.metric` option (possible values: `'fahrenheit'` or `'celsius'`). You can choose which metric for heat index to be calculated in using using the `output.metric` option (the default is to give heat index in the same metric as the air temperature values input to the function).

As another example, the `lyon` dataset gives daily values of air temperature (`lyon$TemperatureC`) and dew point temperature (`lyon$DewpointC`), both in degrees Celsius, for Lyon, France, for the week of June 18, 2000. You can use this data to calculate daily heat index values in degrees Fahrenheit using:


```r
data(lyon)
lyon$HI_F <- heat.index(t = lyon$TemperatureC,
                      dp = lyon$DewpointC,
                      temperature.metric = "celsius",
                      output.metric = "fahrenheit")
lyon
#>         Date TemperatureC DewpointC HI_F
#> 1 2000-06-18           22        13   71
#> 2 2000-06-19           24        13   75
#> 3 2000-06-20           24         9   74
#> 4 2000-06-21           21        10   69
#> 5 2000-06-22           23        13   73
#> 6 2000-06-23           20         8   67
#> 7 2000-06-24           16        11   60
```

When calculating heat index from air temperature and dew point temperature, both must be in the same metric (either degrees Fahrenheit or degrees Celsius) when they are input to the `heat.index` function. If this is not the case, you can use either `celsius.to.fahrenheit` or `fahrenheit.to.celsius` to convert one of the metrics before using `heat.index`.

The algorithm for the \texttt{heat.index} function is adapted for R
from the algorithms used by the United States National Weather Service's [online heat index calculator](http://www.wpc.ncep.noaa.gov/html/heatindex.shtml) (accessed December 18, 2015). Therefore, results will agree with results from the US National Weather Service online calculator. However, heat index is sometimes calculated using a simpler algorithm. Therefore, heat index values from the \texttt{heat.index} function will sometimes differ by one or two degrees compared to other heat index calculators or charts.

## Handling missing or impossible weather values

When any of the functions in this package encounter a missing value (`NA`) within any of the input vectors, the output weather metric for that observation will also be set as `NA`. For example:


```r
df <- data.frame(T = c(NA, 90, 85),
                 DP = c(80, NA, 70))
df$RH <- dewpoint.to.humidity(t = df$T, dp = df$DP,
                              temperature.metric = "fahrenheit")
df
#>    T DP       RH
#> 1 NA 80       NA
#> 2 90 NA       NA
#> 3 85 70 60.88112
```

Certain values of dew point temperature or relative humidity are impossible. Relative humidity cannot be lower than 0% or higher than 100%. Dew point temperature cannot be higher than air temperature (except in the case of supersaturation) . When any of these functions encounter an impossible weather metric in an input vector, it returns `NA` as the output weather metric for that observation. For example:


```r
df <- data.frame(T = c(90, 90, 85),
                 DP = c(80, 95, 70))
df$heat.index <- heat.index(t = df$T, dp = df$DP,
                            temperature.metric = 'fahrenheit')
#> Warning in dewpoint.to.humidity(t = t, dp = dp, temperature.metric =
#> temperature.metric): For some observations, dew point temperature was
#> higher than temperature. Since dew point temperature cannot be higher than
#> air temperature, relative humidty for these observations was set to 'NA'.
df
#>    T DP heat.index
#> 1 90 80        108
#> 2 90 95         NA
#> 3 85 70         90
```

Additionally, the function returns a warning to alert the user that the input data includes impossible values for some observations. 

## Rounding output values

All functions have defaults for rounding that are consistent with the algorithms used by the United States National Weather Service's [online heat index calculator](http://www.wpc.ncep.noaa.gov/html/heatindex.shtml) (accessed December 18, 2015). For several of the functions, you may also specify that outputs are rounded to a different number of digits using the `round` option. For example:


```r
data(suffolk)
suffolk$TempC <- fahrenheit.to.celsius(suffolk$TemperatureF,
                                       round = 5)
suffolk$HI <- heat.index(t = suffolk$TemperatureF, 
                         rh = suffolk$Relative.Humidity,
                         round = 3)
suffolk
#>         Date TemperatureF Relative.Humidity    TempC     HI
#> 1 1998-07-12           72                69 22.22222 72.143
#> 2 1998-07-13           73                66 22.77778 73.102
#> 3 1998-07-14           74                74 23.33333 74.578
#> 4 1998-07-15           78                86 25.55556 80.426
#> 5 1998-07-16           78               100 25.55556 80.605
#> 6 1998-07-17           80                98 26.66667 88.688
#> 7 1998-07-18           81                78 27.22222 86.051
```

## Citation for package

To cite this package, use: 

Anderson GB, Bell ML, Peng RD. 2013.  Methods to calculate the heat
  index as an exposure metric in environmental health research.
  Environmental Health Perspectives 121(10):1111-1119.
