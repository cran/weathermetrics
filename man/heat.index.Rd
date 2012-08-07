\name{heat.index}
\alias{heat.index}
\title{Calculate heat index}
\description{
Create a vector of heat index values from vectors of air temperature and either relative humidity or dew point temperature values.
}
\usage{
heat.index(t = NA, dp = c(), rh = c(), 
temperature.metric = "fahrenheit", output.metric = NULL,
round = 0)
}
\arguments{
  \item{t}{ vector of air temperature }
  \item{dp}{ vector of dew point temperature }
  \item{rh}{ vector of relative humidity (in \%) }
  \item{temperature.metric}{ temperature metric for air temperature and, if you're using it, dew point temperature; value can be either 'fahrenheit' or 'celsius' }
  \item{output.metric}{ metric heat index should be calculated in, either 'fahrenheit' or 'celcius' }
  \item{round}{ the number of digits to round the heat index to }
}
\details{
Include air temperature ('t') and either dew point temperature ('dp') or relative humdity ('rh'). You cannot specify both dew point temperature and relative humidity- this will return an error. Heat index is calculated as 'NA' when impossible values of dew point temperature or humidity are inputted (e.g., humidity above 100\% or below 0\%, dew point temperature above air temperature).
}
\value{
A vector of heat index values in the metric specified by 'output.metric' (if 'output.metric' is not specified, heat index will be returned in the same metric in which air temperature was input, specified by 'temperature.metric')
}
\author{
Brooke Anderson and Roger Peng
}
\note{
Equations are from the source code for the US National Weather Service's online heat index calculator (http://www.hpc.ncep.noaa.gov/html/heatindex.shtml).
}
\references{ 
National Weather Service Hydrometeorological Prediction Center Web Team. Heat Index Calculator. 7 Nov 2011. http://www.hpc.ncep.noaa.gov/html/heatindex.shtml Accessed 7 Aug 2012

L. Rothfusz, 1990. The heat index (or, more than you ever wanted to know about heat index) (Technical Attachment SR 90-23). Fort Worth: Scientific Services Division, National Weather Service.
 
R. Steadman, 1979. The assessment of sultriness. Part I: A temperature-humidity index based on human physiology and clothing science. Journal of Applied Meteorology, 18(7):861--873.
 }
\examples{
## Calculate heat index from temperature (in Fahrenheit) and
## relative humidity
data(suffolk)
suffolk$heat.index <- heat.index(t = suffolk$TemperatureF,
			   rh = suffolk$Relative.Humidity)
suffolk

## Calculate heat index (in Celsius) from temperature (in 
## Celsius) and dew point temperature (in Celsius)
data(lyon)
lyon$heat.index <- heat.index(t = lyon$TemperatureC,
			dp = lyon$DewpointC,
			temperature.metric = "celsius",
			output.metric = "celsius")
lyon
}
