\name{dewpoint.to.humidity}
\alias{dewpoint.to.humidity}
\title{Calculate relative humidity}
\description{
Create a vector of relative humidity values from vectors of air temperature and dew point temperature values.
}
\usage{
dewpoint.to.humidity(dp = NA, t = NA,
temperature.metric = "fahrenheit")
}
\arguments{
  \item{dp}{ vector of dew point temperatures }
  \item{t}{ vector of air temperatures }
  \item{temperature.metric}{ temperature metric for both air temperature and dew point temperature, possible values: 'fahrenheit' or 'celsius' }
}
\details{
Dew point temperature and temperature must be in the same metric (i.e., either both in Celsius or both in Fahrenheit). If
necessary, use 'fahrenheit.to.celsius' or 'celsius.to.fahrenheit' to convert before using this function.
}
\value{
A vector of relative humidity (in \%)
}
\author{Brooke Anderson and Roger Peng}
\note{
Equations are from the source code for the US National Weather Service's online heat index calculator (http://www.hpc.ncep.noaa.gov/html/heatindex.shtml).
}
\references{ 
National Weather Service Hydrometeorological Prediction Center Web Team. Heat Index Calculator. 7 Nov 2011. http://www.hpc.ncep.noaa.gov/html/heatindex.shtml Accessed 7 Aug 2012
 }
\seealso{
\code{\link{fahrenheit.to.celsius}, 
\link{celsius.to.fahrenheit},
\link{humidity.to.dewpoint}}
}
\examples{
## Calculate relative humidity from air temperature and 
## dew point temperature
data(lyon)
lyon$RH <- dewpoint.to.humidity(t = lyon$TemperatureC,
		dp = lyon$DewpointC,
		temperature.metric = "celsius")
lyon
}

