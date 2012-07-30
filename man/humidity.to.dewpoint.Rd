\name{humidity.to.dewpoint}
\alias{humidity.to.dewpoint}
\title{ Calculate dew point temperature }
\description{
Create a vector of dew point temperatures from vectors of air temperature and relative humidity.
}
\usage{
humidity.to.dewpoint(t = NA, rh = NA, 
temperature.metric = "fahrenheit")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{t}{ vector of air temperatures }
  \item{rh}{ vector of relative humidity (in \%) }
  \item{temperature.metric}{ temperature metric for air temperature, either 'fahrenheit' or 'celsius' }
}
\details{
Dew point temperature will be calculated in the same metric as the temperature vector (as specified by the 'temperature.metric' option). If you'd like dew point temperature in a different metric, use the function 'celsius.to.fahrenheit' or 'fahrenheit.to.celsius' on the output from this function.
}
\value{
A vector of dew point temperature, in the same metric as the temperature vector (as specified by the 'temperature.metric' option)
}
\author{
Brooke Anderson and Roger Peng
}
\note{
Equations are from the source code for the US National Weather Service's online heat index calculator (http://www.hpc.ncep.noaa.gov/html/heatindex.shtml).
}
\references{ 
National Weather Service Hydrometeorological Prediction Center Web Team. Heat Index Calculator. 7 Nov 2011. http://www.hpc.ncep.noaa.gov/html/heatindex.shtml Accessed 7 Aug 2012
 }
\seealso{
\code{\link{fahrenheit.to.celsius}, 
\link{celsius.to.fahrenheit},
\link{dewpoint.to.humidity}}
}
\examples{
## Calculate dew point temperature from relative humidity and
## air temperature
data(newhaven)
newhaven$DP <- humidity.to.dewpoint(t = newhaven$TemperatureF,
			  rh = newhaven$Relative.Humidity,
			  temperature.metric = "fahrenheit")
newhaven
}

