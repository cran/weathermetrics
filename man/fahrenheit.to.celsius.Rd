\name{fahrenheit.to.celsius}
\alias{fahrenheit.to.celsius}
\title{Convert from Fahrenheit to Celsius}
\description{Create a vector of temperatures in Celsius from a vector of temperature in Fahrenheit}
\usage{
fahrenheit.to.celsius(T.fahrenheit, round = 2)
}
\arguments{
  \item{T.fahrenheit}{ a vector of temperature values in Fahrenheit }
  \item{round}{ number of digits to round the converted value }
}
\value{
A vector of temperature values in Celsius
}
\author{
Brooke Anderson and Roger Peng
}
\note{
Equations are from the source code for the US National Weather Service's online heat index calculator (http://www.hpc.ncep.noaa.gov/html/heatindex.shtml).
}
\seealso{
\code{\link{celsius.to.fahrenheit}}
}
\examples{
## Convert from Fahrenheit to Celsius
data(norfolk)
norfolk$TempC <- fahrenheit.to.celsius(norfolk$TemperatureF)
norfolk
}