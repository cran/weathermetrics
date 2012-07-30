\name{celsius.to.fahrenheit}
\alias{celsius.to.fahrenheit}
\title{Convert from Celsius to Fahrenheit}
\description{Create a vector of temperatures in Fahrenheit from a vector of temperatures in Celsius}
\usage{
celsius.to.fahrenheit(T.celsius, round = 2)
}
\arguments{
  \item{T.celsius}{ a vector of temperature values in Celsius }
  \item{round}{ number of digits to round converted value }
}
\value{
A vector of temperature values in Fahrenheit
}
\author{
Brooke Anderson and Roger Peng
}
\note{
Equations are from the source code for the US National Weather Service's online heat index calculator (http://www.hpc.ncep.noaa.gov/html/heatindex.shtml).
}
\seealso{
\code{\link{fahrenheit.to.celsius}}
}
\examples{
## Convert from Celsius to Fahrenheit
data(lyon)
lyon$TemperatureF <- celsius.to.fahrenheit(lyon$TemperatureC)
lyon
}
