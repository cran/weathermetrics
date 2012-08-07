\name{heat.index.algorithm}
\alias{heat.index.algorithm}
\title{Algorithm for heat.index function}
\description{
Convert a single value of temperature (in Fahrenheit) and a single value of relative humidity (in \%) to heat index (in Fahrenheit). This function is not meant to be used outside of the 'heat.index' function.
}
\usage{
heat.index.algorithm(t = NA, rh = NA)
}
\arguments{
  \item{t}{ a single value of temperature, in Fahrenheit }
  \item{rh}{ a single value of relative humidity, in \% }
}
\details{
If an impossible value of relative humidity is given (below 0\% or above 100\%), heat index is returned as 'NA'.
}
\value{
A single value of heat index, in Fahrenheit
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
