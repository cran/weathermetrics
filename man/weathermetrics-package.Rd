\name{weathermetrics-package}
\alias{weathermetrics-package}
\alias{weathermetrics}
\docType{package}
\title{Functions to convert between weather metrics}
\description{
weathermetrics includes functions to calculate dew point temperature, relative humidity, and heat index and to convert between Celsius and Fahrenheit. All functions are based on equations and algorithms used by the United States National Weather Service's online heat index calculator.
}
\details{
\tabular{ll}{
Package: \tab weathermetrics\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2012-08-07\cr
License: \tab MIT\cr
}

This package includes the following functions to calculate vectors of weather metrics: 'celsius.to.fahrenheit', 'fahrenheit.to.celsius', 'dewpoint.to.humidity', 'humidity.to.dewpoint', and 'heat.index'
}
\author{
Brooke Anderson and Roger Peng

Maintainer: Brooke Anderson <geanders@jhsph.edu>
}
\keyword{ package }
\references{ 
National Weather Service Hydrometeorological Prediction Center Web Team. Heat Index Calculator. 7 Nov 2011. http://www.hpc.ncep.noaa.gov/html/heatindex.shtml Accessed 7 Aug 2012

L. Rothfusz, 1990. The heat index (or, more than you ever wanted to know about heat index) (Technical Attachment SR 90-23). Fort Worth: Scientific Services Division, National Weather Service.
 
R. Steadman, 1979. The assessment of sultriness. Part I: A temperature-humidity index based on human physiology and clothing science. Journal of Applied Meteorology, 18(7):861--873.
 }
