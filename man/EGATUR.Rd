\name{EGATUR}
\alias{EGATUR}
\docType{data}
\title{
EGATUR dataset
}
\description{Tourist Expenditure Survey (EGATUR) is the response by the Spanish Tourist Authorities to the growing need for information by this sector.}
\usage{data("EGATUR")}
\format{
  A data frame with 30541 observations on the following 13 variables.
  \describe{
    \item{\code{mm_aaaa}}{a numeric vector}
    \item{\code{pais}}{a factor with country names }
    \item{\code{A13}}{a numeric vector}
    \item{\code{aloja}}{a factor with levels \code{Hotels} \code{Rest of market} \code{Over-The-Counter Accommodation}}
    \item{\code{motivo}}{a factor with levels \code{Leisure} \code{Business} \code{Others}}
    \item{\code{gastototal}}{a numeric vector}
    \item{\code{factoregatur}}{a numeric vector}
    \item{\code{GastoTotalD}}{a factor with levels \code{[17,1.67e+03)} \code{[1.67e+03,5.51e+03)} \code{[5.51e+03,1.99e+04]}}
    \item{\code{A13_D}}{a factor with levels \code{[1,3)} \code{[3,4)} \code{[4,6)} \code{[6,7)} \code{[7,14)} \code{[14,180]}}
  }
}
\details{
Tourist Expenditure Survey (EGATUR) is the response by the Spanish Tourist Authorities to the growing need for information by this sector, which is one of the major driving forces of the Spanish economy.
The information provided by this survey makes it possible to ascertain with a greater degree of precision the volume of tourist expenditure by foreign visitors coming to Spain each month by different concepts, and to also analyse key aspects of their tourist behaviour.
EGATUR makes it possible to improve strategic knowledge of variables regarding fundamental expenditure and tourist behaviour by visitors from other countries, and to a large extent compensate the loss of information which, in order to estimate the income and payment entries for tourism in the Balance of Payments, was being used by the Bank of Spain prior to the introduction of the Euro. Likewise it provides highly relevant information in terms of National Accounts estimates and, in particular, in estimating the main groups of the recent Tourism Satellite Account for Spain.
}

\references{
‘EGATUR’,  available  at  http://www.ine.es/dyngs/inebase/es/operacion.htm?c=estadistica_c&cid=1254736177002&menu=ultidatos&idp=1254735576863
}
\examples{
data(EGATUR)
}
\keyword{datasets}
