\name{SpecTest}
\alias{SpecTest}
\alias{SpecTest.iwe}
\alias{SpecTest.rwe}
\title{Test for a difference between the OLS and sample-weighted estimates}
\description{Perform a specification test of the difference between the OLS and sample-weighted effect estimates as described in Gibbons et al. (2018)}
\usage{
SpecTest(model)
}
\arguments{
  \item{model}{SWE estimates of class 'iwe' or 'rwe'}
}
\value{Object of class 'chisq.test'
  \item{stat}{Chi-squared test statistic}
  \item{df}{Degrees-of-freedom for the Chi-squared test}
}
\references{Gibbons, Charles E., Juan Carlos Suarez Serrato, and Michael
	B. Urbancic. 2018. "Broken or Fixed Effects?" \emph{Journal of
		Econometric Methods}.}
\seealso{\code{\link{print.chisq.test}}}
\examples{
data(CensusData, package = "GSSU")

wage.iwe <- EstimateIWE("lnww", "educ", "age_dum", c("state", "year", "birthpl"),
  NULL, CensusData, NULL, "st.year")

wage.rwe <- EstimateRWE("lnww", "educ", "age_dum", c("state", "year", "birthpl"),
  NULL, CensusData, NULL, "st.year")

wage.iwe.spec <- SpecTest(wage.iwe, CensusData)
wage.rwe.spec <- SpecTest(wage.rwe, CensusData)
}
\keyword{regression}
