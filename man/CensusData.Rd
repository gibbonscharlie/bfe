\name{CensusData}
\alias{CensusData}
\docType{data}
\title{U.S. Census data}
\description{A 1\% random sample of men in the 1960, 1970, and 1980
  U.S. Censuses from the IPUMS database}
\usage{data(CensusData)}
\format{
  A data frame with 36229 observations on the following 14 variables.
  \describe{
    \item{\code{year}}{Census year; a factor with levels \code{1960} \code{1970} \code{1980}}
    \item{\code{state}}{State of residence; a factor with 51 levels}
    \item{\code{age}}{Age; a factor with 61 levels}
    \item{\code{birthqtr}}{Quarter of birth; a factor with levels \code{Jan-Feb-March} \code{April-May-June} \code{July-Aug-Sept} \code{Oct-Nov-Dec}}
    \item{\code{birthpl}}{State of birth FIPS code; a factor with 51 levels}
    \item{\code{yearat14}}{Year at age 14; a numeric vector}
    \item{\code{ca}}{Compulsory schooling grade; a numeric vector}
    \item{\code{cl}}{Child labor grade; a numeric vector}
    \item{\code{educ}}{Years of education; a numeric vector}
    \item{\code{black}}{Black indicator variable; a factor with levels \code{0} \code{1}}
    \item{\code{prison}}{Prison indicator variable; a numeric vector}
    \item{\code{age_dum}}{Age group; a factor with 21 levels}
    \item{\code{cohort_dum}}{Cohort; a factor with levels \code{0} \code{1} \code{2} \code{3} \code{4} \code{5} \code{6}}
    \item{\code{lnww}}{Log weekly wage; a numeric vector}
    \item{\code{st.year}}{State-year interaction; a factor with 153 levels}
  }
}
\source{
Steven Ruggles, J. Trent Alexander, Katie Genadek, Ronald Goeken, Matthew B. Schroeder, and Matthew Sobek. Integrated Public Use Microdata Series: Version 5.0 [Machine-readable database]. Minneapolis: University of Minnesota, 2010.
}
\references{See analysis in: 
Acemoglu, Daron and Joshua Angrist. 2000. "How Large Are Human Capital
Externalities Evidence? Evidence from Compulsory Schooling Laws" NBER
\emph{Macroeconomics Annual}. 15: 9-59.
}
\examples{
data(CensusData)
}
\keyword{datasets}
