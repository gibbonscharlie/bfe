\name{EstimateIWE}
\alias{EstimateIWE}
\alias{EstimateRWE}
\alias{print.iwe}
\alias{print.rwe}
\title{Estimate the sample-weighted effect}
\description{These functions estimate the sample-weighted effects using the interaction-weighted and regression-weighted estimators (IWE and RWE respectively) as described in Gibbons, Suarez Serrato, and Urbancic (2014).}
\usage{
EstimateIWE(y, treatment, group, controls, fe.other = NULL, data, 
    subset = NULL, cluster.var = NULL, is.robust = TRUE, 
    is.data.returned = FALSE)

EstimateRWE(y, treatment, group, controls, fe.other = NULL, data, 
    subset = NULL, cluster.var = NULL, is.robust = TRUE, 
    is.data.returned = FALSE)

\method{print}{iwe}(x, digits = 4, ...)
\method{print}{rwe}(x, digits = 4, ...)
}
\arguments{
  \item{y}{Character vector of length 1 giving the name of the outcome variable}
  \item{treatment}{Character vector of length 1 giving the name of the treatment variable}
  \item{group}{Character vector of length 1 giving the name of the fixed effect group with heterogeneous treatment effects; must be a factor variable}
  \item{controls}{Character vector giving names of control variables. Can be any value acceptable between plus signs in a formula (such as interactions or functions of variables)}
  \item{fe.other}{Character vector giving names of additional fixed effects variables. These variables can also be part of the vector passed to the \code{controls} argument, but this argument allows future integratiion with the \code{lfe} package}
  \item{data}{Data frame of the data}
  \item{subset}{A subsetting condition}
  \item{cluster.var}{Character vector of length 1 giving tne name of the variable used for clustering standard errors}
  \item{is.robust}{Logical; should robust standard errors be used. Currently must be TRUE}
  \item{is.data.returned}{Logical; should the data set be returned in the model object.}
  \item{x}{IWE or RWE model}
  \item{digits}{Number of digits to display in printed tables}
  \item{...}{Not currently used}
}
\value{The object returned is a list of class 'iwe' or 'rwe' as appropriate.
  \item{y}{Character vector of length 1 giving the name of the outcome variable}
  \item{treatment}{Character vector of length 1 giving the name of the treatment variable}
  \item{group}{Character vector of length 1 giving the name of the fixed effect group with heterogeneous treatment effects; must be a factor variable}
  \item{controls}{Character vector giving names of control variables. Can be any value acceptable between plus signs in a formula (such as interactions or functions of variables)}
  \item{fe.other}{Character vector giving names of additional fixed effects variables. These variables can also be part of the vector passed to the 'controls' argument, but this argument allows future integratiion with the 'lfe' package}
  \item{subset}{A subsetting condition}
  \item{cluster.var}{Character vector of length 1 giving tne name of the variable used for clustering standard errors}
  \item{is.robust}{Logical; should robust standard errors be used. Currently must be TRUE}
  \item{N}{Number of observations}
  \item{M}{Number of fixed effects groups}
  \item{formula.fe}{Formula for estimating the base (uninteracted) model}
  \item{formula.int}{Formula for estimating the interacted model}
  \item{f.weights}{Proportion of the (possibly subset) data belonging to each fixed effect group (for IWE estimate only)}
  \item{fe.est}{OLS estimate (for RWE estimate only)}
  \item{fe.var}{Variance of OLS estimate (for RWE estimate only)}
  \item{swe.est}{RWE estimate}
  \item{swe.var}{Variance of RWE estimate}
  \item{reg.fe}{For the IWE estimate: OLS (uninteracted) regression; for the RWE estimate: regression of annihilated y on annihilated treatment}
  \item{reg.int}{Interacted regression (for IWE estimate only)}
  \item{reg.w}{Weighted regression of annihilated y on annihilated treatment (for RWE estimate only)}
  \item{data}{Data (possibly subset) used in estimation (if requested)}
}
\references{
Gibbons, Charles E., Juan Carlos Suarez Serrato, and Michael B. Urbancic. 2014. "Broken or Fixed Effects?"}
\examples{
data(CensusData, package = "GSSU")

wage.iwe <- EstimateIWE("lnww", "educ", "age_dum", c("state", "year", "birthpl"),
  NULL, CensusData, NULL, "st.year")

wage.rwe <- EstimateRWE("lnww", "educ", "age_dum", c("state", "year", "birthpl"),
  NULL, CensusData, NULL, "st.year")
}

\keyword{regression}
