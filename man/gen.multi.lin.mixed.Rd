% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen.multi.lin.mixed.R
\name{gen.multi.lin.mixed}
\alias{gen.multi.lin.mixed}
\title{generates data from a random intercept linear mixed models 
with only intercepts: \eqn{y_{rij} = \beta_r + b_{ri} + \epsilon_{rij}}}
\usage{
gen.multi.lin.mixed(sigma2, D, mu, num.clust, clust.size, num.var,
  seed = 123)
}
\arguments{
\item{sigma2}{error variance}

\item{D}{random intercepts covariance matrix}

\item{mu}{fixed effects values, the same length as num.var}

\item{num.clust}{sample size}

\item{clust.size}{cluster size}

\item{num.var}{number of response variables}

\item{seed}{a seed}
}
\value{
prepared dataset
}
\description{
generates data from a random intercept linear mixed models 
with only intercepts: \eqn{y_{rij} = \beta_r + b_{ri} + \epsilon_{rij}}
}
\author{
Vahid Nassiri
}
