% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getWLpars.R
\name{getWLpars}
\alias{getWLpars}
\title{Calculate weight--length relationship parameters}
\usage{
getWLpars(
  data,
  col.length = "lengthcm",
  col.weight = "weightkg",
  verbose = TRUE
)
}
\arguments{
\item{data}{A data frame containing empirical weights and lengths
from sampled fish.
Sexes should be available in the column \code{sex} or \code{SEX}.}

\item{col.length}{A numeric or character value specifying the column
to use in \code{data} for length information. These lengths are assumed to
be in centimeters. The default value is \code{lengthcm}, which is added
to a data set automatically when running \link{cleanPacFIN}.}

\item{col.weight}{A numeric or character value specifying the column
to use in \code{data} for weight information. These weights are assumed to
be in kilograms The default value is \code{weightkg}, which is added
to a data set automatically when running \link{cleanPacFIN}.
Using kilograms is the default because Stock Synthesis assumes the
weight--length parameters are calculated using centimeters and kilograms.
The reported values are easily scaled to give you results in grams if
you wish to have more standard parameter estimates.}

\item{verbose}{A logical specifying if output should be written to the
screen or not. Good for testing and exploring your data but can be turned
off when output indicates information that you already know. The printing
of output to the screen does not affect any of the returned objects. The
default is to always print to the screen, i.e., \code{verbose = TRUE}.}
}
\value{
A data frame of weight--length parameters by sex.
Parameters A and B are in the appropriate units to input
into Stock Synthesis Wtlen_1_Fem and Wtlen_2_Fem, or
Wtlen_1_Mal and Wtlen_1_Mal, parameters in the
control file. Values of \code{NA} are returned for models that
did not have enough data to properly estimate the parameters.
This will happen when there are no females in your data set,
for example.
}
\description{
Estimate parameters of the weight--length relationship for each
sex and all sexes combined, where the latter includes unsexed fish.
}
\author{
Kelli F. Johnson
}
