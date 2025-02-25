% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getAge.R
\name{getAge}
\alias{getAge}
\title{Get age information for each fish}
\usage{
getAge(Pdata, verbose = TRUE, keep, col.bestage = lifecycle::deprecated())
}
\arguments{
\item{Pdata}{A data frame returned from \code{\link[=PullBDS.PacFIN]{PullBDS.PacFIN()}} containing
biological samples. These data are stored in the Pacific Fishieries
Information Network (PacFIN) data warehouse, which originated in 2014 and
are pulled using sql calls.}

\item{verbose}{A logical specifying if output should be written to the
screen or not. Good for testing and exploring your data but can be turned
off when output indicates information that you already know. The printing
of output to the screen does not affect any of the returned objects. The
default is to always print to the screen, i.e., \code{verbose = TRUE}.}

\item{keep}{A vector of strings, integers, \code{NA}, and empty strings (e.g.,
\code{""}) representing the \code{AGE_METHOD}s you want to keep. All entries will be
converted to character values using \code{\link[=codify_age_method]{codify_age_method()}}. To keep the best
age for a given row, at least one of the ageing methods needs to have been
performed using one of the methods present in this argument. Often, it is
helpful to run \code{\link[=getAgeMethod]{getAgeMethod()}} on the relevant data prior to running this
function to see what age methods are currently present.}

\item{col.bestage}{Deprecated in version 0.2.5.}
}
\value{
A vector of integer ages in years the same length as the number of rows
present in \code{Pdata}.
}
\description{
Get age information for each fish, or row of the data. This functionality was
originally in \code{cleanAges()}, moved to \code{\link[=cleanPacFIN]{cleanPacFIN()}}, and is now
compartmentalized here, but called in \code{\link[=cleanPacFIN]{cleanPacFIN()}} and stored in the \code{Age}
column.
}
\details{
\subsection{\code{FISH_AGE_YEARS_FINAL}}{

\code{FISH_AGE_YEARS_FINAL} is defined in PacFIN as the ``age of specimen (best
age or final age)''. If \code{FISH_AGE_YEARS_FINAL} does not have an entry, where
this should be the best age available, then the user is warned that they
should potentially look at the age reads from an individual reader, e.g.,
`age1` or `age2`, until the state works out what age read is best and
provides this in `FISH_AGE_YEARS_FINAL`.

How \code{FISH_AGE_YEARS_FINAL} is determined can be unclear because it does not
always equal the value from one of the age readers. How the value is
determined is specific to the lab that conducted the ageing. Sometimes,
reconciling multiple age reads to determine the best age will lead to the
final age not equaling any of the individual age reads. For example, readers
can get together to determine the best read. We assume that those who
populated the column know what they are doing; but, please feel free to
report errors in PacFIN to GitHub issues or the package maintainer after
contacting the state representative.

Ageing methods listed in keep (or derivatives of those) will be used to
filter for ages that are of desired method. Any age that was not estimated
using one of the desired methods will be replaced with an \code{NA}.
\subsection{Deprecated methods}{

Previously, \code{getAge()} averaged over multiple reads if there was no entry in
\code{FINAL_FISH_AGE_IN_YEARS}. This functionality has since been removed from the
package starting with version 0.2.6.
}

\subsection{Washington}{

Washington uses the following order of operations to assign the best age
given there are multiple reads. See \href{http://www.github.com/pfmc-assessments/pacfintools/issues/49}{issue #49} for a
semi-detailed discussion. Note, that they will never report more than three
reads because that is the limitation of their database.
\itemize{
\item age1 if only one read;
\item randomly chosen age between age-read 1 and age-read 2 if the difference
between the two reads is less than or equal to three;
\item if the difference between two reads is more than three, then the best age
is manually keyed in from that designated by a human;
\item if three reads are equal, then the value is automatically assigned; and
\item if there are three reads and they are not equal then an age reader must
choose the final value.
}
}

\subsection{Oregon}{

Oregon employs a single age reader, or at least has since 2014. Twenty
percent of first reads are re-read by the same reader but without knowing any
information about the results of the first read.
\itemize{
\item age1 if only one read;
\item age1 if age1 and age2 agree;
\item age3 if age1 and age2 do not agree, where age3 is not a blind read and the
reader knows the values reported in age1 and age2 when reading the otolith
for a third time.
}
}

\subsection{California}{

Ageing of otoliths collected in California is performed by the Cooperative
Ageing Project, funded through a grant with the Pacific States Marine
Fisheries Commission. This same lab also ages the samples collected on the
coast-wide surveys as well as some of the samples collected by Oregon when
there are too many ages to read for their single age reader.
\itemize{
\item age1 if only one read;
\item age1 if age1 and age2 agree;
\item for younger fish, i.e., less than or equal to age 10, discrepancies can
normally be resolved by looking at the otolith and having knowledge of both
previous reads;
\item for older fish, i.e., older than age 10, discrepancies are resolved and the
resolved age is used;
\item for older fish where the discrepancy cannot be resolved, age1 is used
because it had the freshest otolith and potentially used the better half.
}
}

}
}
\author{
Kelli F. Johnson
}
