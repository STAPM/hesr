% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/icdFlag.R
\name{icdFlag}
\alias{icdFlag}
\title{Scan episodes for matching International Classification of Disease codes}
\usage{
icdFlag(data, lkups, col_name)
}
\arguments{
\item{data}{Data table - the episode-level hospital episode statistics data.}

\item{lkups}{Data table - the disease diagnosis codes associated with tobacco and/or alcohol
related conditions.}

\item{col_name}{Character string - The name of the diagnosis column that 
should be scanned to identify target disease diagnosis codes.}
}
\value{
Returns the input episode-level hospital episode statistics data with a new column added 
containing the name of the tobacco and/or alcohol related condition found during the scan.
}
\description{
Scans specified diagnosis code columns for 
disease diagnosis codes that match the provided list of tobacco and/or alcohol related diseases.
}
\details{
The scans the first 3 or 4 characters of the column name
for the target disease diagnosis code. If a match is found, then it is stored in a new variable.  

This function is called by [hesr::findEpisodes()].
}
\examples{
\dontrun{


}

}
