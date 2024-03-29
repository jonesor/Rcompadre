% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cdb_id_studies.R
\name{cdb_id_studies}
\alias{cdb_id_studies}
\title{Create a vector of unique study identifiers from a COM(P)ADRE database}
\usage{
cdb_id_studies(
  cdb,
  columns = c("Authors", "Journal", "YearPublication", "DOI_ISBN")
)
}
\arguments{
\item{cdb}{A CompadreDB object}

\item{columns}{Vector of column names from which unique combinations should
be identified. Defaults to \code{c("Authors", "Journal", "YearPublication",
"DOI_ISBN")}.}
}
\value{
Vector of integer study identifiers corresponding to the rows of
  \code{cdb}, based on unique combinations of the elements in \code{columns}.
}
\description{
Creates a vector of integer study identifiers corresponding to the rows of a
CompadreDB object, based on unique combinations of the columns 'Authors',
'Journal', 'YearPublication', and 'DOI_ISBN' (or optionally, a different set
of columns supplied by the user).
}
\details{
Identifiers are assigned by pasting together the relevant columns, assigning
factor levels based on alphabetical order, and then converting the factor
levels to integers.
}
\examples{
Compadre$StudyID <- cdb_id_studies(Compadre)

}
\seealso{
\link{cdb_id}

Other data management: 
\code{\link{cdb_flatten}()},
\code{\link{cdb_id_stages}()},
\code{\link{cdb_id}()},
\code{\link{cdb_mean_matF}()},
\code{\link{cdb_rbind}()},
\code{\link{cdb_unflatten}()},
\code{\link{cdb_unnest}()},
\code{\link{mpm_elementwise_apply}()},
\code{\link{mpm_mean}()},
\code{\link{mpm_median}()},
\code{\link{mpm_sd}()},
\code{\link{string_representation}}
}
\author{
Patrick Barks <patrick.barks@gmail.com>
}
\concept{data management}
