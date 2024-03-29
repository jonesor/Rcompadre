% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cdb_rbind.R
\name{cdb_rbind}
\alias{cdb_rbind}
\title{Merge two COM(P)ADRE databases via row-bind}
\usage{
cdb_rbind(cdb1, cdb2)
}
\arguments{
\item{cdb1, cdb2}{CompadreDB objects}
}
\value{
A CompadreDB object created by binding the rows of \code{cdb1} and
  \code{cdb2}
}
\description{
Merges two CompadreDB objects via a row-bind of the data slots.
}
\examples{
Compadre1 <- subset(Compadre, Continent == "Asia")
Compadre2 <- subset(Compadre, Continent == "Africa")

cdb_rbind(Compadre1, Compadre2)

}
\seealso{
Other data management: 
\code{\link{cdb_flatten}()},
\code{\link{cdb_id_stages}()},
\code{\link{cdb_id_studies}()},
\code{\link{cdb_id}()},
\code{\link{cdb_mean_matF}()},
\code{\link{cdb_unflatten}()},
\code{\link{cdb_unnest}()},
\code{\link{mpm_elementwise_apply}()},
\code{\link{mpm_mean}()},
\code{\link{mpm_median}()},
\code{\link{mpm_sd}()},
\code{\link{string_representation}}
}
\author{
Sam Levin <levisc8@gmail.com>

Owen R. Jones <jones@biology.sdu.dk>
}
\concept{data management}
