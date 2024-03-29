% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CompadreDB.R
\docType{class}
\name{CompadreDB}
\alias{CompadreDB}
\alias{CompadreData}
\alias{CompadreData,CompadreDB-method}
\alias{$,CompadreDB-method}
\alias{$<-,CompadreDB-method}
\alias{[[,CompadreDB,ANY,missing-method}
\alias{[[<-,CompadreDB,ANY,missing-method}
\alias{VersionData}
\alias{VersionData,CompadreDB-method}
\alias{Version}
\alias{Version,CompadreDB-method}
\alias{DateCreated}
\alias{DateCreated,CompadreDB-method}
\title{CompadreDB Class}
\usage{
CompadreData(object)

\S4method{CompadreData}{CompadreDB}(object)

\S4method{$}{CompadreDB}(x, name)

\S4method{$}{CompadreDB}(x, name) <- value

\S4method{[[}{CompadreDB,ANY,missing}(x, i, j, ...)

\S4method{[[}{CompadreDB,ANY,missing}(x, i, j) <- value

VersionData(object)

\S4method{VersionData}{CompadreDB}(object)

Version(object)

\S4method{Version}{CompadreDB}(object)

DateCreated(object)

\S4method{DateCreated}{CompadreDB}(object)
}
\arguments{
\item{object}{A CompadreDB object}

\item{x}{A CompadreDB object}

\item{name}{The name of a column within x}

\item{value}{Vector of values to assign to the column}

\item{i, j}{elements to extract or replace (see \link{[[.data.frame})}

\item{...}{ignored}
}
\description{
This page describes the CompadreDB class, including methods for accessing the
slots (see functions \code{CompadreData} and \code{VersionData}), accessing
(\code{$}) and replacing (\code{$<-}) columns within the \code{data} slot,
accessing elements from the \code{version} slot (see functions
\code{VersionData} and \code{DateCreated}), and converting legacy database
objects to the CompadreDB class (see \code{as_cdb}).
}
\section{Slots}{

\describe{
\item{\code{data}}{A tibble-style data frame with a list-column of matrix population
models (column \code{mat}) and a variety of other metadata columns.}

\item{\code{version}}{A list with elements \code{Version} (database version number),
\code{DateCreated} (date of version release), and \code{Agreement} (a url
link to the User Agreement)}
}}

\examples{
# extract entire 'data' slot
dat <- CompadreData(Compadre)

# access the date of database creation
DateCreated(Compadre)

# extract column SpeciesAccepted
Compadre$SpeciesAccepted

# create new list-column with stage-specific survival
Compadre$stage_survival <- lapply(Compadre$mat, function(x) colSums(x@matU))
}
\seealso{
\code{\link{CompadreDB-Methods}} \code{\link{CompadreDB-Subsetting}}
}
\author{
Iain M. Stott

Tamora D. James
}
