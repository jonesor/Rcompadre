#' Subsetting CompadreDB objects
#'
#' \code{CompadreDB} objects can be subset just like a regular
#' \code{data.frame}, using either \code{[} or \code{subset()}. Note, however,
#' that returning a valid \code{CompadreDB} object requires the \code{mat}
#' column. If a column subset excludes \code{mat}, a plain tabular object is
#' returned instead of a \code{CompadreDB}.
#'
#' @return No return value, called for side effects
#'
#' @name CompadreDB-Subsetting
#'
#' @examples
#' # subset to the first 10 rows
#' Compadre[1:10, ]
#'
#' # subset to the species 'Echinacea angustifolia'
#' subset(Compadre, SpeciesAccepted == "Echinacea angustifolia")
#'
#' # remove the column SurvivalIssue
#' Compadre[, names(Compadre) != "SurvivalIssue"]
#'
#' subset(Compadre, select = c("SpeciesAccepted", "Authors"))
NULL



#' @rdname CompadreDB-Subsetting
#' @param x A \code{CompadreDB} object
#' @param i row indices (see \link{[.data.frame})
#' @param j column indices (see \link{[.data.frame})
#' @param ... ignored
#' @param drop ignored
#' @importFrom methods new
#' @export
setMethod(
  f = "[", signature = signature(
    x = "CompadreDB",
    i = "ANY",
    j = "ANY"
  ),
  function(x, i, j, ..., drop = FALSE) {
    dat <- x@data
    if (!missing(j)) {
      if (!any(is.logical(j), is.numeric(j), is.character(j))) {
        stop(
          "subset criteria must be logical, numeric (column / row",
          "numbers) or character (column / row names)"
        )
      }
    }

    out <- dat[i, j, drop = FALSE]

    if ("mat" %in% names(out)) {
      new("CompadreDB",
        data = out,
        version = x@version
      )
    } else {
      out
    }
  }
)


#' @rdname CompadreDB-Subsetting
#' @param subset logical expression indicating which rows to keep
#' @param select expression indicating which columns to keep
#' @export
subset.CompadreDB <- function(x, subset, select, drop = FALSE, ...) {
  dat <- x@data
  if (missing(subset)) {
    r <- rep_len(TRUE, nrow(dat))
  } else {
    e <- substitute(subset)
    r <- eval(e, dat, parent.frame())
    if (!is.logical(r)) stop("'subset' must be logical")
    r <- r & !is.na(r)
  }

  if (missing(select)) {
    vars <- TRUE
  } else {
    nl <- as.list(seq_along(dat))
    names(nl) <- names(dat)
    vars <- eval(substitute(select), nl, parent.frame())
  }
  x[r, vars, drop = drop]
}
