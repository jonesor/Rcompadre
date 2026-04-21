#' Methods for CompadreDB objects
#'
#' This page describes a variety of methods that can be used with CompadreDB
#' objects, including common data frame operations (\code{head}, \code{names},
#' \code{colnames}, \code{print}, \code{cbind}, \code{rbind}, and \code{merge}),
#' conversion methods (\code{as.data.frame} and \code{as_tibble}), and methods
#' to calculate the number of species
#' (\code{NumberAcceptedSpecies}), studies (\code{NumberStudies}), or matrices
#' (\code{NumberMatrices}).
#'
#' @param x,object A CompadreDB object
#' @param y A data.frame to merge with x
#' @param n The number of rows to extract
#' @param deparse.level passed to [base::cbind()] or [base::rbind()]
#' @param ... additional arguments
#'
#' @return No return value, called for side effects
#'
#' @name CompadreDB-Methods
NULL


.print_CompadreDB <- function(x, ...) {
  Mno <- NumberMatrices(x)
  Sno <- ifelse("SpeciesAccepted" %in% names(x@data),
    NumberAcceptedSpecies(x),
    "??"
  )

  cat(paste0(
    "A COM(P)ADRE database ('CompadreDB') object with ",
    as.character(Sno),
    " SPECIES and ",
    as.character(Mno),
    " MATRICES.\n\n"
  ))
  print(x@data, ...)
}



#' @rdname CompadreDB-Methods
#' @export
as.data.frame.CompadreDB <- function(x, ...) {
  dat <- x@data
  as.data.frame(dat, ...)
}

setAs("CompadreDB", "data.frame", function(from) {
  as.data.frame.CompadreDB(from)
})


#' @rdname CompadreDB-Methods
#' @importFrom tibble as_tibble
#' @param .rows passed to [tibble::as_tibble()]
#' @param .name_repair passed to [tibble::as_tibble()]
#' @param rownames passed to [tibble::as_tibble()]
#' @export
as_tibble.CompadreDB <- function(x,
                                 .rows = NULL,
                                 .name_repair = c(
                                   "check_unique", "unique",
                                   "universal", "minimal"
                                 ),
                                 rownames = NULL, ...) {
  as_tibble(
    x@data,
    .rows = .rows,
    .name_repair = .name_repair,
    rownames = rownames,
    ...
  )
}


#' @rdname CompadreDB-Methods
#' @importFrom utils head
#' @importFrom methods new
#' @export
head.CompadreDB <- function(x, n = 6L, ...) {
  dat <- head(x@data, n = n, ...)

  new("CompadreDB",
    data = dat,
    version = x@version
  )
}


#' @rdname CompadreDB-Methods
#' @importFrom utils tail
#' @importFrom methods new
#' @export
tail.CompadreDB <- function(x, n = 6L, ...) {
  dat <- tail(x@data, n = n, ...)

  new("CompadreDB",
    data = dat,
    version = x@version
  )
}


#' @rdname CompadreDB-Methods
#' @export
names.CompadreDB <- function(x) {
  names(x@data)
}


#' @rdname CompadreDB-Methods
#' @export
dim.CompadreDB <- function(x) {
  dim(x@data)
}


#' @rdname CompadreDB-Methods
#' @param do.NULL,prefix passed to [base::colnames()]
#' @export
setMethod("colnames", "CompadreDB", function(x, do.NULL = TRUE, prefix = "col") {
  colnames(x@data, do.NULL = do.NULL, prefix = prefix)
})


#' @rdname CompadreDB-Methods
#' @export
print.CompadreDB <- function(x, ...) {
  .print_CompadreDB(x, ...)
  invisible(x)
}


#' @rdname CompadreDB-Methods
#' @param deparse.level passed to [base::cbind()] or [base::rbind()]
#' @param fill passed to [cdb_rbind()]; if `TRUE`, missing columns are filled
#'   with `NA`
#' @export
rbind.CompadreDB <- function(..., deparse.level = 1, fill = FALSE) {
  cdb_rbind(..., fill = fill)
}


#' @rdname CompadreDB-Methods
#' @importFrom tibble as_tibble
#' @importFrom methods new
#' @export
cbind.CompadreDB <- function(..., deparse.level = 1) {
  args <- list(...)

  if (length(args) == 0) {
    stop("Please provide at least one CompadreDB object.")
  }

  x <- args[[1]]
  if (!inherits(x, "CompadreDB")) {
    stop("First argument must be a CompadreDB object.")
  }

  extras <- args[-1]

  if (any(vapply(extras, inherits, logical(1), what = "CompadreDB"))) {
    stop(
      "Column-binding multiple CompadreDB objects is not supported. ",
      "Use rbind()/cdb_rbind() for row-binding, or extract the data slot ",
      "explicitly if you want to bind metadata columns."
    )
  }

  n <- nrow(x@data)
  for (arg in extras) {
    if (is.data.frame(arg)) {
      if (nrow(arg) != n) {
        stop("All data.frame arguments must have the same number of rows as x.")
      }
    } else if (length(arg) != n) {
      stop("All non-data.frame arguments must have length equal to nrow(x).")
    }
  }

  dataout <- as_tibble(do.call(base::cbind, c(list(x@data), extras)))

  new("CompadreDB",
    data = dataout,
    version = x@version
  )
}


#' @rdname CompadreDB-Methods
#' @importFrom methods new
#' @export
droplevels.CompadreDB <- function(x, ...) {
  new("CompadreDB",
    data = droplevels(x@data, ...),
    version = x@version
  )
}


#' @rdname CompadreDB-Methods
#' @importFrom tibble as_tibble
#' @importFrom methods new
#' @export
merge.CompadreDB <- function(x, y, ...) {
  if (inherits(y, "CompadreDB")) {
    y <- y@data
  }
  dataout <- as_tibble(merge(x@data, y, ...))

  new("CompadreDB",
    data = dataout,
    version = x@version
  )
}


#' The number of accepted binary species names in a CompadreDB object
#' @rdname CompadreDB-Methods
#' @export
setGeneric(
  "NumberAcceptedSpecies",
  function(object) {
    standardGeneric("NumberAcceptedSpecies")
  }
)

#' @rdname CompadreDB-Methods
#' @export
setMethod("NumberAcceptedSpecies",
  signature = "CompadreDB",
  function(object) {
    if (!"SpeciesAccepted" %in% names(object@data)) {
      stop(
        "Cannot count number of species because column ",
        "'SpeciesAccepted' is missing"
      )
    } else {
      return(length(unique(object@data$SpeciesAccepted)))
    }
  }
)


#' The number of different studies in a CompadreDB object
#' @rdname CompadreDB-Methods
#' @export
setGeneric(
  "NumberStudies",
  function(object) {
    standardGeneric("NumberStudies")
  }
)

#' @rdname CompadreDB-Methods
#' @export
setMethod("NumberStudies",
  signature = "CompadreDB",
  function(object) {
    col_req <- c("Authors", "Journal", "YearPublication")
    col_missing <- setdiff(col_req, names(object@data))
    if (length(col_missing) > 0) {
      stop(
        "Cannot count number of studies because the following ",
        "columns are missing: ", toString(col_missing)
      )
    } else {
      return(length(unique(paste0(
        object@data$Authors,
        object@data$Journal,
        object@data$YearPublication
      ))))
    }
  }
)


#' The number of projection matrices in a CompadreDB object
#' @rdname CompadreDB-Methods
#' @export
setGeneric(
  "NumberMatrices",
  function(object) {
    standardGeneric("NumberMatrices")
  }
)

#' @rdname CompadreDB-Methods
#' @export
setMethod("NumberMatrices",
  signature = "CompadreDB",
  function(object) {
    return(nrow(object@data))
  }
)
