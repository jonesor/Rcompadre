#' Merge COM(P)ADRE databases via row-bind
#'
#' Merges one or more CompadreDB objects via a row-bind of the data slots.
#'
#' @param ... CompadreDB objects
#' @param fill Logical indicating whether data-slot columns should be unioned
#'   across inputs and missing columns filled with \code{NA}. If \code{FALSE}
#'   (default), all inputs must have identical column names.
#'
#' @return A CompadreDB object created by binding the rows of all supplied
#'   CompadreDB objects
#'
#' @author Sam Levin <levisc8@@gmail.com>
#' @author Owen R. Jones <jones@@biology.sdu.dk>
#'
#' @family data management
#'
#' @examples
#' Compadre1 <- subset(Compadre, Continent == "Asia")
#' Compadre2 <- subset(Compadre, Continent == "Africa")
#'
#' Compadre3 <- subset(Compadre, Continent == "Europe")
#'
#' cdb_rbind(Compadre1, Compadre2)
#' cdb_rbind(Compadre1, Compadre2, Compadre3)
#' cdb_rbind(Compadre1, Compadre2[, names(Compadre2) != "CommonName"], fill = TRUE)
#'
#' @importFrom methods new
#' @export cdb_rbind
cdb_rbind <- function(..., fill = FALSE) {
  cdbs <- list(...)

  if (length(cdbs) < 2) {
    stop("Please provide at least two CompadreDB objects.")
  }

  if (!all(vapply(cdbs, inherits, logical(1), what = "CompadreDB"))) {
    stop("cdbs must be of class CompadreDB. See function as_cdb")
  }

  # cdbs must have matching columns to merge
  dats <- lapply(cdbs, CompadreData)
  ref_names <- names(dats[[1]])
  if (!fill &&
      !all(vapply(dats, function(x) identical(names(x), ref_names), logical(1)))) {
    stop("Data components do not have identical names. ",
      "Make sure the data slot \n",
      "in each is identical to other.",
      Call. = FALSE
    )
  }
  if (fill) {
    dats <- .align_cdb_rbind_cols(dats)
  }

  # test whether cdbs have same version info
  versions <- lapply(cdbs, VersionData)
  ref_version <- versions[[1]]

  if (all(vapply(versions, function(x) isTRUE(all.equal(ref_version, x)), logical(1)))) {
    vers_out <- ref_version
  } else {
    # if version info differs, set Version and DateCreated to NA
    vers_out <- ref_version
    vers_out$Version <- NA_character_
    vers_out$DateCreated <- NA_character_
  }

  new("CompadreDB",
    data = do.call(rbind, dats),
    version = vers_out
  )
}


.cdb_missing_col_like <- function(proto, n) {
  if (is.list(proto)) {
    return(vector("list", n))
  }

  out <- proto[rep(NA_integer_, n)]
  names(out) <- NULL
  out
}


.align_cdb_rbind_cols <- function(dats) {
  all_names <- unique(unlist(lapply(dats, names), use.names = FALSE))

  prototypes <- lapply(all_names, function(col) {
    for (dat in dats) {
      if (col %in% names(dat)) {
        return(dat[[col]])
      }
    }
    NULL
  })
  names(prototypes) <- all_names

  lapply(dats, function(dat) {
    missing_cols <- setdiff(all_names, names(dat))

    for (col in missing_cols) {
      dat[[col]] <- .cdb_missing_col_like(prototypes[[col]], nrow(dat))
    }

    dat[, all_names, drop = FALSE]
  })
}
