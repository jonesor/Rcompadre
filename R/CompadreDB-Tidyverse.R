#' Tidyverse methods for CompadreDB objects
#'
#' CompadreDB methods for functions in \link[dplyr]{dplyr} and
#' \link[ggplot2]{ggplot2}.
#'
#' @name CompadreDB-Tidyverse
#' @param x,model,.data A CompadreDB object
#' @param data see \code{\link[ggplot2]{fortify}}
#' @param ... other arguments
#' @param y see \code{\link[dplyr]{join}}
#' @param by see \code{\link[dplyr]{join}}
#' @param copy see \code{\link[dplyr]{join}}
#' @param suffix see \code{\link[dplyr]{join}}
#' @param add Logical indicating whether to overwrite existing groups
#'   (\code{FALSE}) or add to any existing groups (\code{TRUE})
NULL
#' @return No return value, called for side effects

#' @rdname CompadreDB-Tidyverse
fortify.CompadreDB <- function(model, data, ...) model@data


#' @rdname CompadreDB-Tidyverse
#' @importFrom methods new
filter.CompadreDB <- function(.data, ...) {
  vers <- .data@version
  .data <- .data@data
  new("CompadreDB",
    data = callGeneric(),
    version = vers
  )
}


#' @rdname CompadreDB-Tidyverse
#' @importFrom methods new
slice.CompadreDB <- function(.data, ...) {
  vers <- .data@version
  .data <- .data@data
  new("CompadreDB",
    data = callGeneric(),
    version = vers
  )
}


#' @rdname CompadreDB-Tidyverse
#' @importFrom methods new
arrange.CompadreDB <- function(.data, ...) {
  vers <- .data@version
  .data <- .data@data
  new("CompadreDB",
    data = callGeneric(),
    version = vers
  )
}


#' @rdname CompadreDB-Tidyverse
#' @importFrom methods new
mutate.CompadreDB <- function(.data, ...) {
  vers <- .data@version
  .data <- .data@data
  new("CompadreDB",
    data = callGeneric(),
    version = vers
  )
}


#' @rdname CompadreDB-Tidyverse
#' @importFrom methods new
group_by.CompadreDB <- function(.data, ..., add = FALSE) {
  vers <- .data@version
  .data <- .data@data
  new("CompadreDB",
    data = callGeneric(),
    version = vers
  )
}


#' @rdname CompadreDB-Tidyverse
#' @importFrom methods new callGeneric
ungroup.CompadreDB <- function(x, ...) {
  vers <- x@version
  x <- x@data
  new("CompadreDB",
    data = callGeneric(),
    version = vers
  )
}


#' @rdname CompadreDB-Tidyverse
summarize.CompadreDB <- function(.data, ...) {
  .data <- .data@data
  callGeneric()
}


#' @rdname CompadreDB-Tidyverse
summarise.CompadreDB <- function(.data, ...) {
  .data <- .data@data
  callGeneric()
}


#' @rdname CompadreDB-Tidyverse
#' @importFrom methods new
select.CompadreDB <- function(.data, ...) {
  vers <- .data@version
  .data <- .data@data
  new("CompadreDB",
    data = callGeneric(),
    version = vers
  )
}


#' @rdname CompadreDB-Tidyverse
#' @importFrom methods new
rename.CompadreDB <- function(.data, ...) {
  vers <- .data@version
  .data <- .data@data
  new("CompadreDB",
    data = callGeneric(),
    version = vers
  )
}


#' @rdname CompadreDB-Tidyverse
#' @importFrom methods new callGeneric
left_join.CompadreDB <- function(x, y, by = NULL, copy = FALSE,
                                 suffix = c(".x", ".y"), ...) {
  vers <- x@version
  x <- x@data
  x$DoNotUse_Temp_Sequence <- seq_len(nrow(x))
  out <- new("CompadreDB",
    data = callGeneric(),
    version = vers
  )
  if (anyDuplicated(out@data$DoNotUse_Temp_Sequence) > 0) {
    warning("One or more rows of the CompadreDB object appear to have been ",
      "duplicated during the join, indicating non-unique matches",
      call. = FALSE
    )
  }
  out@data$DoNotUse_Temp_Sequence <- NULL
  return(out)
}


#' @rdname CompadreDB-Tidyverse
#' @importFrom methods new callGeneric
right_join.CompadreDB <- function(x, y, by = NULL, copy = FALSE,
                                  suffix = c(".x", ".y"), ...) {
  vers <- x@version
  x <- x@data
  x$DoNotUse_Temp_Sequence <- seq_len(nrow(x))
  out <- new("CompadreDB",
    data = callGeneric(),
    version = vers
  )
  if (anyDuplicated(out@data$DoNotUse_Temp_Sequence) > 0) {
    warning("One or more rows of the CompadreDB object appear to have been ",
      "duplicated during the join, indicating non-unique matches",
      call. = FALSE
    )
  }
  out@data$DoNotUse_Temp_Sequence <- NULL
  return(out)
}


#' @rdname CompadreDB-Tidyverse
#' @importFrom methods new callGeneric
inner_join.CompadreDB <- function(x, y, by = NULL, copy = FALSE,
                                  suffix = c(".x", ".y"), ...) {
  vers <- x@version
  x <- x@data
  x$DoNotUse_Temp_Sequence <- seq_len(nrow(x))
  out <- new("CompadreDB",
    data = callGeneric(),
    version = vers
  )
  if (anyDuplicated(out@data$DoNotUse_Temp_Sequence) > 0) {
    warning("One or more rows of the CompadreDB object appear to have been ",
      "duplicated during the join, indicating non-unique matches",
      call. = FALSE
    )
  }
  out@data$DoNotUse_Temp_Sequence <- NULL
  return(out)
}


#' @rdname CompadreDB-Tidyverse
#' @importFrom methods new callGeneric
full_join.CompadreDB <- function(x, y, by = NULL, copy = FALSE,
                                 suffix = c(".x", ".y"), ...) {
  vers <- x@version
  x <- x@data
  x$DoNotUse_Temp_Sequence <- seq_len(nrow(x))
  out <- new("CompadreDB",
    data = callGeneric(),
    version = vers
  )
  if (anyDuplicated(out@data$DoNotUse_Temp_Sequence) > 0) {
    warning("One or more rows of the CompadreDB object appear to have been ",
      "duplicated during the join, indicating non-unique matches",
      call. = FALSE
    )
  }
  out@data$DoNotUse_Temp_Sequence <- NULL
  return(out)
}



# from https://github.com/r-spatial/sf/blob/master/R/tidyverse.R (Nov 30 2018)
# nocov start
register_all_s3_methods <- function() {
  register_s3_method("ggplot2", "fortify", "CompadreDB")
  register_s3_method("dplyr", "filter", "CompadreDB")
  register_s3_method("dplyr", "slice", "CompadreDB")
  register_s3_method("dplyr", "arrange", "CompadreDB")
  register_s3_method("dplyr", "mutate", "CompadreDB")
  register_s3_method("dplyr", "group_by", "CompadreDB")
  register_s3_method("dplyr", "ungroup", "CompadreDB")
  register_s3_method("dplyr", "summarize", "CompadreDB")
  register_s3_method("dplyr", "summarise", "CompadreDB")
  register_s3_method("dplyr", "select", "CompadreDB")
  register_s3_method("dplyr", "rename", "CompadreDB")
  register_s3_method("dplyr", "left_join", "CompadreDB")
  register_s3_method("dplyr", "right_join", "CompadreDB")
  register_s3_method("dplyr", "inner_join", "CompadreDB")
  register_s3_method("dplyr", "full_join", "CompadreDB")
}


register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(
    is.character(pkg), length(pkg) == 1,
    is.character(generic), length(generic) == 1,
    is.character(class), length(class) == 1
  )

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}
# nocov end
