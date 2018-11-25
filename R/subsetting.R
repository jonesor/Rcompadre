#' Subsetting CompadreDB objects
#' 
#' This method enables subsetting the data using square brackets, i.e. for a
#' CompadreDB object "DB" one can use DB[1:2, 1:10] to get a new CompadreDB
#' object that includes the first two matrices and the the first 10 columns of 
#' their associated metadata. It's possible to pass logical vectors, numeric
#' vectors, or character vectors that match the row or column names of the 
#' metadata. 
#' @name Subset-CompadreDB
NULL



#' @rdname Subset-CompadreDB
#' @param x A CompadreDB object
#' @param i row indices (see \link{[.data.frame})
#' @param j column indices (see \link{[.data.frame})
#' @param ... ignored
#' @param drop ignored
#' @importFrom methods new
#' @export
setMethod(f = "[", signature = signature(x = "CompadreDB", i = "ANY", j = "ANY", drop = "ANY"), 
          function(x, i, j, ..., drop = FALSE) {
            dat <- CompadreData(x)
            if(!missing(i)){
              if(!any(is.logical(i), is.numeric(i), is.character(i))) {
                stop("subset criteria must be logical, numeric (column / row numbers)\nor character (column / row names)")
              }
            }
            if(!missing(j)){
              if(!any(is.logical(j), is.numeric(j), is.character(j))) {
                stop("subset criteria must be logical, numeric (column / row numbers)\nor character (column / row names)")
              }
              mat_col <- which(names(dat) == "mat")
              # test for length 1 b/c in [.data.frame, x[,TRUE] selects all columns
              if(is.logical(j) & j[mat_col] != TRUE & length(j) != 1){
                warning("'mat' was included in the output by default, although not selected")
                j[mat_col] <- TRUE
              }
              if(is.numeric(j) & !(mat_col %in% j)){
                warning("'mat' was included in the output by default, although not selected")
                j <- c(mat_col, j)
              }
              if(is.character(j) & !("mat" %in% j)){
                warning("'mat' was included in the output by default, although not selected")
                j <- c("mat", j)
              }
            }
            
            new("CompadreDB",
                CompadreData = dat[i, j, drop = FALSE],
                VersionData = VersionData(x))
          }
)



#' @rdname Subset-CompadreDB
#' @param subset logical expression indicating which rows to keep
#' @param select expression indicating which columns to keep
#' @importFrom methods slotNames
#' @export
subset.CompadreDB <- function(x, subset, select, drop = FALSE, ...) {
  
  if (!"CompadreData" %in% slotNames(x)) {
    stop("subset method requires CompadreDB object with slot 'data'")
  }
  
  dat <- CompadreData(x)
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
