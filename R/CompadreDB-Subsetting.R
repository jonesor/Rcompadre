#' Subsetting CompadreDB objects
#' 
#' \code{CompadreDB} objects can be subset just like a regular
#' \code{data.frame}, using either \code{[} or \code{subset()}. Note, however,
#' that the \code{mat} column will always be retained during subsetting, even if
#' it is not included in the user's column subset.
#' 
#' @return No return value, called for side effects
#' 
#' @name CompadreDB-Subsetting
#' 
#' @examples
#' # subset to the first 10 rows
#' Compadre[1:10,]
#' 
#' # subset to the species 'Echinacea angustifolia'
#' subset(Compadre, SpeciesAccepted == "Echinacea angustifolia")
#' 
#' # remove the column SurvivalIssue
#' Compadre[,names(Compadre) != "SurvivalIssue"]
#' 
#' \dontrun{
#' # column selection doesn't include mat, but mat will still be returned with a
#' #  along with a warning
#' subset(Compadre, select = c("SpeciesAccepted", "Authors"))
#' }
NULL



#' @rdname CompadreDB-Subsetting
#' @param x A \code{CompadreDB} object
#' @param i row indices (see \link{[.data.frame})
#' @param j column indices (see \link{[.data.frame})
#' @param ... ignored
#' @param drop ignored
#' @importFrom methods new
#' @export
setMethod(f = "[", signature = signature(x = "CompadreDB",
                                         i = "ANY",
                                         j = "ANY",
                                         drop = "ANY"), 
          function(x, i, j, ..., drop = FALSE) {
            dat <- x@data
            if(!missing(j)){
              if(!any(is.logical(j), is.numeric(j), is.character(j))) {
                stop("subset criteria must be logical, numeric (column / row",
                     "numbers) or character (column / row names)")
              }
              mat_col <- which(names(dat) == "mat")
              # test for length 1 b/c x[,TRUE] should select all columns
              if(is.logical(j) & j[mat_col] != TRUE & length(j) != 1){
                warning("'mat' was included in the output by default, ",
                        "although not selected")
                j[mat_col] <- TRUE
              }
              if(is.numeric(j)){
                if (all(j >= 0) & !(mat_col %in% j)) {
                  warning("'mat' was included in the output by default, ",
                          "although not selected")
                  j <- c(mat_col, j)
                } else if (all(j < 0) & (mat_col %in% abs(j))) {
                  warning("'mat' was included in the output by default, ",
                          "although not selected")
                  if (length(j) == 1) {
                    # if trying to remove ONLY mat col, keep all cols
                    j <- TRUE
                  } else {
                    # if trying to remove mat col + others, remove only others
                    j <- j[-which(mat_col %in% abs(j))]
                  }
                }
              }
              if(is.character(j) & !("mat" %in% j)) {
                warning("'mat' was included in the output by default, ",
                        "although not selected")
                j <- c("mat", j)
              }
            }
            
            new("CompadreDB",
                data = dat[i, j],
                version = x@version)
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
