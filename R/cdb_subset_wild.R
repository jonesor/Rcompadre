#' Subset a COM(P)ADRE database to wild, unmanipulated populations
#' 
#' @description 
#' Subsets a CompadreDB object to the rows reflecting wild, unmanipulated
#' populations \code{(MatrixCaptivity == "W" & MatrixTreatment ==
#' "Unmanipulated")}.
#' 
#' Note that such a subset may be overly stringent for some analyses, as it
#' excludes populations subject to "treatments" that may not necessitate
#' exclusion (e.g. "treatment" may reflect grazing status, time since fire,
#' etc.).
#' 
#' @param cdb A CompadreDB object
#' 
#' @return A CompadreDB object reflecting the subset of \code{cdb} corresponding
#'   to wild (\code{MatrixCaptivity = "W"}), unmanipulated
#'   (\code{MatrixTreatment == "Unmanipulated"}) populations.
#'
#' @author Patrick M. Barks <patrick.barks@@gmail.com>
#' 
#' @examples
#' CompadreWild <- cdb_subset_wild(Compadre)
#' 
#' @export cdb_subset_wild
cdb_subset_wild <- function(cdb) {
  
  if (!inherits(cdb, "CompadreDB")) {
    stop("cdb must be of class CompadreDB. See function as_cdb")
  }
  
  cdb[!is.na(cdb$MatrixTreatment) &
        !is.na(cdb$MatrixCaptivity) &
        cdb$MatrixTreatment == "Unmanipulated" &
        cdb$MatrixCaptivity == "W",]
}

