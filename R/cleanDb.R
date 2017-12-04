#' A function to create indicator vectors to flag problems in matrices
#' of the COMPADRE/COMADRE database
#' 
#' This function allows users to create input that subsequently can be used to 
#' subset the COMPADRE/COMADRE database by
#' logical argument.
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' %% ~~ This needs to be polished. ~~
#'
#'#' @export
#' @param db The COMPADRE or COMADRE database object.#' 
#' @return Returns the database, with an additional set of metadata to indicate
#' in T/F vector forms whether or not the different matrices have issues with the data
#' plus whether or not the AMat is irreducible, ergodic, and primitive.
#' @note %% ~~further notes~~
#' @author Julia Jones <juliajones@@biology.sdu.dk>
#' 
#' Roberto Salguero-Goméz
#' 
#' Danny Buss
#' 
#' Patrick Barks
#' 
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% references
#' @keywords ~kwd1 ~kwd2
#' @examples


CleanDB <- function(DB) {
  DB$metadata$index <- 1:nrow(DB$metadata)

  DB$metadata$check_NA_A <- sapply(DB$mat, function(x) any(is.na(x$matA)))
  DB$metadata$check_NA_U <- sapply(DB$mat, function(x) any(is.na(x$matU)))
  DB$metadata$check_NA_F <- sapply(DB$mat, function(x) any(is.na(x$matF)))
  DB$metadata$check_NA_C <- sapply(DB$mat, function(x) any(is.na(x$matC)))
  
  DB$metadata$check_colsums_U <- sapply(DB$mat, function(x) any(colSums(x$matU, na.rm = T) > 1))
  
  DB_sub <- subsetDB(DB, check_NA_A == F)
  DB_sub$metadata$check_ergodic <- sapply(DB_sub$mat, function(x) popdemo::isErgodic(x$matA))
  DB_sub$metadata$check_primitive <- sapply(DB_sub$mat, function(x) popdemo::isPrimitive(x$matA))
  DB_sub$metadata$check_irreducible <- sapply(DB_sub$mat, function(x) popdemo::isIrreducible(x$matA))
  
  DB_sub$metadata <- subset(DB_sub$metadata, select = c('index', 'check_ergodic', 'check_primitive', 'check_irreducible'))
  
  DB$metadata <- merge(DB$metadata, DB_sub$metadata, by = 'index', all.x = T)
  DB$metadata <- subset(DB$metadata, select = -index)
}
