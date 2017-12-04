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
#' 
#' 
CleanDB <- function(DB) {
  
  
   <- sapply(DB$mat, function(x) any(is.na(x$matA)))
  
  
}
