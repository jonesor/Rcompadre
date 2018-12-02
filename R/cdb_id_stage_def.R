#' Create integer identifiers for a COM(P)ADRE database corresponding to unique
#' combinations of species, study, and matrix stage class definitions
#'
#' Creates a vector of integer identifiers corresponding to the rows of a
#' CompadreDB object, based on unique combinations of the column 'SpeciesAuthor'
#' and a list of matrix stage class definitions (either 'MatrixClassAuthor' or
#' 'MatrixClassOrganized').
#'
#' @param cdb A CompadreDB object
#' @param stage_def Whether to define matrix stage class based on
#'   'MarixClassAuthor' or 'MatrixClassOrganized' (see \emph{Details}).
#' 
#' @return Vector of integer identifiers corresponding to the rows of
#'   \code{cdb}.
#' 
#' @author Patrick Barks <patrick.barks@@gmail.com>
#' 
#' @details
#' The vector 'MatrixClassOrganized' reflects standardized stage classes
#' ('prop', 'active', or 'dorm'), whereas 'MatrixClassAuthor' reflects a
#' description of the stage classes as defined by the original author (e.g.
#' \code{c('Seedling', 'Medium rosette', 'Large (2 rosettes)', 'Flowering')}).
#' 
#' Because the 'MatrixClassAuthor' definitions are less standardized, they are
#' more prone to typos that could lead to slight differences between stage
#' descriptions of matrices that really do have the same stage classes (e.g. a
#' set of matrices from a single study/species/population). Therefore, using
#' 'MarixClassAuthor' to define stage classes is potentially prone to mistakenly
#' 'splitting' identifiers that should really be the same.
#' 
#' 'MatrixClassOrganized' has the opposite problem. It's possible for two
#' matrices from a given study to have the same stage definitions based on
#' 'MatrixClassOrganized', but legitimately differ in stage definitions as
#' defined by the author. Therefore, using 'MarixClassAuthor' to define stage
#' classes is potentially prone to mistakenly 'lumping' identifiers that should
#' actually differ.
#' 
#' Because the majority of studies in COM(P)ADRE use a single set of stage
#' definitions for all matrices, and typos are rare, results for the different
#' stage definitions will usually be similar. Note, however, that the actual
#' integers returned for the different stage definitions are likely to be very
#' different (because they are based on alphabetical order).
#' 
#' @seealso \link{cdb_id}
#' 
#' @examples
#' cdb_id_stage_def(Compadre, stage_def = "MatrixClassOrganized")
#' 
#' @export cdb_id_stage_def
cdb_id_stage_def <- function(cdb, stage_def) {

  if (!inherits(cdb, "CompadreDB")) {
    stop("db must be of class CompadreDB. See function as_cdb")
  }
  
  stage_def <- match.arg(stage_def,
                         c("MatrixClassOrganized", "MatrixClassAuthor"))
  
  stage_fun <- match.fun(stage_def)
  cdb@data$stage_def <- stage_fun(cdb)
  cdb@data$stage_def <- vapply(cdb@data$stage_def, paste, collapse = "", "")
  
  cdb_id(cdb, columns = c("SpeciesAuthor", "stage_def"))
}

