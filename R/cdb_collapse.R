#' Collapse a COM(P)ADRE database by averaging matrices over levels of one or
#' more grouping variables
#'
#' @description
#' Collapses a CompadreDB object by averaging matrices over levels of one or
#' more grouping variables (e.g. SpeciesAuthor, MatrixPopulation).
#'
#' For a given study and species, a COM(P)ADRE database may contain multiple
#' matrices, reflecting different combinations of population, year, and/or
#' treatment. Collapsing allows a user to obtain a single 'grand mean matrix'
#' for each group of interest (e.g. MatrixPopulation), and therefore limit
#' pseudoreplication.
#'
#' All members of a group \emph{must} have the same matrix dimension (consider
#' adding MatrixDimension as a grouping variable). All members of a group
#' \emph{should} have the same ProjectionInterval and matrix stage class
#' definitions (see \code{\link{cdb_id_stages}}). Note that Seasonal matrices
#' should not be collapsed using this method (they should be matrix-multiplied
#' rather than averaged).
#'
#' @param cdb A CompadreDB object
#' @param columns Vector of grouping variables to collapse over (corresponding
#'   to columns within \code{cdb})
#'
#' @return A CompadreDB object
#'
#' @details
#' Will give a warning if members of any group do not all share the same
#' ProjectionInterval or stage class definitions, or if \code{cdb} contains any
#' rows with a MatrixComposite value of "Seasonal".
#'
#' Prior to collapsing, columns of class 'factor' will be coerced to
#' 'character', and any list-column apart from \code{mat} will be removed.
#'
#' Within a group, rows of a given column are collapsed as follows:
#'
#' \itemize{
#'   \item \code{mat}: matrices are averaged using \link{mpm_mean}, and stage
#'     class definitions are taken from the first group member
#'   \item \code{MatrixComposite}: return original value if N = 1, else return
#'     "Collapsed"
#'   \item \code{Lat}: re-calculated by averaging Lat column (if available)
#'   \item \code{Lon}: re-calculated by averaging Lon column (if available)
#'   \item \code{SurvivalIssue}: re-calculated from the collapsed \code{mat}
#'     (\code{max(colSums(matU))})
#'   \item \code{others}: if all elements equal return that unique value, else
#'     paste together all unique values separated by "; "
#' }
#'
#' @author Patrick M. Barks <patrick.barks@@gmail.com>
#' @author Owen R. Jones <jones@@biology.sdu.dk>
#'
#' @family data checking
#'
#' @seealso \link{cdb_id_stages}
#'
#' @examples
#' # filter out Seasonal matrices
#' CompSub <- subset(Compadre, MatrixComposite != "Seasonal")
#'
#' # add column identifying unique stage class definitions
#' CompSub$id_stage <- cdb_id_stages(CompSub, "MatrixClassOrganized")
#'
#' # collapse
#' CompCollapse <- cdb_collapse(CompSub, columns = c("id_stage"))
#'
#' @importFrom methods new
#' @importFrom tibble as_tibble add_column
#' @export cdb_collapse
cdb_collapse <- function(cdb, columns) {
  if ("MatrixComposite" %in% names(cdb) &&
    any(cdb$MatrixComposite == "Seasonal")) {
    warning("cdb contains rows with MatrixComposite == 'Seasonal'. This ",
      "method of collapsing is not suitable for seasonal matrices. ",
      "Consider removing prior to collapsing.",
      call. = FALSE
    )
  }

  # leave other validation to cdb_id

  # create a unique integer identifier for each group to be collapsed
  id_collapse <- cdb_id(cdb, columns)
  id_unique <- unique(id_collapse)

  # extract data slot
  dat <- cdb@data

  # remove any list-columns except for 'mat'
  col_mat <- names(dat) == "mat"
  col_list <- vapply(dat, class, "", USE.NAMES = FALSE) == "list"
  dat <- dat[, col_mat | !col_list]

  # coerce factor columns to character
  is_factor <- vapply(dat, is.factor, logical(1), USE.NAMES = FALSE)
  if (any(is_factor)) {
    j <- which(is_factor)
    for (i in j) dat[[i]] <- as.character(dat[[i]])
  }

  # collapse to list of 1-row data frames for each group
  coll_l <- lapply(id_unique,
    FUN = CollapseWrapper,
    dat = dat,
    id_collapse = id_collapse
  )

  # bind data frames
  coll_dat <- do.call(rbind, coll_l)

  new("CompadreDB",
    data = coll_dat,
    version = cdb@version
  )
}




CollapseWrapper <- function(id, dat, id_collapse) {
  dat <- dat[id_collapse %in% id, ]
  CollapseFn(dat)
}


CollapseFn <- function(x) {
  mat <- mpm_mean(x$mat)
  d <- as_tibble(lapply(x[, -1], CollapseCol))
  d <- add_column(d, mat = list(mat), .before = 1)
  if (nrow(x) > 1) {
    d$MatrixComposite <- "Collapsed"
    if ("Lat" %in% names(x)) d$Lat <- mean(x$Lat)
    if ("Lon" %in% names(x)) d$Lon <- mean(x$Lon)
    d$SurvivalIssue <- max(colSums(mat@matU))
  }
  return(d)
}


CollapseCol <- function(col) {
  ifelse(length(unique(col)) == 1,
    unique(col),
    paste(unique(col), collapse = "; ")
  )
}
