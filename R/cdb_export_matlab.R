#' Export a COM(P)ADRE database to a Matlab-friendly flat text file
#'
#' Exports a \code{CompadreDB} object to a delimited text file that can be read
#' directly into Matlab with functions such as \code{readtable()}. Matrices and
#' stage vectors are converted to string representation using
#' \code{\link{cdb_flatten}}, so each database row remains a single tabular row
#' in the output file.
#'
#' @param cdb A CompadreDB object
#' @param file Path to the output text file
#' @param format Output format. Either \code{"csv"} or \code{"tsv"}
#' @param na String to use for missing values in the exported file
#' @param quote Logical indicating whether character fields should be quoted
#'
#' @return Invisibly returns the normalized path to the written file
#'
#' @author Owen R. Jones <jones@@biology.sdu.dk>
#'
#' @family data management
#'
#' @examples
#' \dontrun{
#' out_file <- tempfile(fileext = ".csv")
#' cdb_export_matlab(Compadre, out_file)
#' }
#'
#' @export cdb_export_matlab
cdb_export_matlab <- function(cdb, file, format = c("csv", "tsv"),
                              na = "NA", quote = TRUE) {
  if (!inherits(cdb, "CompadreDB")) {
    stop("cdb must be of class CompadreDB. See function as_cdb")
  }

  if (!is.character(file) || length(file) != 1 || is.na(file) || !nzchar(file)) {
    stop("file must be a single non-empty file path")
  }

  format <- match.arg(format)
  db_flat <- cdb_flatten(cdb)

  list_cols <- names(db_flat)[vapply(db_flat, is.list, logical(1))]
  for (col in list_cols) {
    db_flat[[col]] <- vapply(db_flat[[col]], function(x) {
      if (!is.atomic(x)) {
        stop(
          "Cannot export column '", col,
          "' because it contains non-atomic list elements"
        )
      }
      vec_to_string(x)
    }, character(1))
  }

  if (format == "csv") {
    utils::write.csv(
      db_flat,
      file = file,
      row.names = FALSE,
      na = na,
      quote = quote
    )
  } else {
    utils::write.table(
      db_flat,
      file = file,
      sep = "\t",
      row.names = FALSE,
      col.names = TRUE,
      na = na,
      quote = quote
    )
  }

  invisible(normalizePath(file, winslash = "/", mustWork = FALSE))
}
