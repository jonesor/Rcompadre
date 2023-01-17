#' Compare two versions or subsets of a COM(P)ADRE database
#'
#' Prints a summary of the differences between two CompadreDB objects, including
#' the number of species, studies, and matrices in each. If argument
#' \code{verbose == TRUE}, additionally prints a list of the species and studies
#' that are present in one database but not the other.
#'
#' @param cdb1,cdb2 CompadreDB objects to compare
#' @param verbose Logical argument indicating whether or not to return lots of
#'   detail
#'
#' @return NULL. Output is printed rather than returned.
#'
#' @author Owen R. Jones <jones@@biology.sdu.dk>
#'
#' @family data checking
#'
#' @examples
#' Compadre1 <- subset(Compadre, Continent == "Asia")
#' Compadre2 <- subset(Compadre, Continent == "Africa")
#'
#' cdb_compare(Compadre1, Compadre2)
#'
#' @export cdb_compare
cdb_compare <- function(cdb1, cdb2, verbose = FALSE) {
  if (!inherits(cdb1, "CompadreDB") | !inherits(cdb2, "CompadreDB")) {
    stop("cdbs must be of class CompadreDB. See function as_cdb")
  }

  # Quick summary
  cat("Quick Summary...\n\n")

  # File 1
  cat(paste("cdb1 contains data for:\n",
    NumberStudies(cdb1), " source papers\n",
    NumberAcceptedSpecies(cdb1), " accepted species\n",
    NumberMatrices(cdb1), " matrices\n\n",
    sep = ""
  ))

  # File 2
  cat(paste("cdb2 contains data for:\n",
    NumberStudies(cdb2), " source papers\n",
    NumberAcceptedSpecies(cdb2), " accepted species\n",
    NumberMatrices(cdb2), " matrices\n\n",
    sep = ""
  ))

  if (verbose == TRUE) {
    cat("Detailed summary...\n\n")

    # Accepted species in File 1 that are not in File 2
    sp1 <- unique(cdb1$SpeciesAccepted)
    sp2 <- unique(cdb2$SpeciesAccepted)

    cat("Species in cdb1 not in cdb2:\n")
    print(sp1[which(!sp1 %in% sp2)])

    cat("Species in cdb2 not in cdb1:\n")
    print(sp2[which(!sp2 %in% sp1)])

    # Get unique author species for both files
    # asp1 <- unique(SpeciesAuthor(cdb1))
    # asp2 <- unique(SpeciesAuthor(cdb2))

    # cat("Number of study-species combinations in cdb1\n")
    # print(length(asp1))

    # cat("Number of study-species combinations in cdb2\n")
    # print(length(asp2))

    # cat("Study-species in cdb1 that are not in cdb2\n")
    # print(asp1[which(!asp1%in%asp2)])

    # cat("Study-species in cdb2 that are not in cdb1\n")
    # print(asp2[which(!asp2%in%asp1)])

    uniqueSource1 <- unique(paste(cdb1$Authors, " (",
      cdb1$YearPublication, ") ",
      cdb1$Journal,
      sep = ""
    ))

    uniqueSource2 <- unique(paste(cdb2$Authors, " (",
      cdb2$YearPublication, ") ",
      cdb2$Journal,
      sep = ""
    ))

    cat("\n\nSource papers in cdb2 not in cdb1\n")
    print(sort(uniqueSource2[which(!uniqueSource2 %in% uniqueSource1)]))

    cat("\n\nSource papers in cdb1 not in cdb2\n")
    print(sort(uniqueSource1[which(!uniqueSource1 %in% uniqueSource2)]))

    cat("See the User Guide for definitions\n")
  }
}
