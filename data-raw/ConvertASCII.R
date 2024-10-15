library(Rcompadre)

#COMPADRE  -----
# Create a copy of Compadre to store the corrected version
Compadre2 <- Compadre

# Loop through all columns in Compadre
for (col_name in names(Compadre)) {
  # Check if the column is of character type (since non-ASCII makes sense only for character columns)
  if (is.character(Compadre[[col_name]])) {
    # Find non-ASCII characters in the current column
    nonASCII <- tools::showNonASCII(Compadre[[col_name]])
    
    # If there are non-ASCII characters, replace them with ASCII equivalents
    if (length(nonASCII) > 0) {
      Compadre2[[col_name]] <- stringi::stri_trans_general(Compadre[[col_name]], "latin-ascii")
    }
  }
}

#COMADRE -----
# Create a copy of Comadre to store the corrected version
Comadre2 <- Comadre

# Loop through all columns in Comadre
for (col_name in names(Comadre)) {
  # Check if the column is of character type (since non-ASCII makes sense only for character columns)
  if (is.character(Comadre[[col_name]])) {
    # Find non-ASCII characters in the current column
    nonASCII <- tools::showNonASCII(Comadre[[col_name]])
    
    # If there are non-ASCII characters, replace them with ASCII equivalents
    if (length(nonASCII) > 0) {
      Comadre2[[col_name]] <- stringi::stri_trans_general(Comadre[[col_name]], "latin-ascii")
    }
  }
}

#COMPADRE LEGACY -----
# Create a copy of Comadre to store the corrected version
CompadreLegacy2 <- CompadreLegacy

# Loop through all columns in Comadre
for (col_name in names(CompadreLegacy)) {
  # Check if the column is of character type (since non-ASCII makes sense only for character columns)
  if (is.character(CompadreLegacy[[col_name]])) {
    # Find non-ASCII characters in the current column
    nonASCII <- tools::showNonASCII(CompadreLegacy[[col_name]])
    
    # If there are non-ASCII characters, replace them with ASCII equivalents
    if (length(nonASCII) > 0) {
      CompadreLegacy2[[col_name]] <- stringi::stri_trans_general(CompadreLegacy[[col_name]], "latin-ascii")
    }
  }
}


Compadre <- Compadre2
Comadre <- Comadre2
CompadreLegacy <- CompadreLegacy2

usethis::use_data(Comadre,
                  Compadre,
                  CompadreLegacy,
                  overwrite = TRUE
)
