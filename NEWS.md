# Rcompadre (Development version)

* No changes yet.

# Rcompadre 1.0.0

* Updated DESCRIPTION with new contributors.
* Improved documentation to ensure it is more understandable to less experienced users.
* Grouped functions by type in the documentation.
* Improved vignettes with a range of examples demonstrating the range of basic to complex features.
* Added an argument (`check_surv_gte_1`) to `cdb_check` to check `matU` for elements greater than or equal to 1.
* Added `cdb_metadata` function to provide easy access to metadata (without associated matrices).
* Matrix accessor functions (`matA`, `matU` etc.) now ensure that the matrices are provided with named columns and rows. Names are prefixed by matrix type (e.g. A1, A2, A3 or U1, U2, U3).
* Added machine-readable codemeta-data information (`codemeta.json`)
* Unit tests improved to increase coverage.
* Modified build checks via continuous integration on Travis, Appveyor and GitHub actions (including weekly checks).

# Rcompadre 0.2.0 

* Modifications to improve the class definitions, which are renamed to `CompadreDB` and `CompadreM`.
* Added generic functionality with accessor methods via `ClassUnionMethods`. e.g. functions `matA`, `matU`, `matF`, and `matC` output all A,U,F, or C matrices from a database.
* Added `cdb_fetch` function to obtain the latest version of COMPADRE or COMADRE.
* The database object behaves more like a data frame than the complex hierarchical object it is.
* Added Tidyverse support so that most `dplyr` functions can be used with the database (e.g. filter).
* Generic subsetting methods including `subset`, `[]` now work.
* Database can be used with `magrittr` pipes.
* Most functions renamed with `cdb_` prefix, followed by verb (e.g. `dbCompare` becomes `cdb_compare` and `cleanDatabase` becomes `cdb_flag`).
* The function `cdb_unnest` unnests the DB by spreading the matrices into separate list columns. 
* The function `convert2flat` is replaced with `cdb_flatten`, but works on an entire database rather than a single matrix. `cdb_unflatten` reverses the procedure.
* Other functions are also renamed with more intuitive names.
* Functions `collapseMatrix`, `identifyReprodStages`, `rearrangeMatrix`, `splitMatrix` moved to the `Rage` package.
* Added simple vignettes.
* Added unit tests for all functions.

# Rcompadre 0.1.0

* First pre-release version!
* Established S4 class `CompadreData` with definition and methods.
* Established S4 class `CompadreM` with definition and methods.
* Functions to `checkspecies`, check for matrix problems (`cleanDatabase`), 
* Functions to manipulate the databases: merge databases (`mergeDB`), compare database versions (`dbCompare`), subset the database (`subsetDB`).
* Functions to manipulate matrices: to collapse matrix to a smaller number of stages (`collapseMatrix`), convert matrix to a "flat" format (`convert2flat`), calculate the mean F matrix (`getMeanMatF`), segregate reproductive/non-reproductive stages `rearrangeMatrix`, split the matrix into submatrices (`splitMatrix`) 
* Function to produce a matrix from a string representation `stringtomatrix`.
* Added continuous integration.
