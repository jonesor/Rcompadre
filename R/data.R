#' Subsample of a legacy version of the COMPADRE Plant Matrix Database for
#' testing and examples
#' 
#' @description
#' `CompadreLegacy` is a subsample of the COMPADRE Plant Matrix Database in the
#' legacy format (class 'list'), for use in testing and examples. For full
#' documentation see the [COMPADRE User
#' Guide](https://jonesor.github.io/CompadreGuides/).
#' 
#' @md
#' 
#' @format
#' A list with four elements:
#' 
#' __metadata__ - A data frame with the following 47 columns:
#' 
#' * SpeciesAuthor - Binomial species name given by the paper's author
#' * SpeciesAccepted - Accepted binomial species name taken from _The Plant
#'   List_ or _Catalogue of Life_
#' * CommonName - Common name for species used in the publication
#' * Genus - Taxonomic genus that the accepted species belongs to
#' * Family - Family that the species belongs to
#' * Order - Order that the species belongs to
#' * Class - Class that the species belongs to
#' * Phylum - Phylum that the species belongs to
#' * Kingdom - Kingdom that the species belongs to
#' * OrganismType - Organism type (see COMPADRE User Guide for documentation)
#' * DicotMonoc - Whether the species is a dicot or monocot. Non-angiosperms are
#'   `NA`.
#' * AngioGymno - Whether the species is an angiosperm or gymnosperm. Non-plants
#'   are `NA`.
#' * Authors - Last name of all authors (separated with ";")
#' * Journal - Abbreviated journal title, or type of source document (e.g. "PhD
#'   thesis")
#' * YearPublication - Year of publication
#' * DOI.ISBN - Digital Object Identifier or International Standard Book Number
#'   codes to identify each publication
#' * AdditionalSource - Additional source(s) used to reconstruct the matrix or
#'   obtain additional metadata for the matrix (if applicable)
#' * StudyDuration - Number of years of observation in study (`StudyEnd -
#'   StudyStart`)
#' * StudyStart - Study start year
#' * StudyEnd - Study end year
#' * AnnualPeriodicity - Inverse of the length of the projection interval (in
#'   years)
#' * NumberPopulations - The number of study populations as defined by the
#'   authors. Within site replication of permanent plots is not defined as
#'   separate populations
#' * MatrixCriteriaSize - Indicates whether the matrix contains stages based on
#'   size. If so, indicates what that measure of size is
#' * MatrixCriteriaOntogeny - Indicates whether the matrix contains stages based
#'   on ontogenic/developmental stages
#' * MatrixCriteriaAge - Indicates whether the matrix contains stages based on
#'   age
#' * MatrixPopulation - Population name or definition of where the matrix was
#'   recorded, usually given by the author. See COMPADRE User Guide.
#' * Lat - Latitude in decimal degrees
#' * Lon - Longitude in decimal degrees
#' * Altitude - Altitude of study site (m above sea level)
#' * Country - 3-letter ISO country code for the country in which the study took
#'   place (multiple countries separated with ";")
#' * Continent - Continent on which study took place
#' * Ecoregion - Ecoregion in which study took place. See COMPADRE User Guide.
#' * StudiedSex - Whether study included only males ("M"), only females ("F"),
#'   or both sexes ("M/F")
#' * MatrixComposite - Indicates the type of matrix population model. Possible
#'   values are `Individual`, `Mean`, `Pooled`, and `Seasonal`. See COMPADRE
#'   User Guide.
#' * MatrixTreatment - Describes if a treatment was applied to the population or
#'   not. If yes, includes a brief description of the treatment. If not,
#'   `Unmanipulated`
#' * MatrixCaptivity - Whether species was studied in the wild (`W`), captivity
#'   (`C`), or captured from a wild population (`CW`)
#' * MatrixStartYear - First year of matrix
#' * MatrixStartSeason - First season of matrix as described by author
#'   (hemisphere-specific)
#' * MatrixStartMonth - First month of matrix
#' * MatrixEndYear - Final year of matrix
#' * MatrixEndSeason - Final season of matrix as described by author
#'   (hemisphere-specific)
#' * MatrixEndMonth - Final month of matrix
#' * MatrixSplit - Whether the __A__ matrix has been split into components
#'   __U__, __F__, and __C__ ("Divided") or not ("Indivisible"). If not,
#'   elements of `matU`, `matF`, and `matC` are filled with `NA`
#' * MatrixFec - Whether fecundity was measured for the matrix model
#' * Observation - Additional observations recorded by database compilers
#' * MatrixDimension - Dimension of the __A__ matrix
#' * SurvivalIssue - Denotes the maximum stage-specific survival value
#' 
#' 
#' __mat__ - A list of population projection models, which are also
#'  in list format. Each list element contains four matrices:
#' 
#' * matA - A matrix population model
#' * matU - The survival- and growth-related component of matA
#' * matF - The sexual reproduction component of matA
#' * matC - The clonal reproduction component of matA
#' 
#' 
#' __matrixClass__ - A list of data frames, each with the following columns:
#' * MatrixClassOrganized - Standardized stage class of the matrix population
#'   model
#' * MatrixClassAuthor - Stage description from the original publication
#' * MatrixClassNumber - Integer stage number
#' 
#' 
#' __version__ - A list with the following elements:
#' * Version - The version number of the database
#' * DateCreated - The date that the `.RData` file was created
#' * NumberAcceptedSpecies - The number of accepted species in the original
#'   version
#' * NumberStudies - The number of studies in the original version
#' * NumberMatrices - The number of matrices in the original version
#' * Agreement - Link to the COMADRE license agreement
"CompadreLegacy"






#' Subsamples of the COMPADRE Plant Matrix Database and COMADRE Animal
#' Matrix Database for testing and examples
#' 
#' @name Compadre
#' 
#' @description 
#' `Compadre` (plant matrices) and `Comadre` (animal matrices) are subsamples of
#' the COMPADRE Plant Matrix Database and COMADRE Animal Matrix Database,
#' respectively, that are used for testing and examples. Each object is of class
#' 'CompadreDB' and therefore has the following two slots: `data` and `version`.
#' 
#' For full documentation see the [COMPADRE User
#' Guide](https://jonesor.github.io/CompadreGuides/).
#' 
#' @md
#' 
#' @format
#' Slot __data__ - A tibble-style data frame with the following 48 columns:
#' 
#' * mat - A list of 'CompadreMat' objects, each with the following slots:
#'   * matA - A matrix population model
#'   * matU - The survival- and growth-related component of matA
#'   * matF - The sexual reproduction component of matA
#'   * matC - The clonal reproduction component of matA
#'   * matrixClass - A data frame with the following columns:
#'     * MatrixClassOrganized - Standardized stage class of the matrix
#'       population model
#'     * MatrixClassAuthor - Stage description from the original publication
#'     * MatrixClassNumber - Integer stage number
#' * SpeciesAuthor - Binomial species name given by the paper's author
#' * SpeciesAccepted - Accepted binomial species name taken from _The Plant
#'   List_ or _Catalogue of Life_
#' * CommonName - Common name for species used in the publication
#' * Genus - Taxonomic genus that the accepted species belongs to
#' * Family - Family that the species belongs to
#' * Order - Order that the species belongs to
#' * Class - Class that the species belongs to
#' * Phylum - Phylum that the species belongs to
#' * Kingdom - Kingdom that the species belongs to
#' * OrganismType - Organism type (see COMPADRE User Guide for documentation)
#' * DicotMonoc - Whether the species is a dicot or monocot. Non-angiosperms are
#'   `NA`.
#' * AngioGymno - Whether the species is an angiosperm or gymnosperm. Non-plants
#'   are `NA`.
#' * Authors - Last name of all authors (separated with ";")
#' * Journal - Abbreviated journal title, or type of source document (e.g. "PhD
#'   thesis")
#' * YearPublication - Year of publication
#' * DOI.ISBN - Digital Object Identifier or International Standard Book Number
#'   codes to identify each publication
#' * AdditionalSource - Additional source(s) used to reconstruct the matrix or
#'   obtain additional metadata for the matrix (if applicable)
#' * StudyDuration - Number of years of observation in study (`StudyEnd -
#'   StudyStart`)
#' * StudyStart - Study start year
#' * StudyEnd - Study end year
#' * AnnualPeriodicity - Inverse of the length of the projection interval (in
#'   years)
#' * NumberPopulations - The number of study populations as defined by the
#'   authors. Within site replication of permanent plots is not defined as
#'   separate populations
#' * MatrixCriteriaSize - Indicates whether the matrix contains stages based on
#'   size. If so, indicates what that measure of size is
#' * MatrixCriteriaOntogeny - Indicates whether the matrix contains stages based
#'   on ontogenic/developmental stages
#' * MatrixCriteriaAge - Indicates whether the matrix contains stages based on
#'   age
#' * MatrixPopulation - Population name or definition of where the matrix was
#'   recorded, usually given by the author. See COMPADRE User Guide.
#' * Lat - Latitude in decimal degrees
#' * Lon - Longitude in decimal degrees
#' * Altitude - Altitude of study site (m above sea level)
#' * Country - 3-letter ISO country code for the country in which the study took
#'   place (multiple countries separated with ";")
#' * Continent - Continent on which study took place
#' * Ecoregion - Ecoregion in which study took place. See COMPADRE User Guide.
#' * StudiedSex - Whether study included only males ("M"), only females ("F"),
#'   or both sexes ("M/F")
#' * MatrixComposite - Indicates the type of matrix population model. Possible
#'   values are `Individual`, `Mean`, `Pooled`, and `Seasonal`. See COMPADRE
#'   User Guide.
#' * MatrixTreatment - Describes if a treatment was applied to the population or
#'   not. If yes, includes a brief description of the treatment. If not,
#'   `Unmanipulated`
#' * MatrixCaptivity - Whether species was studied in the wild (`W`), captivity
#'   (`C`), or captured from a wild population (`CW`)
#' * MatrixStartYear - First year of matrix
#' * MatrixStartSeason - First season of matrix as described by author
#'   (hemisphere-specific)
#' * MatrixStartMonth - First month of matrix
#' * MatrixEndYear - Final year of matrix
#' * MatrixEndSeason - Final season of matrix as described by author
#'   (hemisphere-specific)
#' * MatrixEndMonth - Final month of matrix
#' * MatrixSplit - Whether the __A__ matrix has been split into components
#'   __U__, __F__, and __C__ ("Divided") or not ("Indivisible"). If not,
#'   elements of `matU`, `matF`, and `matC` are filled with `NA`
#' * MatrixFec - Whether fecundity was measured for the matrix model
#' * Observation - Additional observations recorded by database compilers
#' * MatrixDimension - Dimension of the __A__ matrix
#' * SurvivalIssue - Denotes the maximum stage-specific survival value
#' \cr
#' 
#' Slot __version__ - A list with the following elements:
#' * Version - The version number of the database
#' * DateCreated - The date that the `.RData` file was created
#' * Agreement - Link to the COMADRE license agreement
NULL

#' @rdname Compadre
"Compadre"

#' @rdname Compadre
"Comadre"
