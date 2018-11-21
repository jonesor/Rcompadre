#' COMPADRE Animal demography data base
#' 
#' A \code{CompadreDB} object consisting of \code{metadata},
#' \code{mat}, and \code{version}
#' 
#' @format 
#' \itemize{
#'   \item{\strong{metadata}}{ - A \code{data.frame} with 130 rows and 47 columns}
#'     \describe{
#'       \item{SpeciesAuthor}{character - Binomial species name
#'        given by the paper author}
#'        \item{SpeciesAccepted}{character - Accepted binomial species name 
#'        taken from \emph{The Plant List}}
#'        \item{CommonName}{character - The common name used in the publication}
#'        \item{Genus}{character - Taxonomic genus name derived from
#'        \emph{The Plant List}}
#'        \item{Family}{factor - Family which the species belongs}
#'        \item{Order}{factor - Order to which the species belongs}
#'        \item{Class}{factor - Class to which the species belongs}
#'        \item{Phylum}{factor - Phylum to which the species belongs}
#'        \item{Kingdom}{factor - Kingdom to which the species belongs}
#'        \item{OrganismType}{factor - General plant/algae type. This is
#'        mainly based on architectural organization. The species was 
#'        assigned to one of the following values based on descriptions
#'        by the authors and/or external sources: \code{Algae},
#'        \code{Fungi}, \code{Annual}, \code{Bryophyte}, \code{Epiphyte},
#'        \code{Fern}, \code{Herbaceous perennial}, \code{Liana}, \code{Palm},
#'        \code{Shrub}, \code{Succulent}, and \code{Tree}. For further details
#'        on each level, please see 
#'        \url{https://github.com/jonesor/compadreDB/blob/master/COMPADRE-UserGuide/COMPADRE-UserGuide.pdf}
#'        }
#'        \item{DicotMonoc}{factor - Designates whether the species is a
#'        dicot or monocot. Non-angiosperm species are designated \code{NA}}
#'        \item{AngioGymno}{factor - Designates whether the species is a
#'        gymnosperm or angiosperm. Species that are neither are designated
#'        \code{NA}}
#'        \item{Authors}{characer - Last (family) name of all authors 
#'        separated with a ";"}
#'        \item{Journal}{character - The type of document from which the
#'        data were sourced. Possible values are \code{abbreviated
#'        journal name}, \code{Book}, \code{PhD thesis}, \code{MSc thesis},
#'        \code{Report}, \code{Conference talk}, and \code{Conference poster}}
#'        \item{YearPublication}{character - The year of publication}
#'        \item{DOI.ISBN}{character - The Digitial Object Identifier or 
#'        International Standard Book Number codes to identify each 
#'        publication.}
#'        \item{AdditionalSource}{character - Additional source(s) used to 
#'        reconstruct the matrix or obtain additional metadata for the matrix
#'        (if applicable)}
#'        \item{StudyDuration}{numeric - The number of years of observation
#'        for the study. This is calculated as \code{StudyEnd} - 
#'        \code{StudyStart}} 
#'        \item{StudyStart}{numeric - The year that the study began}
#'        \item{StudyEnd}{numeric - The year that the study ended}
#'        \item{AnnualPeriodicty}{numeric - The time frame for which seasonal
#'        or annual MPMs are constructed. A value of \code{1} indicates that
#'        the iteration period is 1 year, \code{2} indicates iterations twice
#'        per year, \code{0.2} indicates every 5 years, etc.}
#'        \item{NumberPopulations}{numeric - The number of study populations
#'        as defined by the authors. Within site replication of permanent 
#'        plots is not defined as separate populations}
#'        \item{MatrixCriteriaSize}{factor - Indicates whether the matrix
#'        contains stages based on size. If so, indicates what that measure
#'        of size is}
#'        \item{MatrixCriteriaOntogeny}{factor - Indicates whether the matrix
#'        contains stages based on ontogenic/developmental stages}
#'        \item{MatrixCriteriaAge}{factor - Indicates whether the matrix 
#'        contains stages based on age}
#'        \item{MatrixPopulation}{character - Definition of where the matrix
#'        was recorded, usually given by the author. In some cases, 
#'        where the author provides no name, we give the closest geographic 
#'        location instead. If there are multiple populations in the study 
#'        and their names are not pertinent/available, sequential names in 
#'        alphabetical order are assigned for each population in the study
#'        (e.g. \code{A}, \code{B}, \code{C}, etc)}
#'        \item{Lat}{numeric - The latitude in decimal format. Negative
#'        values indicate the site is in the southern hemisphere}
#'        \item{Lon}{numeric - The longitude in decimal format. Negative
#'        values indicate the site is west of the Greenwich Meridian}
#'        \item{Altitude}{numeric - Altitude of the studied population. Units
#'        are meters above sea level}
#'        \item{Country}{factor - The country where the study took place.
#'        3-letter ISO country codes are used
#'        (see \url{https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3} for
#'        codes). Studies taking place in multiple countries have each 
#'        country separated by a \code{;}}
#'        \item{Continent}{factor - The continent where the study took place.
#'        possible entries are \code{Africa}, \code{Asia}, \code{Europe},
#'        \code{N America} (USA, Canada, and Mexico), \code{S America} (everything
#'        in the Americas except USA, Canada, and Mexico), and \code{Oceania}.
#'        Various definitions of Oceania exist - we have opted for this one:
#'        \url{http://en.wikipedia.org/wiki/List_of_Oceanian_countries_by_population}}
#'        \item{Ecoregion}{character - Indicates the ecoregion the study
#'        occurred in. These are taken from Figure 1 of Olson et al 2001.
#'        \url{https://worldwildlife.org/biomes} contains a more inclusive
#'        description of water ecoregions. \code{LAB} is used to denote 
#'        studies that occurred in a lab or greenhouse setting. Possible values
#'        are as follows:
#'        \itemize{
#'          \item{TMB}{ - Terrestrial tropical and subtropical moist broadleaf forest}
#'          \item{TDB}{ - Terrestrial tropical and subtropical dry broadleaf forest}
#'          \item{TSC}{ - Terrestrial tropical and subtropical coniferous forests}
#'          \item{TBM}{ - Terrestrial temperate broadleaf and mixed forests}
#'          \item{TCF}{ - Terrestrial temperate coniferous forests}
#'          \item{BOR}{ - Terrestrial boreal forests/taiga}
#'          \item{TGV}{ - Terrestrial tropical and subtropical grassland, savanna, shrubland}
#'          \item{TGS}{ - Terrestrial temperate grassland, savanna, shrubland}
#'          \item{FGS}{ - Terrestrial flooded grassland and savanna}
#'          \item{MON}{ - Terrestrial montane grassland and shrubland}
#'          \item{TUN}{ - Terrestrial tundra}
#'          \item{MED}{ - Terrestrial Mediterranean forests, woodlands, and scrubs}
#'          \item{DES}{ - Terrestrial deserts and xeric shrublands}
#'          \item{MAN}{ - Terrestrial mangroves}
#'          \item{LRE}{ - Freshwater large river ecosystems}
#'          \item{LRH}{ - Freshwater large river headwater ecosystems}
#'          \item{LRD}{ - Freshwater large river delta ecosystems}
#'          \item{SRE}{ - Freshwater small river ecosystems}
#'          \item{SLE}{ - Freshwater small lake ecosystems}
#'          \item{LLE}{ - Freshwater large lake ecosystems}
#'          \item{XBE}{ - Freshwater xeric basin ecosystems}
#'          \item{POE}{ - Marine polar ecosystems}
#'          \item{TSS}{ - Marine temperate shelf and seas ecosystems}
#'          \item{TEU}{ - Marine temperate upwellings}
#'          \item{TRU}{ - Marine tropical upwellings}
#'          \item{TRC}{ - Marine tropical coral}
#'            
#'        }
#'        }
#'        \item{StudiedSex}{factor - Possible values are \code{M} (studied
#'        only males), \code{F} (studied only females), \code{H} (studied
#'        only hermaphrodites), \code{M/F} (males and females separately in
#'        the same matrix model), and \code{A} (all sexes studied together)}
#'        \item{MatrixComposite}{factor - Indicates the type of matrix
#'        population model. Possible values are \code{Individual},
#'        \code{Mean}, \code{Pooled}, and \code{Seasonal}. See Figure 2
#'        of the COMPADRE user guide 
#'        (\url{https://github.com/jonesor/compadreDB/blob/master/COMPADRE-UserGuide/COMPADRE-UserGuide.Rmd})
#'        for more information}
#'        \item{MatrixTreatment}{character - Describes if a treatment was
#'        applied to the population or not. If yes, includes a brief
#'        description of the treatment. If not, \code{Unmanipulated}}
#'        \item{MatrixCaptivity}{factor - Whether the species was studied
#'        in the wild or under captivity. Possible values are \code{W} (
#'        wild), \code{C} (captive), or \code{CW} (captured from a wild
#'        population)}
#'        \item{MatrixStartYear}{numeric - First year of matrix}
#'        \item{MatrixStartSeason}{factor - First season in the matrix as 
#'        described by the author. Seasons are hemisphere specific. 
#'        possible values are \code{1} (spring), \code{2} (summer),
#'        \code{3} (fall), and \code{4} (winter)}
#'        \item{MatrixStartMonth}{numeric - First month of the matrix}
#'        \item{MatrixEndYear}{numeric - Final year of the matrix}
#'        \item{MatrixEndSeason}{factor - Final season of the matrix. Codes
#'        are the same as \code{MatrixStartSeason}}
#'        \item{MatrixEndMonth}{numeric - Final month of the matrix}
#'        \item{MatrixSplit}{factor - Indicates whether the \strong{A}
#'        matrix has been split into its constituent \strong{U},
#'        \strong{F}, and \strong{C} matrices. \code{Divided} indicates
#'        that it has been split, whereas \code{Indivisible} indicates 
#'        that there is insufficient information to divide the matrix.
#'        When this happens, the \strong{A} matrix is reported and
#'        the others are filled with \code{NA}s}
#'        \item{MatrixFec}{factor - Records whether fecundity was measured
#'        at all in the matrix model. Fecundity may be recorded as 0 because
#'        it was unmeasured or because it was measured and estimated to be 0}
#'        \item{Observation}{factor - Additional observations recorded
#'        by data base compilers that may be useful, but is not suited
#'        for other columns in the metadata data frame}
#'        \item{MatrixDimension}{numeric - Dimension of the \strong{A} matrix}
#'        \item{SurvivalIssue}{numeric - Denotes the minimum stage specific
#'        survival value when the \strong{U} matrix is > 1}
#'    }
#'    
#'  \item{\strong{mat}}{ - A list of \code{CompadreMat}'s. These contain the \strong{A}
#'  matrices, the submatrices (\strong{U}, \strong{F}, and \strong{C}),
#'  and the \code{matrixClass} data frame. \code{matrixClass} slot contains
#'  the following information -}
#'  \describe{
#'    \item{MatrixClassOrganized}{character - Standardized stages for a matrix
#'    population model. These can be \code{prop} (propagule), \code{active} (
#'    not a propagule or dormant stage), or \code{dorm} (dormant)}
#'    \item{MatrixClassAuthor}{character - The stage descriptions from the
#'     publication}
#'    \item{MatrixClassNumber}{A numerical representation for the matrix
#'    stages}
#'  }
#'  
#'  \item{\strong{version}}{ - A list containing summary information on the version
#'  of \code{COMPADRE}. It contains the following information - }
#'  \describe{
#'    \item{Version}{The version number of the data base(Major_Minor_Patch)}
#'    \item{DateCreated}{The date that the \code{.rda} was created. Automatically
#'    updates when \code{subsetDB} or \code{mergeDBs} is used.}
#'    \item{NumberAcceptedSpecies}{The number of accepted species names
#'    in this version or subset of the data base.}
#'    \item{NumberStudies}{The number of studies in this version or subset of
#'    the data base}
#'    \item{NumberMatrices}{The number of matrices in this version or subset of
#'    the data base}
#'    \item{Agreement}{Link to the \code{COMPADRE} license agreement}
#'  }
#' }
"Compadre"  


#' COMADRE Animal demography data base
#' 
#' A \code{CompadreDB} object consisting of \code{metadata},
#' \code{mat}, and \code{version}
#' 
#' @format 
#' \itemize{
#'   \item{\strong{metadata}}{ - A \code{data.frame} with 130 rows and 47 columns}
#'     \describe{
#'       \item{SpeciesAuthor}{character - Binomial species name
#'        given by the paper author}
#'        \item{SpeciesAccepted}{character - Accepted binomial species name 
#'        taken from \emph{The Plant List}}
#'        \item{CommonName}{character - The common name used in the publication}
#'        \item{ColCheckOK}{logical - Whether the taxonomy detailed here has been verified
#'        on the \emph{Catalogue of Life}. See 
#'        \url{http://www.catalogueoflife.org/} for more details}
#'        \item{ColCheckDate}{character - The date that the taxonomy was checkd on the 
#'        \emph{Catalogue of Life}}
#'        \item{Infraspecific}{character - Taxonomic infraspecific name of the
#'        study's \code{SpeciesAuthor}}
#'        \item{SpeciesEpithetAccepted}{character - Taxonomic species epithet
#'        of \code{SpeciesAccepted}, per the \emph{Catalogue of Life}}
#'        \item{GenusAccepted}{character - Taxonomic genus name of 
#'        \code{SpeciesAccepted}, per the \emph{Catalogue of Life}}
#'        \code{GenusAuthor}{chracter - Genus used in \code{SpeciesAuthor}}
#'        \item{Family}{factor - Family which the species belongs}
#'        \item{Order}{factor - Order to which the species belongs}
#'        \item{Class}{factor - Class to which the species belongs}
#'        \item{Phylum}{factor - Phylum to which the species belongs}
#'        \item{Kingdom}{factor - Kingdom to which the species belongs}
#'        \item{OrganismType}{factor - Broad type of organism. Generall, this is 
#'        \code{Class} in animals (except for humans). Non-animals included
#'        in \code{COMADRE} include \code{Bacteria} and \code{Virus}}
#'        \item{Authors}{characer - Last (family) name of all authors 
#'        separated with a ";"}
#'        \item{Journal}{character - The type of document from which the
#'        data were sourced. Possible values are \code{abbreviated
#'        journal name}, \code{Book}, \code{PhD thesis}, \code{MSc thesis},
#'        \code{Report}, \code{Conference talk}, and \code{Conference poster}}
#'        \item{YearPublication}{character - The year of publication}
#'        \item{DOI.ISBN}{character - The Digitial Object Identifier or 
#'        International Standard Book Number codes to identify each 
#'        publication.}
#'        \item{AdditionalSource}{character - Additional source(s) used to 
#'        reconstruct the matrix or obtain additional metadata for the matrix
#'        (if applicable)}
#'        \item{StudyDuration}{numeric - The number of years of observation
#'        for the study. This is calculated as \code{StudyEnd} - 
#'        \code{StudyStart}} 
#'        \item{StudyStart}{numeric - The year that the study began}
#'        \item{StudyEnd}{numeric - The year that the study ended}
#'        \item{AnnualPeriodicty}{numeric - The time frame for which seasonal
#'        or annual MPMs are constructed. A value of \code{1} indicates that
#'        the iteration period is 1 year, \code{2} indicates iterations twice
#'        per year, \code{0.2} indicates every 5 years, etc.}
#'        \item{NumberPopulations}{numeric - The number of study populations
#'        as defined by the authors. Within site replication of permanent 
#'        plots is not defined as separate populations}
#'        \item{MatrixCriteriaSize}{factor - Indicates whether the matrix
#'        contains stages based on size. If so, indicates what that measure
#'        of size is}
#'        \item{MatrixCriteriaOntogeny}{factor - Indicates whether the matrix
#'        contains stages based on ontogenic/developmental stages}
#'        \item{MatrixCriteriaAge}{factor - Indicates whether the matrix 
#'        contains stages based on age}
#'        \item{MatrixPopulation}{character - Definition of where the matrix
#'        was recorded, usually given by the author. In some cases, 
#'        where the author provides no name, we give the closest geographic 
#'        location instead. If there are multiple populations in the study 
#'        and their names are not pertinent/available, sequential names in 
#'        alphabetical order are assigned for each population in the study
#'        (e.g. \code{A}, \code{B}, \code{C}, etc)}
#'        \item{Lat}{numeric - The latitude in decimal format. Negative
#'        values indicate the site is in the southern hemisphere}
#'        \item{Lon}{numeric - The longitude in decimal format. Negative
#'        values indicate the site is west of the Greenwich Meridian}
#'        \item{Altitude}{numeric - Altitude of the studied population. Units
#'        are meters above sea level}
#'        \item{Country}{factor - The country where the study took place.
#'        3-letter ISO country codes are used
#'        (see \url{https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3} for
#'        codes). Studies taking place in multiple countries have each 
#'        country separated by a \code{;}}
#'        \item{Continent}{factor - The continent where the study took place.
#'        possible entries are \code{Africa}, \code{Asia}, \code{Europe},
#'        \code{N America} (USA, Canada, and Mexico), \code{S America} (everything
#'        in the Americas except USA, Canada, and Mexico), and \code{Oceania}.
#'        Various definitions of Oceania exist - we have opted for this one:
#'        \url{http://en.wikipedia.org/wiki/List_of_Oceanian_countries_by_population}}
#'        \item{Ecoregion}{character - Indicates the ecoregion the study
#'        occurred in. These are taken from Figure 1 of Olson et al 2001.
#'        \url{https://worldwildlife.org/biomes} contains a more inclusive
#'        description of water ecoregions. \code{LAB} is used to denote 
#'        studies that occurred in a lab or greenhouse setting. Possible values
#'        are as follows:
#'        \itemize{
#'          \item{TMB}{ - Terrestrial tropical and subtropical moist broadleaf forest}
#'          \item{TDB}{ - Terrestrial tropical and subtropical dry broadleaf forest}
#'          \item{TSC}{ - Terrestrial tropical and subtropical coniferous forests}
#'          \item{TBM}{ - Terrestrial temperate broadleaf and mixed forests}
#'          \item{TCF}{ - Terrestrial temperate coniferous forests}
#'          \item{BOR}{ - Terrestrial boreal forests/taiga}
#'          \item{TGV}{ - Terrestrial tropical and subtropical grassland, savanna, shrubland}
#'          \item{TGS}{ - Terrestrial temperate grassland, savanna, shrubland}
#'          \item{FGS}{ - Terrestrial flooded grassland and savanna}
#'          \item{MON}{ - Terrestrial montane grassland and shrubland}
#'          \item{TUN}{ - Terrestrial tundra}
#'          \item{MED}{ - Terrestrial Mediterranean forests, woodlands, and scrubs}
#'          \item{DES}{ - Terrestrial deserts and xeric shrublands}
#'          \item{MAN}{ - Terrestrial mangroves}
#'          \item{LRE}{ - Freshwater large river ecosystems}
#'          \item{LRH}{ - Freshwater large river headwater ecosystems}
#'          \item{LRD}{ - Freshwater large river delta ecosystems}
#'          \item{SRE}{ - Freshwater small river ecosystems}
#'          \item{SLE}{ - Freshwater small lake ecosystems}
#'          \item{LLE}{ - Freshwater large lake ecosystems}
#'          \item{XBE}{ - Freshwater xeric basin ecosystems}
#'          \item{POE}{ - Marine polar ecosystems}
#'          \item{TSS}{ - Marine temperate shelf and seas ecosystems}
#'          \item{TEU}{ - Marine temperate upwellings}
#'          \item{TRU}{ - Marine tropical upwellings}
#'          \item{TRC}{ - Marine tropical coral}
#'            
#'        }
#'        }
#'        \item{StudiedSex}{factor - Possible values are \code{M} (studied
#'        only males), \code{F} (studied only females), \code{H} (studied
#'        only hermaphrodites), \code{M/F} (males and females separately in
#'        the same matrix model), \code{A} (all sexes studied together),
#'        or \code{NA} (The species has no distinct sexes)}
#'        \item{MatrixComposite}{factor - Indicates the type of matrix
#'        population model. Possible values are \code{Individual},
#'        \code{Mean}, \code{Pooled}, and \code{Seasonal}. See Figure 2
#'        of the COMADRE user guide 
#'        (\url{https://github.com/jonesor/compadreDB/blob/master/COMADRE-UserGuide/COMADRE-UserGuide.pdf})
#'        for more information}
#'        \item{MatrixTreatment}{character - Describes if a treatment was
#'        applied to the population or not. If yes, includes a brief
#'        description of the treatment. If not, \code{Unmanipulated}}
#'        \item{MatrixCaptivity}{factor - Whether the species was studied
#'        in the wild or under captivity. Possible values are \code{W} (
#'        wild), \code{C} (captive), or \code{CW} (captured from a wild
#'        population)}
#'        \item{MatrixStartYear}{numeric - First year of matrix}
#'        \item{MatrixStartSeason}{factor - First season in the matrix as 
#'        described by the author. Seasons are hemisphere specific. 
#'        possible values are \code{1} (spring), \code{2} (summer),
#'        \code{3} (fall), and \code{4} (winter)}
#'        \item{MatrixStartMonth}{numeric - First month of the matrix}
#'        \item{MatrixEndYear}{numeric - Final year of the matrix}
#'        \item{MatrixEndSeason}{factor - Final season of the matrix. Codes
#'        are the same as \code{MatrixStartSeason}}
#'        \item{MatrixEndMonth}{numeric - Final month of the matrix}
#'        \item{MatrixSplit}{factor - Indicates whether the \strong{A}
#'        matrix has been split into its constituent \strong{U},
#'        \strong{F}, and \strong{C} matrices. \code{Divided} indicates
#'        that it has been split, whereas \code{Indivisible} indicates 
#'        that there is insufficient information to divide the matrix.
#'        When this happens, the \strong{A} matrix is reported and
#'        the others are filled with \code{NA}s}
#'        \item{MatrixFec}{factor - Records whether fecundity was measured
#'        at all in the matrix model. Fecundity may be recorded as 0 because
#'        it was unmeasured or because it was measured and estimated to be 0}
#'        \item{Observation}{factor - Additional observations recorded
#'        by data base compilers that may be useful, but is not suited
#'        for other columns in the metadata data frame}
#'        \item{MatrixDimension}{numeric - Dimension of the \strong{A} matrix}
#'        \item{SurvivalIssue}{numeric - Denotes the minimum stage specific
#'        survival value when the \strong{U} matrix is > 1}
#'    }
#'    
#'  \item{\strong{mat}}{ - A list of \code{CompadreM}'s. These contain the \strong{A}
#'  matrices, the submatrices (\strong{U}, \strong{F}, and \strong{C}),
#'  and the \code{matrixClass} data frame. \code{matrixClass} slot contains
#'  the following information -}
#'  \describe{
#'    \item{MatrixClassOrganized}{character - Standardized stages for a matrix
#'    population model. These can be \code{prop} (propagule), \code{active} (
#'    not a propagule or dormant stage), or \code{dorm} (dormant)}
#'    \item{MatrixClassAuthor}{character - The stage descriptions from the
#'     publication}
#'    \item{MatrixClassNumber}{A numerical representation for the matrix
#'    stages}
#'  }
#'  
#'  \item{\strong{version}}{ - A list containing summary information on the version
#'  of \code{COMADRE}. It contains the following information - }
#'  \describe{
#'    \item{Version}{The version number of the data base(Major_Minor_Patch)}
#'    \item{DateCreated}{The date that the \code{.rda} was created. Automatically
#'    updates when \code{subsetDB} or \code{mergeDBs} is used.}
#'    \item{NumberAcceptedSpecies}{The number of accepted species names
#'    in this version or subset of the data base.}
#'    \item{NumberStudies}{The number of studies in this version or subset of
#'    the data base}
#'    \item{NumberMatrices}{The number of matrices in this version or subset of
#'    the data base}
#'    \item{Agreement}{Link to the \code{COMADRE} license agreement}
#'  }
#' }
"Comadre"  