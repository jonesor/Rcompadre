
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Rcompadre <img src="man/figures/logo_rcompadre.png" height="160px" align="right" />

<!--- Continuous integration badges --->

<!--- BE CAREFUL WITH THE FORMATTING --->

| Project                                                                                                                                                                                                | Main branch                                                                                                                                                                | Devel branch                                                                                                                                                                             |
| ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | :------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | :--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| [![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.html)                                                                                | [![R-CMD-check](https://github.com/jonesor/Rcompadre/actions/workflows/main_check.yaml/badge.svg)](https://github.com/jonesor/Rcompadre/actions/workflows/main_check.yaml) | [![R-CMD-check](https://github.com/jonesor/Rcompadre/actions/workflows/devel_build_check.yaml/badge.svg)](https://github.com/jonesor/Rcompadre/actions/workflows/devel_build_check.yaml) |
| [![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active) | [![codecov](https://codecov.io/gh/jonesor/Rcompadre/branch/main/graph/badge.svg?token=S9G2a5K9k9)](https://app.codecov.io/gh/jonesor/Rcompadre)                            |                                                                                                                                                                                          |
| ![](http://cranlogs.r-pkg.org/badges/grand-total/Rcompadre)                                                                                                                                            | [![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/Rcompadre)](https://cran.r-project.org/package=Rcompadre)                                                     |                                                                                                                                                                                          |
| ![](http://cranlogs.r-pkg.org/badges/Rcompadre)                                                                                                                                                        |                                                                                                                                                                            |                                                                                                                                                                                          |

<!--- Continuous Integration Badges End --->

An R package to facilitate working with the
[COM(P)ADRE](https://compadre-db.org/) Plant and Animal Matrix
Population Databases.

## Installation

Install the stable release package from CRAN with:

``` r
install.packages("Rcompadre")
```

Install the package from GitHub with:

``` r
# install package 'remotes' if necessary
# will already be installed if 'devtools' is installed
install.packages("remotes")

# argument 'build_opts = NULL' only needed if you want to build vignettes
remotes::install_github("jonesor/Rcompadre", build_opts = NULL)
```

To install from the development branch use:

``` r
remotes::install_github("jonesor/Rcompadre", ref = "devel")
```

## Usage

``` r
library(Rcompadre)
```

#### Fetching a database

Fetch the most recent database version from
[compadre-db.org](https://compadre-db.org/) with

``` r
compadre <- cdb_fetch("compadre") # or use 'comadre' for the animal database
```

or load from a local `.RData` file with

``` r
compadre <- cdb_fetch("path/to/file/COMPADRE_v.4.0.1.RData")
```

If you prefer using `load()` to load your local copy of a legacy
database, use `as_cdb()` to convert it to the `CompadreDB` class

``` r
load("COMPADRE_v.4.0.1.RData") # loads object 'compadre'
compadre <- as_cdb(compadre)
```

#### Subsetting

For the most part `CompadreDB` objects work like a data frame. They can
be subset using `[`, `subset()` or `filter()`

``` r
# subset to the first 10 rows
compadre[1:10, ]

# subset to the species 'Echinacea angustifolia'
subset(compadre, SpeciesAccepted == "Echinacea angustifolia")

# subset to the species 'Echinacea angustifolia'
filter(compadre, SpeciesAccepted == "Echinacea angustifolia")
```

#### Example analysis: calculating population growth rates

First we’ll use the function `cdb_flag()` to add columns to the database
flagging potential issues with the projection matrices, such as missing
values, or matrices that don’t meet assumptions like ergodicity,
irreducibility, or primitivity.

``` r
compadre_flags <- cdb_flag(compadre)
```

We’ll only be able to calculate population growth rates from matrices
that don’t contain missing values, and we only want to use matrices that
meet the assumption of ergodicity, so we’ll subset the database
accordingly.

``` r
compadre_sub <- subset(
  compadre_flags,
  check_NA_A == FALSE & check_ergodic == TRUE
)
```

Finally, we’ll use the `eigs()` function from the
[popdemo](https://CRAN.R-project.org/package=popdemo) package to
calculate the population growth rate for every matrix in `compadre_sub`.

``` r
compadre_sub$lambda <- sapply(matA(compadre_sub), popbio::eigs, what = "lambda")
```

In the code above, the accessor function `matA()` is used to extract a
list of projection matrices (the full matrix, `matA`) from every row of
`compadre_sub`. There are also accessor functions for the matrix
subcomponents (`matU()`, `matF()`, `matC()`), and for many other parts
of the database too.

## Previous releases

Specific earlier releases of this package can be installed using the
appropriate `@` tag.

For example to install version 0.1.0:

``` r
remotes::install_github("jonesor/Rcompadre@v0.1.0")
```

See the Changelog for more details.

## Citation

Jones, Owen R., Patrick Barks, Iain M. Stott, Tamora D. James, Sam C.
Levin, William K. Petry, Pol Capdevila, et al. 2021. “Rcompadre and Rage
– Two R Packages to Facilitate the Use of the COMPADRE and COMADRE
Databases and Calculation of Life History Traits from Matrix Population
Models.” *bioRxiv*. doi:
[10.1101/2021.04.26.441330](https://doi.org/10.1101/2021.04.26.441330).

## Contributions

All contributions are welcome. Please note that this project is released
with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By participating in this project you agree to abide by its terms.

There are numerous ways of contributing.

1.  You can submit bug reports, suggestions etc. by [opening an
    issue](https://github.com/jonesor/Rcompadre/issues).

2.  You can copy or fork the repository, make your own code edits and
    then send us a pull request. [Here’s how to do
    that](https://jarv.is/notes/how-to-pull-request-fork-github/).

3.  You can get to know us and join as a collaborator on the main
    repository.

4.  You are also welcome to email us.
