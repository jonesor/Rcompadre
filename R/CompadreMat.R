# Class CompadreMat, definition and methods

# Copyright (c) 2017 Tamora D. James and Iain M. Stott

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

################################################################################
#' Methods for working with matrices in com(p)adre
#'
#' This page describes methods for accessing any matrix information from
#' CompadreMat and CompadreDB objects.
#'
#' @name CompadreMatrixMethods
#' @slot matA A matrix population model (i.e. a square projection matrix)
#' @slot matU The survival component of a matrix population model (i.e. a square
#'   projection matrix reflecting survival-related transitions; e.g.
#'   progression, stasis, and retrogression)
#' @slot matF The sexual component of a matrix population model (i.e. a square
#'   projection matrix reflecting transitions due to sexual reproduction)
#' @slot matC The clonal component of a matrix population model (i.e. a square
#'   projection matrix reflecting transitions due to clonal reproduction)
#' @slot matrixClass A data frame with columns \code{MatrixClassOrganized}
#'   (elements are "active", "prop", or "dorm") \code{MatrixClassAuthor} (the
#'   matrix author's stage description), and \code{MatrixClassNumber} (integer
#'   stage number)
setClass("CompadreMat",
  slots = c(
    matA = "matrix",
    matU = "matrix",
    matF = "matrix",
    matC = "matrix",
    matrixClass = "data.frame"
  )
)

################################################################################
## Initialize & check

## define a method for initialize (does not need to be documented)
#' @importFrom methods callNextMethod validObject
setMethod(
  "initialize", "CompadreMat",
  function(.Object, ...) {
    .Object <- methods::callNextMethod()
    methods::validObject(.Object)
    .Object
  }
)

## ---------------------------------------------------------------------
## define validity check function (does not need to be documented)
validCompadreMat <- function(object) {
  errors <- character()

  # test matrix dimensions
  dims <- cbind(
    dim(object@matA),
    dim(object@matU),
    dim(object@matF),
    dim(object@matC)
  )

  n_row <- dims[1, ]
  n_col <- dims[2, ]

  check_square <- n_row == n_col
  check_equal_dims <- all(n_row == n_row[1]) & all(n_col == n_col[1])

  if (!all(check_square)) {
    m <- c("matA", "matU", "matF", "matC")
    errors <- c(errors, paste(
      "The following matrices are not square:",
      paste(m[!check_square], collapse = ", ")
    ))
  }
  if (!check_equal_dims) {
    errors <- c(errors, "Dimensions of matA, matU, matF and matC do not match")
  }

  # test that matrixClass has required column names
  m_names <- c("MatrixClassOrganized", "MatrixClassAuthor", "MatrixClassNumber")
  m_names_present <- m_names %in% names(object@matrixClass)

  if (!all(m_names_present)) {
    errors <- c(
      errors,
      paste(
        "The following columns are missing from matrixClass:",
        paste(m_names[!m_names_present], collapse = ", ")
      )
    )
  }

  # test that the row dimension of matrixClass matches matA
  if (nrow(object@matrixClass) != dims[1, 1]) {
    errors <- c(
      errors,
      "Number of rows in matrixClass does not match nrow(matA)"
    )
  }
  if (length(errors) == 0) {
    TRUE
  } else {
    errors
  }
}

setValidity("CompadreMat", validCompadreMat)


## -----------------------------------------------------------------------------
## define a method for showing the object (does not need to be documented)
# show
setMethod("show",
  signature = (object <- "CompadreMat"),
  function(object) {
    Mdim <- dim(matA(object))[1]
    # start
    start <- cat(paste(
      "A compadre matrix object with",
      as.character(Mdim),
      "stages.\n\n"
    ))
    if (Mdim > 0) {
      # matrixClass info
      showstages <- matrixClass(object)[, c("MatrixClassOrganized", "MatrixClassAuthor")]
      print(showstages)
      cat("\n")
      # matA
      cat("matA:\n")
      showmatA <- matA(object)
      dimnames(showmatA) <- list(1:Mdim, 1:Mdim)
      print(showmatA, nsmall = 3)
      cat("\n")
      cat("matU:\n")
      showmatU <- matU(object)
      dimnames(showmatU) <- list(1:Mdim, 1:Mdim)
      print(showmatU)
      cat("\n")
      cat("matF:\n")
      showmatF <- matF(object)
      dimnames(showmatF) <- list(1:Mdim, 1:Mdim)
      print(showmatF)
      cat("\n")
      cat("matC:\n")
      showmatC <- matC(object)
      dimnames(showmatC) <- list(1:Mdim, 1:Mdim)
      print(showmatC)
      cat("\n")
    }
  }
)
