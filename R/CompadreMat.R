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
setMethod("initialize", "CompadreMat",
          function(.Object, ...) {
            .Object <- methods::callNextMethod()
            if(length(.Object@matrixClass) == 0) {
              .Object@matrixClass <- data.frame(MatrixClassOrganized = character(0),
                                                MatrixClassAuthor = character(0),
                                                MatrixClassNumber = double(0)
              )
            }
            methods::validObject(.Object)
            .Object
          })

## ---------------------------------------------------------------------
## define validity check function (does not need to be documented)
validCompadreMat <- function(object) {
  errors <- character()
  ###matrices
  ##test dimensions
  dims <- cbind(matA = dim(matA(object)),
                matU = dim(matU(object)),
                matF = dim(matF(object)),
                matC = dim(matC(object)))
  dimsMeans <- colMeans(dims)
  dimsDiff <- diff(range(dimsMeans))
  #matA
  if (diff(dims[,"matA"]) != 0 ) {
    matAmsg2 <- "matA is not square"
    errors <- c(errors, matAmsg2)
  }
  #matU
  if (diff(dims[,"matU"]) != 0 ) {
    matUmsg2 <- "matU is not square"
    errors <- c(errors, matUmsg2)
  }
  #matF
  if (diff(dims[,"matF"]) != 0 ) {
    matFmsg2 <- "matF is not square"
    errors <- c(errors, matFmsg2)
  }
  #matC
  if (diff(dims[,"matC"]) != 0 ) {
    matCmsg2 <- "matC is not square"
    errors <- c(errors, matCmsg2)
  }
  #equal dimensions among all matrices
  if(dimsDiff != 0) {
    dimsmsg <- "matA, matU, matF and matC dimensions do not match"
    errors <- c(errors, dimsmsg)
  }
  ###matrixClass
  ##test that necessary column names are there
  matrixClassNames <- c("MatrixClassOrganized", "MatrixClassAuthor", "MatrixClassNumber")
  mCNpresent <- matrixClassNames %in% names(matrixClass(object))
  if(!all(mCNpresent)) {
    mCNmissing <- paste(matrixClassNames[!mCNpresent], collapse = " & ")
    mCNmsg1 <- paste(mCNmissing, "missing from matrixClassNames")
    errors <- c(errors, mCNmsg1)
  }
  ##test that the row dimension matches the matrices,
  ##IF the matrices all have equal dimension.
  if(dimsDiff == 0 & !all(dim(matrixClass(object))[1] == dimsMeans)) {
    mCNmsg2 <- "number of rows in matrixClass does not match dimensions of matA, matU, matF and matC"
    errors <- c(errors, mCNmsg2)
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

setMethod("show", signature = (object ="CompadreMat"),
          function (object){
            Mdim <- dim(matA(object))[1]
            #start
            start <- cat(paste("A compadre matrix object with",
                               as.character(Mdim),
                               "stages.\n\n"
                      ))
            if(Mdim > 0){
              #matrixClass info
              showstages <- as.data.frame(matrixClass(object))[,c("MatrixClassAuthor","MatrixClassNumber")]
              dimnames(showstages)[[2]] <- c("Stage name", "Stage number")
              print(showstages, row.names = FALSE)
              cat("\n")
              #matA
              cat("matA:\n")
              showmatA <- matA(object)
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
