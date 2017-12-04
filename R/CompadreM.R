# Class CompadreM, definition and methods

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

setClass("CompadreM",
         slots = c(
           matA = "matrix",
           matU = "matrix",
           matF = "matrix",
           matC = "matrix",
           matrixClass = "data.frame"
         )
)

## ---------------------------------------------------------------------
## define a method for initialize

setMethod("initialize", "CompadreM", 
          function(.Object, ...) {
            .Object <- callNextMethod()        
            validObject(.Object)
            .Object
          })

## ---------------------------------------------------------------------
## define validity check function
validCompadreMObject <- function(object) {
  errors <- character()
  #matA
  if (any(object@matA < 0, na.rm = T) ) {
    matAmsg1 <- "matA is not nonnegative"
    errors <- c(errors, matAmsg1)
  }
  if (diff(dim(object@matA)) != 0 ) {
    matAmsg2 <- "matA is not square"
    errors <- c(errors, matAmsg2)
  }
  #matU
  if (any(object@matU < 0, na.rm = T) ) {
    matUmsg1 <- "matU is not nonnegative"
    errors <- c(errors, matUmsg1)
  }
  if (diff(dim(object@matU)) != 0 ) {
    matUmsg2 <- "matU is not square"
    errors <- c(errors, matUmsg2)
  }
  #matF
  if (any(object@matF < 0, na.rm = T) ) {
    matFmsg1 <- "matF is not nonnegative"
    errors <- c(errors, matFmsg1)
  }
  if (diff(dim(object@matF)) != 0 ) {
    matFmsg2 <- "matF is not square"
    errors <- c(errors, matFmsg2)
  }
  #matC
  if (any(object@matC < 0, na.rm = T) ) {
    matCmsg1 <- "matC is not nonnegative"
    errors <- c(errors, matCmsg1)
  }
  if (diff(dim(object@matC)) != 0 ) {
    matCmsg2 <- "matC is not square"
    errors <- c(errors, matCmsg2)
  }
  #dimensions
  dims <- c(dim(object@matA)[1], 
            dim(object@matU)[1],
            dim(object@matF)[1],
            dim(object@matC)[1])
  if(diff(range(dims)) != 0) {
    dimsmsg <- "matA, matU, matF and matC do not have equal dimensions"
  }
  if(diff(range(dims)) == 0) {
    dim <- dim(object@matA)[1]
  }
  #matrixClass
  matrixClassNames <- c("MatrixClassOrganized", "MatrixClassAuthor", "MatrixClassNumber")
  mCNpresent <- matrixClassNames%in%names(object@matrixClass)
  if(!all(mCNpresent)) {
    mCNmissing <- paste0(matrixClassNames[!mCNpresent], collapse = " & ")
    mCNmsg1 <- paste0(mCNmissing, "from matrixClassNames", collapse = " ")
    errors <- c(errors, mCNmsg1)
  }
  if(dim(object@matrixClass)[1] != dim) {
    mCNmsg2 <- "number of rows in matrixClass does not equal matrix dimension"
    errors <- c(errors, mCNmsg2)
  }
  if (length(errors) == 0) {
    TRUE
  } else {
    errors
  }
}
setValidity("CompadreM", validCompadreMObject)

