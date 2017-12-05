# Class CompadreData, definition and methods

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

setClass("CompadreData",
         slots = c(
             metadata = "data.frame",
             matA = "list",
             ## matU = "list",
             ## matF = "list",
             ## matC = "list",
             version = "list"
             )
         )

## ---------------------------------------------------------------------
## define a method for initialize

setMethod("initialize", "CompadreData",
    function(.Object, ...) {
        .Object <- callNextMethod()
        validObject(.Object)
        .Object
    })

## ---------------------------------------------------------------------
## define validity check function
validCompadreData <- function(object) {
    errors <- character()
    if (nrow(object@metadata) != length(object@matA)) {
        msg <- paste0("Unequal metadata and matA lengths:",
                      nrow(object@metadata), ", ",
                      length(object@matA))
        errors <- c(errors, msg)
    }
    if (length(errors) == 0) {
        TRUE
    } else {
        errors
    }
}
setValidity("CompadreData", validCompadreData)


## ---------------------------------------------------------------------
## define method to coerce old compadre db object to CompadreData class
setAs("list", "CompadreData", function(from) asCompadreData(from))

asCompadreData <- function(from) {
    ## Need to check that 'from' is a old style compadre db object - by checking
    ## it has the expected structure?
    new("CompadreData",
        metadata = from$metadata,
        matA = lapply(from$mat, `[[`, "matA"),
        version = from$version)
}
