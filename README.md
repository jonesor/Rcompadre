[![Build Status](https://travis-ci.org/jonesor/Rcompadre.svg?branch=master)](https://travis-ci.org/jonesor/Rcompadre)

[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/jonesor/Rcompadre?branch=master&svg=true)](https://ci.appveyor.com/project/jonesor/Rcompadre)

Rcompadre
==========

Note that this package is at an early stage of development. 
Expect bugs!

*How to install:*

`install.packages("devtools")`

`library(devtools)`

`install_github("jonesor/Rcompadre")`

*To install the development branch*

`install_github("jonesor/Rcompadre",ref = "devel")`

The development branch is named "RcompadreDev". This difference in names allows one to install both `master` and `devel` branches and call functions from the respective versions as follows:

`Rcompadre::subsetDB(mydatabase)`
`RcompadreDev::subsetDB(mydatabase)`

*Report bugs/errors to:*

Rob Salguero-Gomez (rob.salguero@zoo.ox.ac.uk)

Owen Jones (jones@biology.sdu.dk)
