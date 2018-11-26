
# Script that creates the subsetted versions of the COM(P)ADRE
# data bases distributed in RCompadre


# load RcompadreDev
devtools::load_all()

# load dbs
load('~/COMADRE_v.X.X.X.RData')
load('~/COMPADRE_v.X.X.X.RData')

# convert to CompadreDB
Compadre <- asCompadreDB(compadre)
Comadre <- asCompadreDB(comadre)

# # create reproducible random subsamples
# s1 <- sample(seq_along(Compadre$SpeciesAccepted), 150)
# s2 <- sample(seq_along(Comadre$SpeciesAccepted), 150)
# 
# dput(s1)
# dput(s2)
s1 <- c(4910L, 3337L, 6743L, 2664L, 2350L, 4193L, 5083L, 2809L, 1207L, 
        4149L, 4008L, 1153L, 450L, 5139L, 1172L, 7139L, 2971L, 6655L, 
        188L, 5453L, 6052L, 2362L, 3796L, 376L, 6560L, 1353L, 3509L, 
        267L, 5255L, 2873L, 5546L, 5387L, 1913L, 1845L, 1417L, 3221L, 
        2466L, 4148L, 951L, 4517L, 4379L, 3601L, 5719L, 4846L, 748L, 
        1249L, 194L, 5492L, 2804L, 7148L, 1486L, 212L, 1789L, 4999L, 
        3172L, 5498L, 3411L, 1846L, 1003L, 3399L, 694L, 104L, 5571L, 
        5224L, 6711L, 2312L, 583L, 6244L, 5512L, 1653L, 6875L, 861L, 
        4191L, 3442L, 3681L, 1765L, 239L, 1844L, 3398L, 6658L, 6178L, 
        2844L, 5905L, 38L, 1488L, 5284L, 1043L, 2890L, 6969L, 1007L, 
        1116L, 7071L, 2216L, 3665L, 1163L, 584L, 3786L, 117L, 1034L, 
        6045L, 3434L, 2291L, 203L, 7130L, 3974L, 1083L, 5296L, 5784L, 
        5111L, 3684L, 3456L, 5028L, 3664L, 6382L, 1428L, 1694L, 2919L, 
        379L, 6243L, 5944L, 1149L, 2800L, 5382L, 208L, 6723L, 3733L, 
        1858L, 7191L, 191L, 4528L, 63L, 1906L, 2867L, 2300L, 5610L, 1495L, 
        1928L, 7277L, 2537L, 4195L, 3806L, 1065L, 2876L, 225L, 5607L, 
        883L, 2718L, 1085L, 151L, 2304L)

s2 <- c(302L, 1903L, 1123L, 11L, 547L, 1492L, 1233L, 1621L, 885L, 914L, 
        2180L, 1046L, 451L, 678L, 960L, 213L, 507L, 176L, 1239L, 2097L, 
        654L, 121L, 27L, 407L, 34L, 1210L, 454L, 320L, 877L, 581L, 1720L, 
        1281L, 847L, 1348L, 1066L, 389L, 1741L, 988L, 1753L, 712L, 2171L, 
        1069L, 1782L, 734L, 470L, 785L, 183L, 2151L, 1517L, 1017L, 597L, 
        1628L, 1728L, 2019L, 2062L, 250L, 1634L, 179L, 664L, 1005L, 807L, 
        805L, 1751L, 1443L, 1683L, 1387L, 632L, 1725L, 1549L, 1102L, 
        517L, 630L, 1943L, 565L, 130L, 1068L, 693L, 493L, 739L, 49L, 
        1407L, 2038L, 1612L, 1616L, 133L, 291L, 443L, 719L, 1912L, 1590L, 
        923L, 599L, 477L, 702L, 1439L, 1002L, 1954L, 912L, 1173L, 1570L, 
        492L, 2144L, 494L, 1605L, 1289L, 835L, 1916L, 1551L, 927L, 1298L, 
        1403L, 1301L, 1036L, 1606L, 638L, 2102L, 617L, 1853L, 244L, 1136L, 
        1486L, 502L, 1745L, 1796L, 1885L, 413L, 182L, 1598L, 614L, 647L, 
        1093L, 864L, 1615L, 1497L, 1428L, 2108L, 2170L, 1012L, 122L, 
        1511L, 1191L, 1847L, 1293L, 283L, 1891L, 1054L, 191L, 2092L, 
        474L, 1744L)

# subsamples to distribute
Compadre <- Compadre[s1,]
Comadre <- Comadre[s2,]


# subsample of legacy version (for testing asCompadreDB)
CompadreLegacy <- compadre
CompadreLegacy$metadata <- CompadreLegacy$metadata[s1,]
CompadreLegacy$mat <- CompadreLegacy$mat[s1]
CompadreLegacy$matrixClass <- CompadreLegacy$matrixClass[s1]


# Write the files into the data folder
usethis::use_data(Comadre,
                  Compadre,
                  CompadreLegacy,
                  overwrite = TRUE)

