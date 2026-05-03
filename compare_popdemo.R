# Comparison script: internal .isIrreducible/.isPrimitive/.isErgodic vs popdemo
# Run this locally where both Rcompadre and popdemo are installed.
# Reports any matrices where the two implementations disagree.

library(Rcompadre)
library(popdemo)

# Source the internal functions directly
source("R/utils_matrix.R")

# Load bundled Compadre data
data(Compadre)

matAs <- matA(Compadre)

# Filter to square, non-NA matrices only
ok <- vapply(matAs, function(m) {
  is.matrix(m) && nrow(m) == ncol(m) && nrow(m) > 0 && !anyNA(m)
}, logical(1))

matAs <- matAs[ok]
cat("Comparing on", length(matAs), "matrices\n\n")

results <- data.frame(
  i             = seq_along(matAs),
  internal_irr  = vapply(matAs, .isIrreducible, logical(1)),
  popdemo_irr   = vapply(matAs, isIrreducible,  logical(1)),
  internal_prim = vapply(matAs, .isPrimitive,    logical(1)),
  popdemo_prim  = vapply(matAs, isPrimitive,     logical(1)),
  internal_erg  = vapply(matAs, .isErgodic,      logical(1)),
  popdemo_erg   = vapply(matAs, isErgodic,       logical(1))
)

diffs_irr  <- results[results$internal_irr  != results$popdemo_irr,  ]
diffs_prim <- results[results$internal_prim != results$popdemo_prim, ]
diffs_erg  <- results[results$internal_erg  != results$popdemo_erg,  ]

cat("=== isIrreducible disagreements:", nrow(diffs_irr), "===\n")
if (nrow(diffs_irr) > 0) print(diffs_irr)

cat("\n=== isPrimitive disagreements:", nrow(diffs_prim), "===\n")
if (nrow(diffs_prim) > 0) print(diffs_prim)

cat("\n=== isErgodic disagreements:", nrow(diffs_erg), "===\n")
if (nrow(diffs_erg) > 0) print(diffs_erg)

# Print an example disagreeing matrix for inspection
if (nrow(diffs_irr) > 0) {
  cat("\nExample disagreeing matrix (isIrreducible, index", diffs_irr$i[1], "):\n")
  print(matAs[[diffs_irr$i[1]]])
}
