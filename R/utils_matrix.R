# Internal replacements for popdemo::isIrreducible, isPrimitive, isErgodic.
# Algorithms follow Stott et al. (2010) Methods Ecol. Evol. 1:242-252.

.isIrreducible <- function(A) {
  n <- nrow(A)
  if (n == 1L) return(A[1L, 1L] > 0)
  S <- diag(n) + A
  Sn <- S
  for (i in seq_len(n - 2L)) Sn <- Sn %*% S
  all(Sn > 0)
}

.isPrimitive <- function(A) {
  if (!.isIrreducible(A)) return(FALSE)
  ev <- eigen(A, only.values = TRUE)$values
  lmax <- which.max(Re(ev))
  max(Mod(ev[-lmax])) < Mod(ev[lmax])
}

.isErgodic <- function(A) {
  if (!.isIrreducible(A)) return(FALSE)
  ev <- eigen(A, only.values = TRUE)$values
  lmax <- which.max(Re(ev))
  max(Re(ev[-lmax])) < Re(ev[lmax])
}
