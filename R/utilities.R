# data cleanup functions -------------------------------------------------------------------
#' @export
trim <- function(x, ...) UseMethod("trim")
#' @export
trim.default <- function(x, ...) gsub("^\\s+|\\s+$", "", x)
#' @export
trim.data.frame <- function(x, all = FALSE, ...) {
  if (all) {
    vars <- seq_len(ncol(x))
  } else {
    vars <- which(sapply(x, is.character))
  }
  if (length(vars)) {
    x[vars] <- lapply(x[vars], trim)
  }
  x
}
#' @export
trim.list <- function(x, all = FALSE, ...) {
  if (all) {
    vars <- seq_len(length(x))
  } else {
    vars <- which(sapply(x, is.character))
  }
  if (length(vars)) {
    x[vars] <- lapply(x[vars], trim)
  }
  x
}

