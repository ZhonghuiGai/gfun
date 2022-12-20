#' @export
data_clean <- function(x){
  x <- remove_empty(x)
  x <- trim(x)
  x
}
#' @export
empty_columns <- function(x) {
  if ((!is.matrix(x) && !is.data.frame(x)) || ncol(x) < 2) {
    vector("numeric")
  } else {
    all_na <- colSums(is.na(x)) == nrow(x)
    all_empty <- vapply(x, function(i) {
      (is.character(i) || is.factor(i)) && max(nchar(as.character(i)), na.rm = TRUE) == 0
    }, FUN.VALUE = logical(1))
    which(all_na | all_empty)
  }
}
#' @export
empty_rows <- function(x) {
  if ((!is.matrix(x) && !is.data.frame(x)) || nrow(x) < 2) {
    vector("numeric")
  } else {
    which(rowSums((is.na(x) | x == "")) == ncol(x))
  }
}
#' @export
remove_empty_columns <- function(x) {
  ec <- empty_columns(x)
  if (length(ec)) {
    x <- x[-ec]
  }
  x
}
#' @export
remove_empty_rows <- function(x) {
  er <- empty_rows(x)
  if (length(er)) {
    attr_data <- attributes(x)
    x <- x[-er, ]
    x <- .replace_attrs(x, attr_data)
  }
  x
}
#' @export
remove_empty <- function(x) {
  x <- remove_empty_rows(x)
  x <- remove_empty_columns(x)
  x
}

.replace_attrs <- function(data, custom_attr) {
  for (nm in setdiff(names(custom_attr), names(attributes(data.frame())))) {
    attr(data, which = nm) <- custom_attr[[nm]]
  }
  return(data)
}
