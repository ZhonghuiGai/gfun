.onAttach <- function(libname, pkgname) {
  quote <- "  I'd seen my father. He was a poor man, and I watched him do astonishing things.\n    - Sidney Poitier"
  packageStartupMessage(quote)
  require(ggplot2, quietly = TRUE)
  require(patchwork, quietly = TRUE)
  cat("\n  Thses packages are loaded: ggplot2; patchwork\n ")
}
