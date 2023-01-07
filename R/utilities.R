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

# from ggplotfy pacakge, also look as_ggplot() function in ggpubr package
as.ggplot <- function(plot) {
  ggplot(data.frame(x = 0:1, y = 0:1), aes_(x = ~x, y = ~y)) +
    geom_blank() +
    scale_x_continuous(limits = c(0,1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0,1), expand = c(0, 0)) +
    annotation_custom(plot$gtable, xmin = 0, xmax = 1, ymin = 0, ymax = 1)  +
    theme_nothing()
}

as_ggplot <- function(x){cowplot::ggdraw() + cowplot::draw_grob(grid::grobTree(x))}

Greek_alphabet <- function(){
  data <- structure(list(
    upper = c("Α", "Β", "Γ", "Δ", "Ε", "Ζ", "Η", "Θ", "Ι", "Κ", "Λ", "Μ", "Ν", "Ξ", "Ο", "Π", "Ρ", "Σ", "Τ",
              "Υ", "Φ", "Χ", "Ψ", "Ω"),
    unicode_upper = c("\\u0391", "\\u0392", "\\u0393", "\\u0394", "\\u0395", "\\u0396", "\\u0397", "\\u0398",
                      "\\u0399", "\\u039A", "\\u039B", "\\u039C", "\\u039D", "\\u039E", "\\u039F", "\\u03A0", "\\u03A1", "\\u03A3", "\\u03A4", "\\u03A5", "\\u03A6",
                      "\\u03A7", "\\u03A8", "\\u03A9"),
    lower = c("α", "β", "γ", "δ", "ε", "ζ", "η", "θ", "ι", "κ", "λ", "μ", "ν", "ξ", "ο",
              "π", "ρ", "σ", "τ", "υ", "φ", "χ", "ψ", "ω"),
    unicode_lower = c("\\u03b1", "\\u03b2", "\\u03b3", "\\u03b4", "\\u03b5", "\\u03b6",
                      "\\u03b7", "\\u03b8", "\\u03b9", "\\u03ba", "\\u03bb", "\\u03bc", "\\u03bd", "\\u03be", "\\u03bf", "\\u03c0", "\\u03c1", "\\u03c3", "\\u03c4",
                      "\\u03c5", "\\u03c6", "\\u03c7", "\\u03c8", "\\u03c9")),
    row.names = c("alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta",
                  "iota", "kappa", "lambda", "mu", "nu", "xi", "omicron", "pi", "rho", "sigma", "tau", "upsilon", "phi", "chi", "psi", "omega"),
    class = "data.frame"
  )
  return(data)
}
