#' @title Read (import) data files from various sources
#' @name data_read
#' @param path Character string, the file path to the data file.
#' @param rowname See read.table row.names
#' @param colname See read.table col.names
#' @param sheet The name or index of the sheet to read data from.
#' @param verbose Toggle warnings and messages.
#' @param ... Arguments passed to the related `read_*()` function.
#' @export
data_read <- function(path, rowname = TRUE, colname = TRUE, sheet = 1,
                      verbose = TRUE, ...){
  type <- .file_ext(path)
  out <- switch(type,
                "txt" = .read_txt(path, rowname, colname, ...),
                "csv" = .read_csv(path, rowname, colname, ...),
                "xlsx" = .read_xlsx(path, rowname, colname, sheet, ...),
                "jpg" = .read_jpg(path, ...))
  if (verbose) {
    empty_cols <- empty_columns(out)
    if (length(empty_cols)) {
      sprintf("Following %i variables are empty:", length(empty_cols))
    }
  }
  out
}
# ---- helper functions --------------------------------------------------------
.file_ext <- function(x) {
  pos <- regexpr("\\.([[:alnum:]]+)$", x)
  ifelse(pos > -1L, substring(x, pos + 1L), "")
} #tools::file_ext
.read_txt <- function(path, rowname = TRUE, colname = TRUE, ...){
 out <- read.table(file = path, header = colname, sep = "\t", quote = "\"",
                   fill = TRUE,  dec = ".", comment.char = "",
                   row.names = .show_rowname(rowname), ...)
}
.read_csv <- function(path, rowname = TRUE, colname = TRUE, ...){
  out <- read.table(file = path, header = colname, sep = ",", quote = "\"",
                    fill = TRUE,  dec = ".", comment.char = "",
                    row.names = .show_rowname(rowname), ...)
}
.read_xlsx <- function(path, rowname = TRUE, colname = TRUE, sheet = 1, ...){
  out <- openxlsx::read.xlsx(xlsxFile = path, sheet = sheet,
                             colNames = colname, rowNames = rowname)
}
.show_rowname <- function(rowname) {
  unlist(ifelse(is.logical(rowname) & isTRUE(rowname), 1,
       ifelse(is.logical(rowname) & !isTRUE(rowname), list(NULL), list(rowname))))
}
.read_jpg <- function(path, plot.margin = c(1, 1, 1, 1), ...) {
  jpg <- jpeg::readJPEG(source = path)
  p <- ggplot() +
    annotation_raster(raster = jpg, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    theme(panel.background = element_rect(fill = "transparent", colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA),
          legend.key = element_rect(fill = "transparent", colour = NA),
          legend.background = element_rect(fill = "transparent", colour = NA),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          plot.margin = unit(plot.margin, "mm"),
          plot.tag = element_text(size = 25, face = "bold", family = "serif",
                                  vjust = -1, hjust = 0))
  return(p)
}
