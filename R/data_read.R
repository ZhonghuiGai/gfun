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
                "jpg" = .read_jpg(path, ...),
                "gff3" = .read_gff3(path, ...))
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
} # tools::file_ext
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
.read_jpg <- function(path, ...) {
  jpg <- jpeg::readJPEG(source = path)
  p <- ggplot() +
    annotation_raster(raster = jpg, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    theme_nothing()
  return(p)
}
# https://www.ncbi.nlm.nih.gov/datasets/docs/v2/reference-docs/file-formats/about-ncbi-gff3/
.read_gff3 <- function(gff3file){
  demo <- file(gff3file, open = "r")
  n <- 1
  res <- c()
  while(TRUE){
    line <-  readLines(demo, n = 1)
    if(grepl("^##FASTA", line)) break
    if(length(line) == 0) break
    if(grepl("^##", line) | grepl("^#!", line)) line <- NULL
      else if(grepl("gbkey=Src", line) & grepl("region", line)) line <- NULL
    line <- gsub("^#", "", line)
    res <- c(res, line)
    n <- n + 1
  }
  close(demo) # end of reading, and start cleaning
  resu <- lapply(res, strsplit, split = "\t")
  result <- matrix(NA, nrow = length(resu), ncol = 9)
  result <- as.data.frame(result)
  colnames(result) <- c("seqid", "source", "type", "start", "end",
                        "score", "strand", "phase", "attributes")
  for (i in 1:length(resu)) result[i, ] <- resu[[i]][[1]]
  result$ID <- .get_attribute_field(result, "ID")
  result$Name <- .get_attribute_field(result, "Name")
  result$product <- .get_attribute_field(result, "product")
  return(result)
}
.get_attribute_field <- function(x, field){
  s <- strsplit(x$attributes, split = ";", fixed = TRUE)
  return(sapply(s, .find_key_value, field))
}
.find_key_value <- function(atts, field){
  a <- strsplit(atts, split = "=", fixed = TRUE)
  m <- match(field, sapply(a, "[", 1))
  if (!is.na(m)) rv <- a[[m]][2] else rv <- as.character(NA)
  return(rv)
}
