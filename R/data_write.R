#' Save data using write.table function
#'
#' @param file the file to be saved, usually a data frame or a matrix
#' @param colname either a logical value indicating whether the column names of x are to be written along with x, or a character vector of column names to be written.
#' @param rowname either a logical value indicating whether the row names of x are to be written along with x, or a character vector of row names to be written.
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' data_write(mtcars, T)
data_write <- function(data, rowname = FALSE, colname = TRUE){
  filename <- as.character(substitute(data))
  cat(paste("The file to be saved is:", filename, "\n"))
  file.name <- paste0(filename, "_", format(Sys.time(), format = "%y%m%d%M"), ".csv")
  if (rowname) data <- data.frame(sample = rownames(data), data)
  write.table(data, file = file.name, sep = ",", col.names = colname, row.names = FALSE)
  message(paste("Save: ", file.name, "...... Finished!"))
}
