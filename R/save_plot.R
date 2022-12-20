#' Save a plot as PDF
#'
#' @param filename  Filename to save to an PDF
#' @param plot A plot object
#' @param width Width of the image in inches
#' @param height Height of the image in inches
#' @param ... other parameters
#'
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
save_plot <- function(width = 10, height = 10, plot = last_plot(), ...) {
  filename <- as.character(substitute(plot))
  filename <- paste0(filename, "_", format(Sys.time(), format = "%y%m%d%M"), sample(LETTERS, 1), ".pdf")
  if (inherits(plot, "ggplot")) {
    ggplot2::ggsave(filename = filename,
                    plot = plot,
                    width = width,
                    height = height)
  }
  if (inherits(plot, "pheatmap")) {
    pdf(file = filename, width = width, height = height)
    grid::grid.newpage()
    grid::grid.draw(plot$gtable)
    grDevices::dev.off()
  }
  if (inherits(plot, "NULL")) {
    stop("The plot using base R should be saved manually!")
  }
}
