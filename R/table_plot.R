#' Draw a Textual Table
#'
#' @param table a data frame
#' @param theme the type of theme, one of grey, blue, red, violet, cyan, and triline
#' @param size  the size of text in the table
#' @param row a numeric vector of row indexes
#' @param col a numeric vector of column indexes
#' @param color text font color
#' @param ... extra parameters for text justification
#'
#' @return an object of class ggplot.
#' @export
#'
#' @examples
#' df <- data.frame(A = 1:2, B = letters[1:2])
#' df$B <- c("italic(α\u03b2)", "bold(adonis) (bold(italic(R))^bold(`2`))")
#' colnames(df) <- c("bold(p.value)", "italic(p.adj[2]^3)")
#' table_plot(table = df, theme = "blue")
#' p2 <- table_plot(table = df, theme = "triline", row = 2, col = 2, color = "purple")
#' p1 <- table_plot(table = df, theme = "violet", row = 2:3, col = 2, size = 16, color = "darkblue")
#' gfun:::Greek_alphabet()  |> groutable::dt2()
#' p <- ggplot(data=  data.frame(x=1:24,y=1:24), aes(x = x,y = y)) +
#' geom_point(shape = gfun:::Greek_alphabet()$lower, size = 8) +
#'   theme_prism(angle = 0) +
#'   ylab(bquote(bold(paste("TNF ",alpha^2," (pg/mL)")))) +
#'   xlab(bquote(Assimilation (mu~ mol ~CO[2]~ m^-2~s^-1))) +#
#'   labs(title = expression(bold(log[10]^2)),
#'        subtitle = "δφυαμΣ\u03b2",
#'        caption = expression(bold(adonis) (bold(italic(R))[3]^bold(`2`))))
#' pp <- p + anno_plot("plot", x = 12, y = 45, label = p2) +
#'   anno_plot("plot", x = 2, y = 45, label = p1)
#' grDevices::dev.off()
#' ggsave("image.pdf", width = 8, height = 5, device = cairo_pdf, plot = pp)
table_plot <- function(table, theme = "grey", size = 10,
                       row = NULL, col = NULL, color = "#D04042", ...){
  nrow <- nrow(table) + 1
  tab <- switch(theme,
                grey = ggtexttable(table, rows = NULL, theme = ttheme(base_size = size)),
                blue = ggtexttable(table, rows = NULL, theme = ttheme("mBlue", base_size = size)),
                red = ggtexttable(table, rows = NULL, theme = ttheme("mRed", base_size = size)),
                violet = ggtexttable(table, rows = NULL, theme = ttheme("mViolet", base_size = size)),
                cyan = ggtexttable(table, rows = NULL, theme = ttheme("mCyan", base_size = size)),
                triline = ggtexttable(table, rows = NULL, theme = ttheme("blank", base_size = size)) |>
                  tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 1, linecolor = "gray10") |>
                  tab_add_hline(at.row = nrow, row.side = "bottom", linewidth = 1),
                NULL
  )
  if (is.numeric(row) & is.numeric(col)) {
    tab <- table_cell_font(tab = tab, row = row, column = col, size = size, color = color)
  }
  return(tab)
}

ggtexttable <- function(x, rows = rownames(x), cols = colnames(x), vp = NULL, theme = ttheme(), ...) {
  res <- gridExtra::tableGrob(x, rows = rows, cols = cols, vp = vp, theme = theme, ...)
  .grob <- res
  res <- as_ggplot(res)
  attr(res, "ggtexttableGrob") <- .grob
  return(res)
}

ttheme <- function(base_style = "default", base_size = 11, base_colour = "black", padding = unit(c(2, 1.5), "mm"),
                   colnames.style = colnames_style(size = base_size),
                   rownames.style = rownames_style(size = base_size),
                   tbody.style = tbody_style(size = base_size)) {
  style <- tstyle(base_style, size = base_size)
  if(!is.null(style)){
    if(missing(colnames.style)) colnames.style <- style$colnames.style
    if(missing(rownames.style)) rownames.style  <- style$rownames.style
    if(missing(tbody.style)) tbody.style <- style$tbody.style
  }
  .ttheme <- gridExtra::ttheme_default(base_size = base_size,
                                       base_colour = base_colour,
                                       padding = padding,
                                       base_family = "serif")
  .ttheme$colhead <- do.call(.add_item, c(list(.list = .ttheme$colhead), colnames.style))
  .ttheme$rowhead <- do.call(.add_item, c(list(.list = .ttheme$rowhead), rownames.style))
  .ttheme$core <- do.call(.add_item, c(list(.list = .ttheme$core), tbody.style))
  attr(.ttheme, "style") <- base_style
  .ttheme
}

colnames_style <- function(color = "black", face = "bold", size = 12,
                           fill = "grey80", linewidth = 1, linecolor = "white",
                           parse = TRUE, fontfamily = "serif", ...) {
  list(
    fg_params = list(parse = parse, col = color,
                     fontface = face, fontsize = size+1, fontfamily = fontfamily) |>
      .add_item(...), # Accept extra parameters
    bg_params = list(fill = fill, lwd = linewidth, col = linecolor))
}

rownames_style <- function(color = "black", face = "italic", size = 12,
                           fill = NA, linewidth = 1, linecolor = "white",
                           parse = TRUE, fontfamily = "serif", ...) {
  list(
    fg_params = list(parse = parse, col = color,
                     fontface = face, fontsize = size, hjust = 1, x = 0.95,
                     fontfamily = fontfamily) |>
      .add_item(...), # Accept extra parameters
    bg_params = list(fill = fill, lwd = linewidth, col = linecolor))
}

tbody_style <- function(color = "black", face = "plain", size = 12,
                        fill = c("grey95", "grey90"),
                        linewidth = 1, linecolor = "white",
                        parse = TRUE, fontfamily = "serif", ...) {
  list(
    fg_params = list(parse = parse, col = color,
                     fontface = face, fontsize = size, fontfamily = fontfamily) |>
      .add_item(...), # Accept extra parameters
    bg_params = list(fill = fill, lwd = linewidth, col = linecolor))
}


# row,column an integer specifying the row and the column numbers for the cell of interest.
table_cell_font <- function(tab, row, column, face = NULL, size = NULL, color = "black") {
  tabGrob <- get_tablegrob(tab)
  cells <- expand.grid(row = row, column = column)
  for(i in 1:nrow(cells)){
    tc <- .find_cell(tabGrob, cells$row[i], cells$column[i], "core-fg")
    tabGrob$grobs[tc][[1]][["gp"]] <- grid::gpar(fontface = face, fontsize = size, col = color, fontfamily = "serif")
  }
  tab_return_same_class_as_input(tabGrob, input = tab)
}

table_cell_bg <- function(tab, row, column, fill = NULL, color = NULL, linewidth = NULL, alpha = NULL) {
  tabGrob <- get_tablegrob(tab)
  cells <- expand.grid(row = row, column = column)
  for(i in 1:nrow(cells)){
    tc <- .find_cell(tabGrob, cells$row[i], cells$column[i], "core-bg")
    tabGrob$grobs[tc][[1]][["gp"]] <- grid::gpar(
      fill = fill, col = color, lwd = linewidth, alpha = alpha)
  }
  tab_return_same_class_as_input(tabGrob, input = tab)
}

.find_cell <- function(tab, row, column, name="core-fg"){
  l <- tab$layout
  which(l$t==row & l$l==column & l$name==name)
}

tab_ncol <- function(tab){ncol(get_tablegrob(tab))}
tab_nrow <- function(tab){nrow(get_tablegrob(tab))}

tab_add_hline <- function(tab, at.row = 2:tab_nrow(tab), row.side = c("bottom", "top"),
                          from.column = 1, to.column = tab_ncol(tab),
                          linetype = 1, linewidth = 1, linecolor = "black"){
  row.side <- match.arg(row.side)
  tabgrob <- get_tablegrob(tab)
  separators <- replicate(
    n = length(at.row),
    tab_hline(row.side = row.side, linetype = linetype, linewidth = linewidth, linecolor = linecolor),
    simplify = FALSE)
  tabgrob <- gtable::gtable_add_grob(
    tabgrob, grobs = separators,
    t = at.row, b = at.row,
    l = from.column, r = to.column)
  tab_return_same_class_as_input(tabgrob, input = tab)
}

# Create hline at the top or the bottom side of a given row
tab_hline <- function(row.side = c("bottom", "top"), linetype = 1, linewidth = 1, linecolor = "black"){
  row.side <- match.arg(row.side)
  y0 <- y1 <- unit(0, "npc")
  if(row.side == "top") y0 <- y1 <- unit(1, "npc")
  grid::segmentsGrob(x0 = unit(0, "npc"), x1 = unit(1,"npc"),
    y0 = y0, y1 = y1,
    gp = grid::gpar( lty = linetype, lwd = linewidth, col = linecolor))
}

# Transform a table grob in ggtexttable like object
as_ggtexttable <- function(tabgrob){
  res <- as_ggplot(tabgrob)
  attr(res, "ggtexttableGrob") <- tabgrob
  res
}

# Extract tableGrob from ggtexttable()
get_tablegrob <- function(tab){
  if(is_ggtexttable(tab)){tabgrob <- attr(tab, "ggtexttableGrob")}
  else if(is_tablegrob(tab)){tabgrob <- tab}
  else{stop("tab should be an object from either ggpubr::ggtexttable() or gridExtra::tableGrob().")}
  tabgrob
}
# Return the same class as the input data, which can be either ggtextable or a gridExtra::tableGrob
tab_return_same_class_as_input <- function(tabgrob, input){
  if(is_ggtexttable(input)){return(as_ggtexttable(tabgrob))}
  else if(is_tablegrob(input)){return(tabgrob)}
  tabgrob
}
is_ggtexttable <- function(tab){!is.null(attr(tab, "ggtexttableGrob"))}
is_tablegrob <- function(tab){inherits(tab, "gtable") & inherits(tab, "grob")}

# Define table style
tstyle <- function(pal, size = 12){
  allowed.palettes = c("default", "blank", "mBlue", "mRed",  "mViolet", "mCyan" )
  if(!(pal %in% allowed.palettes ))
    stop(pal, " is not a supported palette")
  style <- switch(pal,
                  blank = list(
                    colnames.style = colnames_style(fill = NA, linecolor = NA, size = size),
                    rownames.style = rownames_style(fill = NA, linecolor = NA, size = size),
                    tbody.style = tbody_style(fill = NA, linecolor = NA, size = size)),
                  mBlue = list(
                    colnames.style = colnames_style(color = "white", fill = "#477DC0",
                                                     linecolor = "white", linewidth = 1, size = size),
                    rownames.style = rownames_style(size = size),
                    tbody.style = tbody_style(color = "black", fill = c("#B5CBE5", "#DAE6F2"),
                                              linecolor = "white", size = size)),
                  mRed = list(
                    colnames.style = colnames_style( color = "white", fill = "#D04042",
                                                     linecolor = "white", linewidth = 1, size = size),
                    rownames.style = rownames_style(size = size),
                    tbody.style = tbody_style(color = "black", fill = c("#EFB4B5", "#F7DBDA"),
                                              linecolor = "white", size = size)),
                  mViolet = list(
                    colnames.style = colnames_style( color = "white", fill = "#895AA3",
                                                     linecolor = "white", linewidth = 1, size = size),
                    rownames.style = rownames_style(size = size),
                    tbody.style = tbody_style(color = "black", fill = c("#D0BDDA", "#E6DEEC"),
                                              linecolor = "white", size = size)),
                  mCyan = list(
                    colnames.style = colnames_style( color = "white", fill = "#00AEC9",
                                                     linecolor = "white", linewidth = 1, size = size),
                    rownames.style = rownames_style(size = size),
                    tbody.style = tbody_style(color = "black", fill = c("#ADDFEA", "#D5EFF4"),
                                              linecolor = "white", size = size)),
                  NULL)
  style
}

.add_item <- function(.list, ...){
  pms <- list(...)
  for(pms.names in names(pms)){
    .list[[pms.names]] <- pms[[pms.names]]
  }
  .list
}
