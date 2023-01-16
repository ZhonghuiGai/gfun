#' Create random data for test functions
#'
#' @param x the mean value of the variables
#' @param group a character containing the names of group
#' @param n a integer, the numbers of value in each group, used for mean
#' @param sd the sd of mean function, default value is 0.1, which will affect the error bar
#'
#' @return a data frame
#' @export
#' @author Zhonghui Gai
#' @examples
#' data_pat(x = c(24, 35, 46), group = c("C", "T", "L"))
#' lst <- list(w1 = c(1, 2, 3, 4), w2 = c(5, 6, 7, 8), w3 = c(9, 10, 11, 12))
#' data <- data_pat(x = lst, group = c("C", "T", "L", "B"), n = 10, sd = 0.1)
#' aggregate(. ~ group, data = data, mean)
#' pat_barplot(data, variable = "w1") +
#'   ggpub::add_signif(comparisons = list(c("C", "T"), c("T", "L")),
#'                     y_position = c(3, 4)) +
#'   ylab(bquote(bold(paste("TNF ",alpha^2," (pg/mL)"))))
#' pat_point(data, fun = mean_se) + ylab(bquote(As (mu~mol ~CO[2]~ s^-1))) +
#'   annotate("text", x = c(2, 3), y = c(3, 7), label = c("***", "**"), size = 6) +
#' theme(legend.position = c(0.5, 0.1))

data_pat <- function(x, group, n, sd, ...) UseMethod("data_pat")

#' @export
data_pat.default <- function(x, group, n = 10, sd = 0.1){
  l <- length(group)
  grp <- lapply(group, rep, n) |> unlist()
  grp <- factor(grp, levels = group)
  dt <- lapply(x, rn, n, sd) |> unlist()
  res <- data.frame(group = grp, dt)
  return(res)
}

#' @export
data_pat.list <- function(x, group, n = 10, sd = 0.1){
  n <- n
  sd <- sd
  l <- length(group)
  grp <- lapply(group, rep, n) |> unlist()
  grp <- factor(grp, levels = group)
  res <- data.frame(group = grp)
  for (i in seq_along(lst)) {
    names <- names(lst)
    rst <- data_pat(x = lst[[i]], group = group, n = n, sd = sd)
    if (is.null(names)) {
      colnames(rst)[2] <- paste0("var", i)
    } else {
      colnames(rst)[2] <- names(lst)[i]
    }
    res <- cbind(res, rst[2])
  }
  return(res)
}

# ---- helper functions ----
rn <- function(x, n, sd){
  rnorm(n, x, x*sd)
}

# ---- bar plot using black and white theme ----
#' @export
pat_barplot <- function(data, variable = NULL, jitter = TRUE, border = FALSE,
                    fun.data = "mean_sdl", angle = 0){
  if (!is.null(variable)) data <- data[, c("group", variable)]
  dt <- data_long(data, cols = "group")
  p <- ggplot(data = dt, aes(x = group, y = value)) +
    stat_summary(geom = "errorbar", fun.data = get(fun.data), width = 0.3, linewidth = 0.7) +
    stat_summary(geom = "bar", fun = mean, color = "black", aes(fill = group),
                 linewidth = 0.8, width = 0.7)+
    scale_y_continuous(expand = c(0, 0), limits = c(NA, 1.25 * max(dt[, "value"]))) +
    xlab(NULL) + ylab(variable) +
    theme_pub(angle = angle, border = border) +
    scale_fill_grey() + theme(legend.position = 0)
  if (jitter)
    p <- p + geom_jitter(width = 0.25, fill = "white", shape = 21, size = 1.8)
  return(p)
}

#' @export
pat_point <- function(data, fun = mean_sdl, size = 3, angle = 0, border = FALSE,
                      label = TRUE){
  dt <- data_long(data, cols = "group")
  values <- c(15:18, 1, 6)[1:length(levels(data$group))]
  p <- ggplot(dt, aes(x = name, y = value, group = group, shape = group)) +
    xlab(NULL) + ylab(NULL) +
    stat_summary(geom = "errorbar", fun.data = fun, width = 0.1, show.legend = FALSE) +
    stat_summary(geom = "line", fun = mean, size = 0.5, show.legend = FALSE) +
    stat_summary(geom = "point", fun = mean, size = size) +
    theme_pub(angle = angle, border = border) + scale_color_grey() +
    scale_shape_manual(values = values)
  if (label) {
    data_label <- aggregate(. ~ group, data = data, mean)
    data_label <- data_label[, c(1, ncol(data_label))]
    data_label <- data_long(data_label, cols = "group")
    p <- p + ggrepel::geom_label_repel(data = data_label,
                                       aes(x = name, y = value, label = group),
                                       nudge_x = 0.3, nudge_y = 0.3,
                                       na.rm = TRUE, arrow = arrow(length = unit(0.01, "npc")),
                                       colour="black", segment.colour="gray50",
                                       box.padding = 0, label.padding = 0.1,
                                       point.padding = 0.5, family = "serif",
                                       fontface = "bold", label.size = NA) +
      theme(legend.position = 0)
  }
  return(p)
}
