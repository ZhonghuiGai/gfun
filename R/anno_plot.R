#' Annotations ggplot2 plot
#'
#' @param geom character Name of geom to use for annotation.
#' @param x,y,xmin,ymin,xmax,ymax,xend,yend,	numeric Positioning
#'   aesthetics - you must specify at least one of these.
#' @param label character, data.frame, ggplot or grob.
#' @param ...	Other named arguments passed on to \code{layer()}. These are often
#'   aesthetics, used to set an aesthetic to a fixed value, like color = "red"
#'   or size = 3. They may also be parameters to the paired geom/stat.
#' @param na.rm	logical If \code{FALSE}, the default, missing values are removed
#'   with a warning. If TRUE, missing values are silently removed.
#' @return A plot layer instance.
#' @export
#' @examples
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
#' p + anno_plot("text", x = 5, y = 32, label = "Some text")
#' p + anno_plot("label", x = c(2, 5), y = c(15, 32), label = c("A", "B"))
#' p + anno_plot("plot", x = 5.5, y = 34, label = p + theme_bw(9), vp.width = 0.3, vp.height = 0.4)
#' p + anno_plot("rect", xmin = 3, xmax = 4.2, ymin = -Inf, ymax = Inf, alpha = .2, fill = "darkred")
#' p + anno_plot("segment", x = 2.5, xend = 4, y = 15, yend = 25, colour = "blue")
#' p + anno_plot("pointrange", x = 3.5, y = 20, ymin = 12, ymax = 28, colour = "red", size = 1.5)
#'
anno_plot <- function(geom = "plot", x = NULL, y = NULL, xmin = NULL, xmax = NULL,
            ymin = NULL, ymax = NULL, xend = NULL, yend = NULL,
            na.rm = FALSE, label = NULL, ...) {
    # functions from ggplot2, needed here but not exported
    compact <- function(x) {
      null <- vapply(x, is.null, logical(1))
      x[!null]
    }

    new_data_frame <- function(x = list(), n = NULL) {
      if (length(x) != 0 && is.null(names(x))) {
        rlang::abort("Elements must be named")
      }
      lengths <- vapply(x, length, integer(1))
      if (is.null(n)) {
        n <- if (length(x) == 0 || min(lengths) == 0)
          0
        else max(lengths)
      }
      for (i in seq_along(x)) {
        if (lengths[i] == n)
          next
        if (lengths[i] != 1) {
          rlang::abort("Elements must equal the number of rows or 1")
        }
        x[[i]] <- rep(x[[i]], n)
      }
      tibble::as_tibble(x)
    }

    if (inherits(label, what = c("data.frame", "gg", "grob"))) {
      label <- list(label)
    }
    position <- compact(list(x = x,
                             xmin = xmin,
                             xmax = xmax,
                             xend = xend,
                             y = y,
                             ymin = ymin,
                             ymax = ymax,
                             yend = yend,
                             label = label))
    aesthetics <- c(position, list(...))
    lengths <- vapply(aesthetics, length, integer(1))
    n <- unique(lengths)
    if (length(n) > 1L) {
      n <- setdiff(n, 1L)
    }
    if (length(n) > 1L) {
      bad <- lengths != 1L
      details <- paste(names(aesthetics)[bad], " (", lengths[bad],
                       ")", sep = "", collapse = ", ")
      rlang::abort(glue::glue("Unequal parameter lengths: {details}"))
    }
    data <- new_data_frame(position, n = n)
    ggplot2::layer(geom = geom, params = list(na.rm = na.rm, ...),
                   stat = StatIdentity,
                   position = PositionIdentity, data = data,
                   mapping = aes_all(names(data)),
                   inherit.aes = FALSE, show.legend = FALSE)
  }

geom_plot <- function(mapping = NULL,
                      data = NULL,
                      stat = "identity",
                      position = "identity",
                      ...,
                      nudge_x = 0,
                      nudge_y = 0,
                      default.colour = "black",
                      colour.target = "segment",
                      default.alpha = 1,
                      alpha.target = "segment",
                      add.segments = TRUE,
                      box.padding = 0.25,
                      point.padding = 1e-06,
                      segment.linewidth = 0.5,
                      min.segment.length = 0,
                      arrow = NULL,
                      na.rm = FALSE,
                      show.legend = FALSE,
                      inherit.aes = FALSE) {
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position) && position != "identity") {
      rlang::abort("You must specify either `position` or `nudge_x`/`nudge_y`.")
    }
    # original position needed for "position" justification
    position <-
      position_nudge_center(nudge_x, nudge_y, kept.origin = "original")
  }
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPlot,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      default.colour = default.colour,
      colour.target = colour.target,
      default.alpha = default.alpha,
      alpha.target = alpha.target,
      add.segments = add.segments,
      box.padding = box.padding,
      point.padding = point.padding,
      segment.linewidth = segment.linewidth,
      min.segment.length = min.segment.length,
      arrow = arrow,
      na.rm = na.rm,
      ...
    )
  )
}

gplot_draw_panel_fun <- function(data, panel_params, coord, add.segments = TRUE,
                                 box.padding = 0.25, point.padding = 1e-06,
                                 segment.linewidth = 1, min.segment.length = 0,
                                 arrow = NULL, default.colour = "black",
                                 colour.target = "all", default.alpha = 1,
                                 alpha.target = "all", na.rm = FALSE) {

    if (nrow(data) == 0) {return(grid::nullGrob())}

    if (!ggplot2::is.ggplot(data$label[[1]])) {
      warning("Skipping as object mapped to 'label' is not",
              " a list of \"gg\" or \"ggplot\" objects.")
      return(grid::nullGrob())
    }

    add.segments <- add.segments && all(c("x_orig", "y_orig") %in% colnames(data))

    # should be called only once!
    data <- coord$transform(data, panel_params)
    if (add.segments) {
      data_orig <- data.frame(x = data$x_orig, y = data$y_orig)
      data_orig <- coord$transform(data_orig, panel_params)
      data$x_orig <- data_orig$x
      data$y_orig <- data_orig$y
    }

    if (is.character(data$vjust)) {
      data$vjust <-
        compute_just2d(data = data,
                       coord = coord,
                       panel_params = panel_params,
                       just = data$vjust,
                       a = "y", b = "x")
    }
    if (is.character(data$hjust)) {
      data$hjust <-
        compute_just2d(data = data,
                       coord = coord,
                       panel_params = panel_params,
                       just = data$hjust,
                       a = "x", b = "y")
    }
    if (add.segments) {
      segments.data <- shrink_segments(data,
                                       point.padding = point.padding,
                                       box.padding = box.padding,
                                       min.segment.length = min.segment.length)
    }
    # loop needed as gpar is not vectorized
    all.grobs <- grid::gList()

    for (row.idx in 1:nrow(data)) {
      row <- data[row.idx, , drop = FALSE]
      plot.alpha <-
        ifelse(any(alpha.target %in% c("all", "plot")),
               row$alpha, default.alpha)
      segment.alpha <-
        ifelse(any(alpha.target %in% c("all", "segment")),
               row$alpha, default.alpha)
      user.grob <- ggplot2::ggplotGrob(x = data$label[[row.idx]])

      user.grob$vp <-
        grid::viewport(x = grid::unit(row$x, "native"),
                       y = grid::unit(row$y, "native"),
                       width = grid::unit(row$vp.width, "npc"),
                       height = grid::unit(row$vp.height, "npc"),
                       just = c(row$hjust, row$vjust),
                       angle = row$angle,
                       name = paste("inset.plot.vp", row$PANEL,
                                    "row", row.idx, sep = "."))

      # give unique name to each grob
      user.grob$name <- paste("inset.plot", row.idx, sep = ".")

      if (add.segments) {
        segment.row <- segments.data[row.idx, , drop = FALSE]
        if (segment.row$too.short) {
          segment.grob <- grid::nullGrob()
        } else {
          segment.grob <-
            grid::segmentsGrob(x0 = segment.row$x,
                               y0 = segment.row$y,
                               x1 = segment.row$x_orig,
                               y1 = segment.row$y_orig,
                               arrow = arrow,
                               gp = grid::gpar(
                                 col = if (segment.linewidth == 0) NA else # lwd = 0 is invalid in 'grid'
                                   ifelse(any(colour.target %in% c("all", "segment")),
                                          ggplot2::alpha(row$colour, segment.alpha),
                                          ggplot2::alpha(default.colour, segment.alpha)),
                                 lwd = (if (segment.linewidth == 0) 1 else segment.linewidth) * .stroke),
                               name = paste("plot.s.segment", row$group, row.idx, sep = "."))
        }
        all.grobs <- grid::gList(all.grobs, segment.grob, user.grob)
      } else {
        all.grobs <- grid::gList(all.grobs, user.grob)
      }
    }
    grid::grobTree(children = all.grobs)
  }

GeomPlot <-
  ggplot2::ggproto("GeomPlot", ggplot2::Geom, required_aes = c("x", "y", "label"),
                   default_aes = ggplot2::aes(
                     colour = "black",
                     angle = 0,
                     hjust = "inward",
                     vjust = "inward",
                     alpha = NA,
                     family = "serif",
                     fontface = 1,
                     vp.width = 0.4,
                     vp.height = 0.4),
                   draw_panel = gplot_draw_panel_fun,
                   draw_key = function(...) {grid::nullGrob()})

# helper function
compute_just2d <- function(data, coord, panel_params, just, a = "x", b = a) {
  if (a != b) {
    angle <- data$angle
  }
  else {
    angle <- 0
  }
  if (any(grepl("outward|inward|position", just))) {
    angle <- angle%%360
    angle <- ifelse(angle > 180, angle - 360, angle)
    angle <- ifelse(angle < -180, angle + 360, angle)
    rotated_forward <- grepl("outward|inward|position", just) &
      (angle > 45 & angle < 135)
    rotated_backwards <- grepl("outward|inward|position",
                               just) & (angle < -45 & angle > -135)
    ab <- ifelse(rotated_forward | rotated_backwards, b,
                 a)
    swap_ab <- rotated_backwards | abs(angle) > 135
    if (any(just == "position")) {
      ab_orig <- paste(ab, "_orig", sep = "")
      position <- just == "position"
      if (!all(unique(ab_orig) %in% colnames(data))) {
        just[position] <- "middle"
      }
      else {
        just[position] <- c("left", "middle", "right")[2L +
                                                         1L * sign(data[[ab_orig[1L]]] - data[[ab[1L]]])]
      }
    }
    if (any(grepl("outward|inward", just))) {
      just_used <- unique(just)
      just_special <- grep("_mean$|_median$|.*[0-9].*",
                           just_used, value = TRUE)
      middle <- rep(0.5, length(just))
      for (j in just_special) {
        j_selector <- just == j
        if (j %in% c("outward_mean", "inward_mean")) {
          middle[j_selector & !swap_ab] <- mean(data[[a]])
          middle[j_selector & swap_ab] <- mean(data[[b]])
        }
        else if (j %in% c("outward_median", "inward_median")) {
          middle[j_selector & !swap_ab] <- stats::median(data[[a]])
          middle[j_selector & swap_ab] <- stats::median(data[[b]])
        }
        else {
          middle[j_selector & swap_ab] <- stats::median(data[[b]])
          middle_a <- as.numeric(gsub("outward_|inward_",
                                      "", unique(just)))
          if (a == "x") {
            tmp_data <- tibble::tibble(x = middle_a,
                                       y = data[[b]])
            middle[j_selector & !swap_ab] <- coord$transform(tmp_data,
                                                             panel_params)$x
          }
          else {
            tmp_data <- tibble::tibble(y = middle_a,
                                       x = data[[b]])
            middle[j_selector & !swap_ab] <- coord$transform(tmp_data,
                                                             panel_params)$y
          }
        }
      }
      just <- gsub("_.*$", "", just)
      obs <- data[[a]]
      obs[swap_ab] <- data[[b]][swap_ab]
      inward <- just == "inward"
      just[inward] <- c("left", "middle", "right")[just_dir(obs[inward],
                                                            split_at = middle[inward])]
      outward <- just == "outward"
      just[outward] <- c("right", "middle", "left")[just_dir(obs[outward],
                                                             split_at = middle[outward])]
    }
  }
  unname(c(left = 0, center = 0.5, right = 1, bottom = 0, middle = 0.5,
           top = 1)[just])
}

just_dir <- function(x, tol = 0.001, split_at = 0.5) {
  out <- rep(2L, length(x))
  out[x < split_at - tol] <- 1L
  out[x > split_at + tol] <- 3L
  out
}


