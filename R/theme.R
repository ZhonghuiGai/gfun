#' GraphPad Prism themes
#'
#' @param base_size `numeric`. Base font size, given in `"pt"`.
#' @param angle `integer`. Angle of x_axis text in degrees.
#' One of: `-30, 45, 90, 270`.
#' @param border `logical`. Should a border be drawn around the plot?
#' Clipping will occur unless e.g. `coord_cartesian(clip = "off")` is used.
#' @param plot.margin plot.margin, trouble, mm
#'
#' @return Returns a list-like object of class _theme_.
#'
#' @export
theme_prism <- function(base_size = 16,
                        angle = 45,
                        border = FALSE,
                        plot.margin = c(3, 3, 3, 3)) {
  base_fontface = "bold"
  # Ensure x axis text is at a sensible angle
  angle <- angle[1]
  if(!angle %in% c(-30, 0, 45, 90, 270))
    stop(sprintf("'axis_text_angle' must be one of [%s]",
                 paste(c(-30, 0, 45, 90, 270), collapse=", ")),
         ".\nFor other angles, use the guide_axis() function in ggplot2 instead",
         call. = FALSE)
  # Draw border or not
  if(!is.logical(border)) {
    stop("border must be either: TRUE or FALSE")
  } else {
    if(border){
      panel.border <- element_rect(fill = NA)
      axis.line <- element_blank()
    }
    else if (!border) {
      panel.border <- element_blank()
      axis.line <- element_line()
    }
  }
  t <- theme(
    # Base elements (to be inherited by other elements)
    line = element_line(colour = "black", size = 1, linetype = 1, lineend = "square"),
    rect = element_rect(fill = "white", colour = "black", size = 1, linetype = 1),
    text = element_text(family = "serif", face = base_fontface,
                        colour = "black", size = base_size,
                        lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0,
                        margin = margin(), debug = FALSE),

    # Normal ggplot2 theme elements
    axis.line =          axis.line,
    axis.line.x =        NULL,
    axis.line.y =        NULL,
    axis.text =          element_text(size = rel(0.95), colour = "black"),
    axis.text.x =        element_text(margin = margin(t = 0.8 * base_size / 4),
                                      angle = angle,
                                      hjust = ifelse(angle %in% c(45, 90, 270), 1, 0.5),
                                      vjust = ifelse(angle %in% c(-30, 0, 90, 270), 0.5, 1)),
    axis.text.x.top =    element_text(margin = margin(b = 0.8 * base_size / 4), vjust = 0),
    axis.text.y =        element_text(margin = margin(r = 0.5 * base_size / 4), hjust = 1),
    axis.text.y.right =  element_text(margin = margin(l = 0.5 * base_size / 4), hjust = 0),
    axis.ticks =         element_line(),
    axis.ticks.length =  unit(base_size / 4, "pt"),
    axis.ticks.length.x = NULL,
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = NULL,
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    axis.title =         element_text(colour = "black"),
    axis.title.x =       element_text(margin = margin(t = base_size * 0.5), vjust = 1),
    axis.title.x.top =   element_text(margin = margin(b = base_size * 0.5), vjust = 0),
    axis.title.y =       element_text(angle = 90,
                                      margin = margin(r = base_size * 0.5),
                                      vjust = 1),
    axis.title.y.right = element_text(angle = -90,
                                      margin = margin(l = base_size * 0.5),
                                      vjust = 0),

    legend.background =  element_rect(fill = "transparent", colour = NA),
    legend.spacing =     unit(base_size, "pt"),
    legend.spacing.x =   NULL,
    legend.spacing.y =   NULL,
    legend.margin =      margin(0, 0, 0, 0),
    legend.key =         element_rect(fill = "transparent", colour = NA),
    legend.key.size =    unit(1.2, "lines"),
    legend.key.height =  NULL,
    legend.key.width =   unit(base_size, "pt"),
    legend.text =        element_text(size = rel(0.8)),
    legend.text.align =  NULL,
    legend.title =       element_blank(),
    legend.title.align = NULL,
    legend.position =    "right",
    legend.direction =   NULL,
    legend.justification = "center",
    legend.box =         NULL,
    legend.box.margin =  margin(0, 0, 0, 0, "cm"),
    legend.box.background = element_blank(),
    legend.box.spacing = unit(base_size, "pt"),

    panel.background = element_rect(fill = "transparent", colour = NA),
    panel.border =       panel.border,
    panel.grid =         element_blank(),
    panel.grid.minor =   element_blank(),
    panel.spacing =      unit(base_size/2, "pt"),
    panel.spacing.x =    NULL,
    panel.spacing.y =    NULL,
    panel.ontop    =     FALSE,

    strip.background =   element_blank(),
    strip.text =         element_text(colour = "black",
                                      size = rel(0.9),
                                      margin = margin(base_size / 2.5, base_size / 2.5,
                                                      base_size / 2.5, base_size / 2.5)),
    strip.text.x =       element_text(margin = margin(b = base_size / 3)),
    strip.text.y =       element_text(angle = -90, margin = margin(l = base_size / 3)),
    strip.text.y.left =  element_text(angle = 90),
    strip.placement =    "inside",
    strip.placement.x =  NULL,
    strip.placement.y =  NULL,
    strip.switch.pad.grid = unit(base_size / 4, "pt"),
    strip.switch.pad.wrap = unit(base_size / 4, "pt"),

    plot.background =    element_rect(fill = "transparent", colour = NA),
    plot.title =         element_text(size = rel(1.2),
                                      hjust = 0.5, vjust = 1,
                                      margin = margin(b = base_size)),
    plot.title.position = "panel",
    plot.subtitle =      element_text(hjust = 0.5, vjust = 1,
                                      margin = margin(b = base_size / 2)),
    plot.caption =       element_text(size = rel(0.8),
                                      hjust = 1, vjust = 1,
                                      margin = margin(t = base_size / 2)),
    plot.caption.position = "panel",
    plot.tag =           element_text(size = 25, family = "serif",
                                      hjust = 0, vjust = -1),
    plot.margin =        unit(plot.margin, "mm"),
    complete = TRUE
  )
}

#' ggplot theme for publication ready Plots
#'
#' @param base_size the default value is 14
#' @param border `logical`. Should a border be drawn around the plot?
#' @param angle `integer`. Angle of x_axis text in degrees.
#' One of: `-30, 45, 90, 270`.
#' @param plot.margin plot.margin, trouble, mm
#' @param panel.grid.major.color the color of grid, default value is "#f0f0f0".
#'
#' @return ggtheme object
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' gglm::gglm() + ggtheme::theme_pub()
#' gglm(group = "Species", label.y = 0.99) + ggtheme::theme_pub() +
#' ggprism::scale_color_prism(palette = "floral")
theme_pub <- function(base_size = 14,
                      border = FALSE,
                      angle = 0,
                      plot.margin = c(3, 3, 3, 3),
                      panel.grid.major.color = "#f0f0f0") {
  library(ggplot2)
  # Draw border or not
  if(!is.logical(border)) {
    stop("border must be either: TRUE or FALSE")
  } else {
    if(border){
      panel.border <- element_rect(fill = NA)
      axis.line <- element_blank()
    }
    else if (!border) {
      panel.border <- element_blank()
      axis.line <- element_line()
    }
  }
  # Ensure x axis text is at a sensible angle
  angle <- angle[1]
  if(!angle %in% c(-30, 0, 45, 90, 270))
    stop(sprintf("'axis_text_angle' must be one of [%s]",
                 paste(c(-30, 0, 45, 90, 270), collapse=", ")),
         ".\nFor other angles, use the guide_axis() function in ggplot2 instead",
         call. = FALSE)
  theme_foundation(base_size = base_size) +
    theme(plot.title =       element_text(face = "bold", size = rel(1.2), hjust = 0.5,
                                          margin = margin(0,0,20,0)),
          text =             element_text(face = "bold"),
          axis.line =        axis.line,
          panel.background = element_rect(fill = "transparent", colour = NA),
          panel.border =       panel.border,
          plot.background =  element_rect(fill = "transparent", colour = NA),
          axis.title =       element_text(face = "bold", size = rel(1.1)),
          axis.title.y =     element_text(angle = 90, vjust = 1),
          axis.title.x =     element_text(vjust = 1),
          axis.text =        element_text(face = "bold"),
          axis.text.x =      element_text(angle = angle,
                                          hjust = ifelse(angle %in% c(45, 90, 270), 1, 0.5),
                                          vjust = ifelse(angle %in% c(-30, 0, 90, 270), 0.5, 1)),
          axis.line.x =      element_line(colour = "black"),
          axis.line.y =      element_line(colour = "black"),
          axis.ticks =       element_line(),
          panel.grid.major = element_line(colour = panel.grid.major.color, size = 0.1),
          panel.grid.minor = element_blank(),
          legend.key =       element_rect(fill = "transparent", colour = NA),
          legend.position =  "bottom",
          legend.direction = "horizontal",
          legend.box =       "vetical",
          legend.key.size =  unit(0.5, "cm"),
          legend.margin =    margin(0, 0, 0, 0),
          legend.title =     element_blank(),
          legend.background =  element_rect(fill = "transparent", colour = NA),
          plot.margin =      unit(plot.margin, "mm"),
          strip.background = element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
          strip.text =       element_text(face = "bold"),
          plot.tag =         element_text(size = 25, face = "bold", family = "serif",
                                          hjust = 0, vjust = -1)
    )
}

#' Foundation Theme
#'
#' This theme is designed to be a foundation from which to build new
#' themes, and not meant to be used directly. \code{theme_foundation()}
#' is a complete theme with only minimal number of elements defined.
#' It is easier to create new themes by extending this one rather
#' than \code{\link[ggplot2]{theme_gray}()} or \code{\link[ggplot2]{theme_bw}()},
#' because those themes define elements deep in the hierarchy.
#'
#' This theme takes \code{\link[ggplot2]{theme_gray}()} and sets all
#' \code{colour} and \code{fill} values to \code{NULL}, except for the top-level
#' elements (\code{line}, \code{rect}, and \code{title}), which have
#' \code{colour = "black"}, and \code{fill = "white"}. This leaves the spacing
#' and-non colour defaults of the default \pkg{ggplot2} themes in place.
#'
#' @inheritParams ggplot2::theme_grey
#'
#' @family themes
#' @importFrom ggplot2 theme_grey
theme_foundation <- function(base_size = 13) {
  thm <- theme_grey(base_size = base_size, base_family = "serif")
  for (i in names(thm)) {
    if ("colour" %in% names(thm[[i]])) {
      thm[[i]]["colour"] <- list(NULL)
    }
    if ("fill" %in% names(thm[[i]])) {
      thm[[i]]["fill"] <- list(NULL)
    }
  }
  thm + theme(panel.border = element_rect(fill = NA),
              legend.background = element_rect(colour = NA),
              line = element_line(colour = "black"),
              rect = element_rect(fill = "white", colour = "black"),
              text = element_text(colour = "black"))
}

theme_nothing <- function(){
  theme_void() %+replace%
    theme(plot.margin = unit(c(0, 1, -0.2, -0.2), "lines"),
          plot.tag = element_text(size = 25, face = "bold", family = "serif",
                                  vjust = -1, hjust = 0))
}

theme_transparant <- function(){
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(2, 2, 2, 2), "mm"),
        plot.tag = element_text(size = 25, face = "bold", family = "serif",
                                vjust = -1, hjust = 0))
}
