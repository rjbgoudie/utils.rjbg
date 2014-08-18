#' @export
theme_rjbg_white <- function(base_size = 8, base_family = "") {
  # Starts with theme_grey and then modify some parts
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
  theme(
    axis.ticks        = element_line(colour = "grey80",
                                     size = 0.25),
    legend.key        = element_rect(fill = "white",
                                     colour = "grey80",
                                     size = 0.25),
    panel.background  = element_rect(fill = "white", colour = NA),
    panel.border      = element_rect(fill = NA,
                                     colour = "grey80",
                                     size = 0.5),
    panel.grid.major  = element_line(colour = "grey94",
                                     size = 0.125),
    panel.grid.minor  = element_line(colour = "grey97",
                                     size = 0.075),
    strip.background  = element_rect(fill = "grey80",
                                     size = NA),
    strip.text.x      = element_text(colour = "black"),
    strip.text.y      = element_text(colour = "black",
                                     angle = -90),
    legend.key = element_blank(),
    legend.background = element_rect(fill="transparent")
    )
}

#' @export
colours_rjbg <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
                  "#D55E00", "#CC79A7")[c(1, 5, 3, 7, 2, 4, 6)]

.onLoad <- function(libname = find.package("utils.rjbg"),
                    pkgname = "utils.rjbg"){
  if (require(ggplot2)){
    update_geom_defaults("line", list(size = 0.2))
    update_geom_defaults("step", list(size = 0.2))
  }
}
