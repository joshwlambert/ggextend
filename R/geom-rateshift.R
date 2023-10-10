#' Geom function for rate-shift model
#'
#' @export
#' @noRd
geom_rateshift <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           ...,
                           linejoin = "mitre",
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {
  ggplot2::layer(
    geom = GeomRateShift,
    data = data,
    mapping = mapping,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      linejoin = linejoin,
      na.rm = na.rm,
      ...
    )
  )
}

#' <ggproto> Geom constructor function for rate-shift model
#'
#' @export
#' @noRd
GeomRateShift <- ggplot2::ggproto(
  "Geomrateshift", ggplot2::Geom,
  default_aes = ggplot2::aes(colour = NA, fill = "purple", linewidth = 0.5, linetype = 1,
                    alpha = NA),
  required_aes = c("xmin", "xmax", "ymin", "ymax"),
  draw_key = ggplot2::draw_key_polygon,
  draw_panel = function(self, data, panel_params, coord, lineend = "butt", linejoin = "mitre") {
    coords <- coord$transform(data, panel_params)
    coords$alpha[is.na(coords$alpha)] <- 1
    grob <- grid::rectGrob(
      coords$xmin, coords$ymax,
      width = coords$xmax - coords$xmin,
      height = coords$ymax - coords$ymin,
      default.units = "native",
      just = c("left", "top"),
      gp = grid::gpar(
        col = coords$colour,
        fill = coords$fill,
        alpha = coords$alpha,
        lwd = coords$linewidth * .pt,
        lty = coords$linetype,
        linejoin = linejoin,
        lineend = lineend
      )
    )
    grob$name <- grid::grobName(
      grob = grob,
      prefix = "geom_rect"
    )
    grob
  }
)

