#' Plot method for `<bdts>`
#'
#' @param x An `<bdts>` object
#' @param ... [dots] not used and will produce a warning if extra
#' arguments are supplied
#'
#' @importFrom rlang .data
#'
#' @return A plot, called for side-effects.
#' @export
plot.bdts <- function(x, ...) {
  chkDots(...)
  ggplot2::ggplot(data = x) +
    ggplot2::geom_point(
      mapping = ggplot2::aes(x = .data$time, y = .data$system)
    ) +
    ggplot2::geom_line(
      mapping = ggplot2::aes(x = .data$time, y = .data$system)
    ) +
    ggplot2::scale_x_continuous(name = "Time steps") +
    ggplot2::scale_y_continuous("Population size") +
    ggplot2::theme_bw()
}
