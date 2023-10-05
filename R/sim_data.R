#' Simulate a time series of data under an arbitrary simple generating process
#'
#' @param t Total time of the simulation.
#' @param b Birth rate.
#' @param d Death rate.
#' @param t2 Time window of rate regime 2. It can either be a single `numeric`,
#' which switches to rate regime 2 and does not return to rate regime 1; or a
#' vector of two `numerics` for when to enter and leave rate regime 2.
#' @param b2 Birth rate for rate regime 2.
#' @param d2 Death rate for rate regime 2.
#'
#' @return <data.frame> with two numeric columns
#' @export
sim_data <- function(t, b, d, t2 = NA, b2 = NA, d2 = NA) {
  # check input
  stopifnot(
    "Parameters for rate shift are of the incorrect length" =
      length(t2) >= 1 && length(b2) == 1 && length(d2) == 1,
    "Parameters for rate shift are not properly specified" =
      is.na(t2) || is.numeric(t2) && is.na(b2) || is.numeric(b2) &&
      is.na(d2) || is.numeric(d2)
  )

  total_t <- t
  t <- 0
  p <- 100
  rel_b <- b / (b + d)
  rel_d <- d / (b + d)

  t2[is.na(t2)] <- total_t
  b2[is.na(b2)] <- b
  d2[is.na(d2)] <- d
  rel_b2 <- b2 / (b2 + d2)
  rel_d2 <- d2 / (b2 + d2)
  if ((length(t2) %% 2) == 1) {
    t2 <- c(t2, total_t)
  }

  for (i in seq_len(total_t)) {

    if (i > t2[1] && i < t2[2]) {
      diff <- sample(c(1, -1), 1, prob = c(rel_b2, rel_d2))
    } else {
      diff <- sample(c(1, -1), 1, prob = c(rel_b, rel_d))
    }
    p <- c(p, p[length(p)] + diff)
    t <- c(t, i)
  }

  # return data.frame
  structure(data.frame(time = t, system = p), class = c("bdts", "data.frame"))
}
