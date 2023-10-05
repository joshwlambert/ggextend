#' Simulate a time series of data under an arbitrary simple generating process
#'
#' @param t Total time of the simulation.
#' @param b Birth rate.
#' @param d Death rate.
#'
#' @return <data.frame> with two numeric columns
#' @export
sim_data <- function(t, b, d) {
    total_t <- t
    t <- 0
    p <- 100
    rel_b <- b / (b + d)
    rel_d <- d / (b + d)
    for (i in seq_len(total_t)) {
        diff <- sample(c(1, -1), 1, prob = c(rel_b, rel_d))
        p <- c(p, p[length(p)] + diff)
        t <- c(t, i)
    }

    # return data.frame
    data.frame(time = t, system = p)
}
