#' Shared internal helpers for network topology
#'
#' Logic used identically by both \code{get_network_topology()} and
#' \code{get_network_topology_parallel()}. Kept in one place so fixes (e.g. NA
#' guards, cohesion definition) cannot drift between the serial and parallel
#' implementations.
#'
#' @keywords internal
#' @noRd
NULL

#' Compute per-network cohesion (positive / negative).
#'
#' Mirrors the historical nested `.cohension_compute()`; `mat` (variables in
#' rows, samples in columns) is passed explicitly instead of being captured
#' from an enclosing scope.
#'
#' @param network.raw Numeric adjacency-like matrix (thresholded correlations).
#' @param mat The abundance matrix used to weight cohesion by mean relative
#'   abundance.
#' @return A list with `cohension_position` and `cohension_negative`.
#' @keywords internal
#' @noRd
.compute_cohension <- function(network.raw, mat) {

  ASV_Correlation <- network.raw %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "ASV") %>%
    tidyr::pivot_longer(cols = -ASV, values_to = "Correlation", names_to = "ASV_to") %>%
    dplyr::filter(ASV != ASV_to) %>%
    dplyr::filter(Correlation != 0) %>%
    dplyr::group_by(ASV) %>%
    dplyr::mutate(r_pos_mean = mean(dplyr::if_else(Correlation > 0, Correlation, 0), na.rm = TRUE),
                  r_neg_mean = mean(dplyr::if_else(Correlation < 0, Correlation, 0), na.rm = TRUE),
                  t_total = mean(dplyr::if_else(Correlation < 0, abs(Correlation), abs(Correlation)), na.rm = TRUE)) %>%
    dplyr::ungroup()  %>%
    dplyr::distinct(ASV, .keep_all = TRUE) %>%
    dplyr::select(1, 4, 5, 6)

  relative_abundance_df <- mat %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "ASV") %>%
    tidyr::pivot_longer(cols = -ASV, values_to = "relative_abundance", names_to = "Groups") %>%
    dplyr::group_by(ASV) %>%
    dplyr::summarise(mean_relative_abundance = mean(relative_abundance))

  ASV_cohensition <- ASV_Correlation %>%
    dplyr::left_join(relative_abundance_df, by = "ASV") %>%
    dplyr::mutate(positive_cohension = r_pos_mean * mean_relative_abundance,
                  negative_cohension = r_neg_mean * mean_relative_abundance,
                  total_cohension = t_total * mean_relative_abundance)

  # guard against `mean(numeric(0))` (which returns NaN with a warning)
  # when every per-ASV cohesion is exactly 0.
  .safe_mean <- function(x) if (length(x) == 0L) NA_real_ else mean(x)
  list(
    cohension_position = .safe_mean(ASV_cohensition$positive_cohension[ASV_cohensition$positive_cohension != 0]),
    cohension_negative = .safe_mean(ASV_cohensition$negative_cohension[ASV_cohensition$negative_cohension != 0])
  )
}
