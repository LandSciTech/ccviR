#' Plot evidence by type
#'
#' @returns ggplot2 figure
#' @export

plot_evidence <- function(score_df, base_size = 14) {

  score_df <- score_df %>%
    dplyr::rename("evidence" = dplyr::matches("^evi$")) %>%
    dplyr::mutate(evidence = dplyr::na_if(.data$evidence, ""))

  ggplot2::ggplot(data = score_df, ggplot2::aes(x = .data$evidence)) +
    ccvir_gg_theme(base_size) +
    ggplot2::geom_bar() +
    ggplot2::labs(x = NULL, y = "Number of Questions")

}
