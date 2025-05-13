#' Plot evidence by type
#'
#' @returns ggplot2 figure
#' @export
#' @examples
#' e <- data.frame(
#'   score = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE),
#'   Code = c("B1", "B2a", "B2b", "B3", "C1", "C2ai", "C2aii"),
#'   evidence = c("", "Other", "Expert Opinion",
#'                "Expert Opinion, Literature", "",
#'                "Spatial Analysis", "Spatial Analysis - ccviR"))
#' plot_evidence(e)
#' plot_evidence(e[-5,])
#'
#' plot_evidence(data.frame(score = FALSE, Code = "B1", evidence = "Other"))

plot_evidence <- function(score_df, base_size = 14) {

  trans_int <- scales::trans_new("integer", as.integer, as.numeric)

  na <- "None\nsupplied"

  score_df <- score_df %>%
    # Only keep evidence which corresponds to an answered factor
    dplyr::filter(as.numeric(score) == 1) %>%
    dplyr::rename("evidence" = dplyr::matches("^evi$")) %>%
    # Depending on whether this comes from a loaded file or the inputs,
    # may need to be unnested first or later, so double up
    tidyr::unnest(evidence, keep_empty = TRUE) %>%
    dplyr::mutate(evidence = purrr::map(.data$evidence, ~{
      if(!is.na(.x)) stringr::str_split_1(.x, ", ?") else NA_character_
    })) %>%
    tidyr::unnest(evidence, keep_empty = TRUE) %>%
    dplyr::mutate(
      evidence = dplyr::case_when(
        .data$evidence == "" | is.na(.data$evidence) ~ na,
        .data$evidence == "Spatial Analysis - ccviR" ~ "Spatial Analysis\nccviR",
        .default = .data$evidence),
      ,
      )

  if(nrow(score_df) == 0) {
    g <- ggplot2::ggplot(data = score_df, ggplot2::aes(x = .data$evidence, fill = .data$evidence == na)) +
      ccvir_gg_theme(base_size) +
      ggplot2::annotate(geom = "text", x = 0.5, y = 0.5, label = "No questions answered", size = 6) +
      ggplot2::theme(axis.title = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(),
                     axis.text = ggplot2::element_blank())
    return(g)
  }


  # Order types of evidence
  lvls <- unique(score_df$evidence)
  lvls <- c(lvls[!lvls %in% c("Other", na)], "Other", na)
  score_df <- dplyr::mutate(score_df, evidence = factor(evidence, levels = lvls))

  # Calculate counts
  n_qs <- dplyr::filter(score_df, .data$evidence != na) %>%
    dplyr::count(.data$Code) %>%
    dplyr::pull(n) %>%
    mean() %>%
    round(2)

  n_missing <- dplyr::filter(score_df, .data$evidence == na) %>%
    nrow()
  if(n_missing > 0) {
    n_missing <- paste("No evidence supplied for", n_missing, "question(s)")
  } else n_missing <- NULL

  ggplot2::ggplot(data = score_df, ggplot2::aes(x = .data$evidence, fill = .data$evidence == na)) +
    ccvir_gg_theme(base_size) +
    ggplot2::theme(
      plot.subtitle = ggplot2::element_text(colour = "#d9534f", hjust = 1),
      legend.position = "none") +
    ggplot2::geom_bar() +
    ggplot2::scale_y_continuous(transform = trans_int) +
    ggplot2::scale_fill_manual(values = c("TRUE" = "#d9534f", "FALSE" = "grey35")) +
    ggplot2::labs(
      x = NULL, y = "Count",
      subtitle = n_missing,
      caption = paste(n_qs, "types of evidence per question, on average"))
}
