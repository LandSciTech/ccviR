#' Function to plot scores for each question
#'
#' Will help to visualize what factors are influencing index
#'
plot_q_score <- function(vuln_df){
  vuln_df <- filter(vuln_df, !Code %in% c("Z2", "Z3")) %>%
    left_join(vulnq_code_lu_tbl, by = "Code") %>%
    mutate(score = case_when(score < 0 ~ 0,
                             is.na(score) ~ 0,
                             TRUE ~ score))

  plot_ly(vuln_df, hoverinfo = list("text", "y")) %>%
    add_bars(x = ~Code, y = ~score, name = "Score",
             text = ~Question,
             hovertemplate = "%{text}: <br> Score: %{y}<extra></extra>") %>%
    add_markers(x = ~Code, y = ~exp, name = "Exposure\nMultiplier",
                text = ~Question,
                hovertemplate = "%{text}: <br> Exposure Multiplier: %{y}<extra></extra>") %>%
    layout(xaxis = list(title = "Question"), yaxis = list(title = "Score"))
}



