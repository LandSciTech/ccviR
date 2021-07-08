#' Function to plot scores for each question
#'
#' Will help to visualize what factors are influencing index
#'
plot_q_score <- function(vuln_df){
  vuln_df <- filter(vuln_df, !Code %in% c("Z2", "Z3")) %>%
    mutate(shape = "Exposure Multiplier", fill = "Score") %>%
    #select(-Question) %>%
    left_join(vulnq_code_lu_tbl, by = "Code")

  plt <-  ggplot2::ggplot(vuln_df, ggplot2::aes(Code, text = Question))+
    ggplot2::geom_col(ggplot2::aes(y = score, fill = fill))+
    ggplot2::geom_point(ggplot2::aes(y = exp, shape = shape))+
    ggplot2::theme_classic()+
    ggplot2::ylim(0,NA)+
    ggplot2::labs(x = "Vulnerability Question", y = NULL, shape = NULL,
                  fill = NULL)
  plotly::ggplotly(plt, tooltip = c("text", "y"))
}



