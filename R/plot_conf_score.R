plot_conf_score <- function(){
  res %>% select(scenario_name, index_conf) %>%
    tidyr::unnest(index_conf) %>%
    tidyr::pivot_longer(c(b_c_score, d_score), names_to = "section", values_to = "score") %>%
    ggplot2::ggplot(ggplot2::aes(x = score))+
    ggplot2::geom_histogram(binwidth = 1)+
    # ggplot2::labs(x = "Index",
    #               y = "Proportion of Runs",
    #               main = "Monte Carlo Simulation Results")+
    #ggplot2::ylim (c(NA, 1))+
    ggplot2::facet_grid(scenario_name ~ section, scales = "free_x")
}
