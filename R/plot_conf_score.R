plot_conf_score <- function(score_df){

  score_df %>% select(scenario_name, mc_results) %>%
    mutate(mc_results = purrr::map(mc_results, ~.x$index %>%
                                     factor(levels = c( "EV", "HV", "MV", "LV", "IE")) %>%
                                     table() %>%
                                     prop.table() %>%
                                     as.data.frame(stringsAsFactors = FALSE) %>%
                                     `names<-`(c("index", "frequency")))) %>%
    tidyr::unnest(mc_results) %>%
    ggplot2::ggplot(ggplot2::aes(x = factor(index, levels = c( "EV", "HV", "MV", "LV", "IE")),
                                 y = frequency))+
    ggplot2::geom_col(position = "dodge")+
    ggplot2::labs(x = "Index",
                  y = "Proportion of Runs",
                  main = "Monte Carlo Simulation Results")+
    ggplot2::ylim (c(NA, 1))+
    ggplot2::facet_wrap(~scenario_name, ncol = 3)
}
