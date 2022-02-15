# vuln_calc_lst <- res

summarize_scenarios <- function(vuln_calc_df){
  index_freq <- vuln_calc_df %>% group_by(index) %>%
    summarise(scenarios = paste0(scenario_name, collapse = ", "),
              n = n())

  section_score_summary <- vuln_calc_df %>% select(contains("score")) %>%
    tidyr::pivot_longer(contains("score"), names_to = "section", values_to = "score") %>%
    group_by(section) %>%
    summarise(across(score, lst(mean, sd, min, max)))

  question_score_summary <- bind_rows(vuln_calc_df$vuln_df %>%
                                        purrr::set_names(vuln_calc_df$scenario_name),
                                      .id = "scenario") %>%
    select(scenario, Code, Question, exp, score) %>%
    mutate(score = ifelse(score < 0, 0, score)) %>%
    group_by(Code, Question) %>%
    summarise(across(c(exp, score), lst(mean, sd, min, max)))


}

