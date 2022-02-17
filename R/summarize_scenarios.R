# vuln_calc_lst <- res

summarize_scenarios <- function(vuln_calc_df){
  index_freq <- vuln_calc_df %>%
    mutate(index = factor(index, levels = c("IE", "LV", "MV", "HV", "EV"), ordered = TRUE)) %>%
    group_by(index) %>%
    summarise(scenarios = paste0(scenario_name, collapse = ", "),
              n = n())

  index_stats <- data.frame(index_mode = index_freq %>% filter(n == max(n)) %>%
                              pull(index),
                            index_min = min(index_freq$index),
                            index_max = max(index_freq$index))

  mig_freq <- vuln_calc_df %>%
    mutate(mig_exp = factor(mig_exp, levels = c("N/A", "Low", "Moderate", "High"), ordered = TRUE)) %>%
    group_by(mig_exp) %>%
    summarise(scenarios = paste0(scenario_name, collapse = ", "),
              n = n())

  mig_stats <- data.frame(mig_mode = mig_freq %>% filter(n == max(n)) %>%
                              pull(mig_exp),
                            mig_min = min(mig_freq$mig_exp),
                            mig_max = max(mig_freq$mig_exp))

  section_score_summary <- vuln_calc_df %>% select(contains("score")) %>%
    tidyr::pivot_longer(contains("score"), names_to = "section", values_to = "score") %>%
    group_by(section) %>%
    summarise(across(score, lst(mean, sd, min, max)))

  question_score_summary <- bind_rows(vuln_calc_df$vuln_df %>%
                                        purrr::set_names(vuln_calc_df$scenario_name),
                                      .id = "scenario") %>%
    select(scenario, Code, exp, score) %>%
    mutate(score = ifelse(score < 0, 0, score)) %>%
    group_by(Code) %>%
    summarise(across(c(exp, score), lst(mean, sd, min, max)))

  lst(index_stats, index_freq, mig_stats, mig_freq, section_score_summary, question_score_summary)
}

