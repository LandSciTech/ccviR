#' Create bar indicator of index
#'
#' If min or max are missing use ind ie no error bars
#'
#' @param ind,ind_max,ind_min string index initials one of "IE", "EV", "HV",
#'   "MV", "LV"
#'
#' @noRd

plt_index_gauge <- function(ind, ind_min = ind, ind_max = ind){
  df <- data.frame(val = 1,
                   ind = rep(c("IE", "EV", "HV", "MV", "LV"), 20) %>%
                     factor(levels = c("IE","LV", "MV", "HV", "EV")) %>%
                     sort())

  val_df <- data.frame(ind = ind, ind_min = ind_min, ind_max = ind_max) %>%
    mutate(across(.fns = ~case_when(.x == "IE" ~ 10,
                                    .x == "EV" ~ 90,
                                    .x == "HV" ~ 70,
                                    .x == "MV" ~ 50,
                                    .x == "LV" ~ 30,
                                    TRUE ~ 10))) %>%
    mutate(val = 1)

  ggplot2::ggplot(df, ggplot2::aes(x = val, fill = ind))+
    ggplot2::geom_bar()+
    ggplot2::geom_hline(yintercept = val_df$ind, size = 2)+
    ggplot2::geom_errorbar(data = val_df,
                           ggplot2::aes(x = val, ymin = ind_min, ymax = ind_max),
                           inherit.aes = FALSE)+
    ggplot2::coord_flip()+
    ggplot2::scale_y_continuous(breaks = c(10, 30, 50, 70, 90),
                                labels = c("Insufficient\nEvidence",
                                           "Less\nVulnerable",
                                           "Moderately\nVulnerable",
                                           "Highly\nVulnerable",
                                           "Extremely\nVulnerable"),
                                name = NULL, expand = ggplot2::expansion())+
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(), labels = NULL,
                                breaks = NULL, name = NULL)+
    ggplot2::scale_fill_discrete(type = rev(c("#808080", "#008000", "#FFC125", "#FF8C00",
                                              "#FF0000")), guide = "none")+
    ggplot2::theme_minimal()+
    ggplot2::theme(panel.border = ggplot2::element_rect(fill = NA),
                   axis.text = ggplot2::element_text(size = 12))
}

plt_mig_exp_gauge <- function(ind, ind_min = ind, ind_max = ind){

  df <- data.frame(val = 1,
                   ind = rep(c("N/A", "Low", "Moderate", "High"), 25) %>%
                     factor(levels = c("N/A", "Low", "Moderate", "High")) %>%
                     sort())

  val_df <- data.frame(ind = ind, ind_min = ind_min, ind_max = ind_max) %>%
    mutate(across(.fns = ~case_when(.x == "N/A" ~ 13.5,
                                    .x == "Low" ~ 38.5,
                                    .x == "Moderate" ~ 63.5,
                                    .x == "High" ~ 88.5,
                                    TRUE ~ 13.5))) %>%
    mutate(val = 1)

  ggplot2::ggplot(df, ggplot2::aes(x = val, fill = ind))+
    ggplot2::geom_bar()+
    ggplot2::geom_hline(yintercept = val_df$ind, size = 2)+
    ggplot2::geom_errorbar(data = val_df,
                           ggplot2::aes(x = val, ymin = ind_min, ymax = ind_max),
                           inherit.aes = FALSE)+
    ggplot2::coord_flip()+
    ggplot2::scale_y_continuous(breaks = c(13.5 + 25*0:3),
                                labels =  c("Not\nApplicable",
                                            "Low",
                                            "Moderate",
                                            "High"),
                                name = NULL, expand = ggplot2::expansion())+
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(), labels = NULL,
                                breaks = NULL, name = NULL)+
    ggplot2::scale_fill_discrete(type = rev(c("#808080", "#008000", "#FF8C00",
                                              "#FF0000")), guide = "none")+
    ggplot2::theme_minimal()+
    ggplot2::theme(panel.border = ggplot2::element_rect(fill = NA),
                   axis.text = ggplot2::element_text(size = 12))
}



