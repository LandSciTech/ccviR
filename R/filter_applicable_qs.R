filter_applicable_qs <- function(vuln_df, tax_grp, plant_taxa){
  # some qs should be NA for certain tax_grp
  C5a_unk <- vuln_df %>% filter(.data$Code == "C5a") %>% pull(.data$Value1) < 0
  C5b_unk <- vuln_df %>% filter(.data$Code == "C5b") %>% pull(.data$Value1) < 0

  vuln_df <- vuln_df  %>%
    mutate(score = case_when(.data$Code == "C4b" & tax_grp %in% c(plant_taxa, "Lichen") ~
                               NA_real_,
                             .data$Code == "C4c" & !tax_grp %in% plant_taxa ~ NA_real_,
                             .data$Code == "C5b" & !C5a_unk ~ NA_real_,
                             .data$Code == "C5c" & (!all(C5a_unk, C5b_unk) |
                                                      !tax_grp %in% plant_taxa) ~ NA_real_,
                             .default = .data$score))
  vuln_df
}

# Early filtering for assessing feedback on progress
filter_qs <- function(q, tax_grp, plant_taxa){
  # some qs should be NA for certain tax_grp
  C5a_unk <- q %>% filter(.data$Code == "C5a") %>% pull(.data$Value1) < 0
  C5b_unk <- q %>% filter(.data$Code == "C5b") %>% pull(.data$Value1) < 0

  q <- q  %>%
    mutate(
      Value1 = case_when(.data$Code == "C4b" & tax_grp %in% c(plant_taxa, "Lichen") ~
                           NA_real_,
                         .data$Code == "C4c" & !tax_grp %in% plant_taxa ~ NA_real_,
                         .data$Code == "C5b" & !C5a_unk ~ NA_real_,
                         .data$Code == "C5c" & (!all(C5a_unk, C5b_unk) |
                                                  !tax_grp %in% plant_taxa) ~ NA_real_,
                         .default = .data$Value1))
  q[is.na(q$Value1), c("Value2", "Value3", "Value4")] <- NA

  q
}
