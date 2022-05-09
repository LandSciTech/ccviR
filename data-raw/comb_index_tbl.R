# Table to combine index values from d and bc sections based on NatureServe Excel sheet

comb_index_tbl <- data.frame(Dindex = c("EV", "HV", "MV", "LV", "IE"),
                             bc_ev = c("EV", "EV", "HV", "HV", "EV"),
                             bc_hv = c("EV", "HV", "HV", "MV", "HV"),
                             bc_mv = c("HV", "HV", "MV", "MV", "MV"),
                             bc_lv = c("HV", "MV", "LV", "LV", "LV"),
                             bc_ie = c("EV", "HV", "MV", "LV", "IE"),
                             stringsAsFactors = FALSE)

levs <- c("EV", "HV", "MV", "LV", "IE")

comb_index_tbl <- comb_index_tbl %>% tidyr::pivot_longer(-Dindex) %>%
  transmute(Dindex,
            Bindex = name %>% stringr::str_remove("bc_") %>% toupper(),
            value = factor(value, levels = levs))

usethis::use_data(comb_index_tbl, overwrite = TRUE)
usethis::use_data(comb_index_tbl, overwrite = TRUE, internal = TRUE)
