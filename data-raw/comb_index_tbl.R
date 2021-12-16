# Table to combine index values from d and bc sections based on NatureServe Excel sheet

comb_index_tbl <- data.frame(Dindex = c("EV", "HV", "MV", "LV", "IE"),
                             bc_ev = c("EV", "EV", "HV", "HV", "EV"),
                             bc_hv = c("EV", "HV", "HV", "MV", "HV"),
                             bc_mv = c("HV", "HV", "MV", "MV", "MV"),
                             bc_lv = c("HV", "MV", "LV", "LV", "LV"),
                             bc_ie = c("EV", "HV", "MV", "LV", "IE"),
                             stringsAsFactors = FALSE)

usethis::use_data(comb_index_tbl, overwrite = TRUE)
