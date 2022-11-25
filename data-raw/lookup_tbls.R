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


## code to prepare `vulnq_code_lu_tbl` dataset goes here

# made a csv with the codes and questions but need to tidy it up

lu_tbl <- read.csv("../CCVI_analysis/data/Code_Q_lookup.csv")

vulnq_code_lu_tbl <- mutate_all(lu_tbl, stringr::str_trim) %>%
  mutate(Question = stringr::str_remove(Question, "^..?\\)\\s") %>%
           stringr::str_to_sentence())


usethis::use_data(vulnq_code_lu_tbl, overwrite = TRUE)

# table for updating inputs based on saved file
ui_build_table <- read.csv("data-raw/ui_build_table.csv")

# create the update fun from input fun
ui_build_table <- ui_build_table %>%
  mutate(update_fun = gsub("(^)([[:alpha:]])", "update\\U\\1\\2", input_fun ,perl = TRUE))


usethis::use_data(ui_build_table, overwrite = TRUE)

# need to include all object for internal because they are saved together.
usethis::use_data(vulnq_code_lu_tbl, comb_index_tbl, ui_build_table, overwrite = TRUE, internal = TRUE)
