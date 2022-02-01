## code to prepare `vulnq_code_lu_tbl` dataset goes here

# made a csv with the codes and questions but need to tidy it up

lu_tbl <- read.csv("../CCVI_analysis/data/Code_Q_lookup.csv")

vulnq_code_lu_tbl <- mutate_all(lu_tbl, stringr::str_trim) %>%
  mutate(Question = stringr::str_remove(Question, "^..?\\)\\s") %>%
           stringr::str_to_sentence())


usethis::use_data(vulnq_code_lu_tbl, overwrite = TRUE)
usethis::use_data(vulnq_code_lu_tbl, overwrite = TRUE, internal = TRUE)
