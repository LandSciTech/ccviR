# Test that results align for NS and ccviR calculated indexs

# tried openxlsx and xlsx but could not get them to write to the NS spreadsheet
# set up a macro to calculate Index based on data copied into the Results Table

# Steps for testing

# 1) Create a table with all possible values for each vulnerability factor.
# Exposure should just include a range across the thresholds. There are too many
# possible combinations of each factor so take a random sample.

# 2) translate the table into correct format for calc_vulnerability() and
# spreadsheet

# 3) calculate the index and compare. Investigate reasons for differences. There
# have been some differences recorded already so will have to confirm that ours
# is good.

# 0 Setup #=======================================
library(dplyr)
library(ccviR)
library(tidyr)
library(stringr)
library(purrr)

# 1 Create Table #================================
# possible values of sensitivity factors
valueNms <- c("Greatly increase", "Increase", "Somewhat increase", "Neutral", "Unknown")
valueOpts <- c(3, 2, 1, 0, -1)

names(valueOpts) <- valueNms

multValueOpts <- expand.grid(valueOpts[1:4], valueOpts[1:4]) %>%
  filter(Var1 != Var2)

vuln_value_table <- vulnq_code_lu_tbl %>% select(Code) %>%
  mutate(vals = list(multValueOpts)) %>%
  unnest(vals) %>%
  rowwise() %>%
  mutate(vals = sort(c(Var1, Var2)) %>%
              paste0(collapse = "-")) %>%
  pivot_wider(names_from = "Code", values_from = "vals") %>%
  select(-Var1, -Var2) %>%
  distinct() %>%
  bind_rows(
    expand.grid(vulnq_code_lu_tbl$Code, valueOpts) %>%
      mutate(var3 = Var2,
             Var2 = as.character(Var2)) %>%
      pivot_wider(names_from = "Var1", values_from = "Var2") %>%
      select(-var3)
  )

# remove 3s from qs with max of 2
max2 <- filter(vulnq_code_lu_tbl, Max_Value == 2) %>% pull(Code)

# NAs are omitted in sampling below
vuln_value_table <- vuln_value_table %>%
  mutate(across(.cols = all_of(max2), ~ifelse(str_detect(.x, "3"), NA_character_, .x)))

# possible values of exposure factors

# there are 6 different classes can sample this for both of temp and moist
exp_value_table <- expand.grid(Class_1 = 1:10/10, Class_2 = 1:10/10, Class_3 = 1:10/10,
                               Class_4 = 1:10/10, Class_5 = 1:10/10, Class_6 = 1:10/10) %>%
  mutate(total = rowSums(across(where(is.numeric)))) %>%
  filter(total == 1) %>%
  select(-total)

ccei_value_table <- expand.grid(Class_1 = 1:10/10, Class_2 = 1:10/10, Class_3 = 1:10/10,
                                Class_4 = 1:10/10) %>%
  mutate(total = rowSums(across(where(is.numeric)))) %>%
  filter(total == 1) %>%
  select(-total)

# get random sample from tables
N <- 2
mat <- slice_sample(exp_value_table, n = N) %>%
  rename_with(~str_replace(.x, "Class", "MAT"))
cmd <- slice_sample(exp_value_table, n = N) %>%
  rename_with(~str_replace(.x, "Class", "CMD"))
ccei <- slice_sample(ccei_value_table, n = N) %>%
  rename_with(~str_replace(.x, "Class", "CCEI"))

exp_facts <- bind_cols(mat, cmd, ccei)

vuln_facts <- map_dfc(vuln_value_table, ~sample(na.omit(.x), N))

# other factors
cave <- sample(c(0,0,0,0,1), N)
mig <- sample(c(0,0,0,0,1), N)

taxa <- c("Vascular Plant", "Nonvascular Plant", "Lichen", "Invert-Insect",
          "Invert-Mollusk", "Invert-Other", "Fish", "Amphibian", "Reptile",
          "Mammal", "Bird") %>%
  sample(N)

sp_nm <- paste0("Species_", 1:N)

# 2) Format tables #============================================================

# For spreadsheet
# B1	B1	B2a	B2a	B2b	B2b	B3	B3	C1	C1	C2ai	C2ai	C2aii	C2aii	C2bi	C2bi	C2bii	C2bii	C2c	C2c	C2d	C2d	C3	C3	C4a	C4a	C4b	C4b	C4c	C4c	C4d	C4d	C4e	C4e	C4f	C4f	C4g	C4g	C5a	C5a	C5b	C5b	C5c	C5c	C6	C6	D1	D1	D2	D2	D3	D3	D4	D4

NS_table <- data.frame(`Taxonomic Group` = taxa,
                       Species = sp_nm,
                       `English Name` = "",
                       `Geographic Area` = "area",
                       Cave/GW = ifelse(cave, "X", ""),
                       Migratory = ifelse(mig, "X", ""),
                       GRank = "",
                       SRank = "",
                       `A >6.0F` = pull(exp_facts, MAT_1),
                       `A 5.5F` = pull(exp_facts, MAT_2),
                       `A 5.1F` = pull(exp_facts, MAT_3),
                       `A 4.5F` = pull(exp_facts, MAT_4),
                       `A 3.9F` = pull(exp_facts, MAT_5),
                       `A <3.9F` = pull(exp_facts, MAT_6),
                       `< -0.119` = pull(exp_facts, CMD_1),
                       `-0.119` = pull(exp_facts, CMD_2),
                       `-0.096` = pull(exp_facts, CMD_3),
                       `-0.073` = pull(exp_facts, CMD_4),
                       `-0.05` = pull(exp_facts, CMD_5),
                       `>-0.028` = pull(exp_facts, CMD_6),
                       `>7` = pull(exp_facts, CCEI_1),
                       `6-7` = pull(exp_facts, CCEI_2),
                       `4-5` = pull(exp_facts, CCEI_3),
                       `2-3` = pull(exp_facts, CCEI_4))

# Need to convert vuln_facts to text abbreviations and add comment columns then cbind on to above




