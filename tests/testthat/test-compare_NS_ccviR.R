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
library(readxl)

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
exp_value_table <- expand.grid(Class_1 = 1:10*10, Class_2 = 1:10*10, Class_3 = 1:10*10,
                               Class_4 = 1:10*10, Class_5 = 1:10*10, Class_6 = 1:10*10) %>%
  mutate(total = rowSums(across(where(is.numeric)))) %>%
  filter(total == 100) %>%
  select(-total)

ccei_value_table <- expand.grid(Class_1 = 1:10*10, Class_2 = 1:10*10, Class_3 = 1:10*10,
                                Class_4 = 1:10*10) %>%
  mutate(total = rowSums(across(where(is.numeric)))) %>%
  filter(total == 100) %>%
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

## For spreadsheet
NS_table <- tibble(`Taxonomic Group` = taxa,
                       Species = sp_nm,
                       `English Name` = "",
                       `Geographic Area` = "area",
                       `Cave/GW` = ifelse(cave, "X", ""),
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

# Need to convert vuln_facts to text abbreviations and add comment columns then
# cbind on to above
vuln_facts_NS <- vuln_facts %>%
  mutate(across(everything(),
                ~str_replace(.x, "^-1", "U") %>% str_replace("0", "N") %>%
                  str_replace("1", "SI") %>% str_replace("2", "Inc") %>%
                  str_replace("3", "GI")))

vuln_facts_NS[,paste0(names(vuln_facts_NS), "_comments")] <- ""

vuln_facts_NS <- select(vuln_facts_NS, order(colnames(vuln_facts_NS)))

NS_table <- NS_table %>% bind_cols(vuln_facts_NS)

## For calc_vulnerability

vuln_facts_ccviR <- vuln_facts %>%
  mutate(Species = sp_nm, Z2 = as.character(cave),
         Z3 = as.character(mig)) %>%
  pivot_longer(-Species, names_to = "Code", values_to = "vals") %>%
  separate(vals, into = c(paste0("Value", 1:4)), fill = "right", sep = "(?<=\\d)-") %>%
  mutate(across(contains("Value"), as.numeric)) %>%
  split(.$Species)

exp_ccviR <- exp_facts %>% mutate(species = sp_nm, tax_grp = taxa)
cols_to_add <- c(paste0("HTN_", 1:4), "PTN", "MAP_max", "MAP_min", "perc_lost", "perc_maint")
exp_ccviR[,cols_to_add] <- NA_real_
exp_ccviR <- split(exp_ccviR, exp_ccviR$species)


# this is circumventing the conversion from spatial data to vuln qs for non
# exposure spatial components

# 3) calculate the index #======================================================
## For NS
# copy the table to clipboard
write.table(NS_table, "clipboard", sep="\t", row.names=FALSE)

# open the excel file
shell.exec(file.path(getwd(), "tests/testthat/data",
                     "For_Comparison_ccvi_release_3.02_1_jun_2016_0.xlsm"))

# paste the table into the Results Table sheet, make sure the column names line
# up and then remove the pasted ones. Select all the species names and click
# Calculate Index from Table. Return to RStudio

NS_results <- read_excel(file.path("tests/testthat/data",
                                   "For_Comparison_ccvi_release_3.02_1_jun_2016_0.xlsm"),
                         sheet = "Results Table", range = "A5:CI200") %>%
  filter(!is.na(Index)) %>%
  slice(-1) # remove first row which is a duplicate

names(NS_results)[1:ncol(NS_table)] <- names(NS_table)

## For ccviR
ccviR_results <- map2(vuln_facts_ccviR, exp_ccviR,
                      ~calc_vulnerability(exp_df = .y, vuln_df = .x,
                                          tax_grp = .y$tax_grp))

# make into a table similar to NS_table
ccviR_results_table <- tibble(Species = sp_nm,
                              Index = map_chr(ccviR_results, "index"),
                              `Migr Exp` = map_chr(ccviR_results, "mig_exp") %>%
                                ifelse(. == "N/A", "--", .),
                              Confidence = map_chr(ccviR_results, "conf_index")%>%
                                ifelse(. == "Very High", "VH", .) %>%
                                ifelse(. == "Moderate", "Mod", .),
                              B = map_chr(ccviR_results, "n_b_factors") %>%
                                paste0("/4 factors"),
                              C = map_chr(ccviR_results, "n_c_factors") %>%
                                paste0("/16 factors"),
                              D = map_chr(ccviR_results, "n_d_factors") %>%
                                paste0("/4 factors"),
                              BC_Subscore = map_dbl(ccviR_results, "b_c_score"),
                              D_Subscore = map_dbl(ccviR_results, "d_score"))

compare_table <- ccviR_results_table %>%
  mutate(method = "ccviR") %>%
  bind_rows(mutate(NS_results, method = "NatureServe") %>%
              select(names(ccviR_results_table), method)) %>%
  mutate(across(where(is.numeric), as.character)) %>%
  pivot_longer(cols = -c(Species, method), names_to = "Variable", values_to = "Values") %>%
  pivot_wider(names_from = method, values_from = Values)

mismatch <- filter(compare_table, ccviR != NatureServe)

write.csv(mismatch, "tests/testthat/data/compare_result.csv")
