
test_that("single plot works", {
  v <- calc_vulnerability(test_exp_tbl(1),
                          make_vuln_df("nm", 0),
                          tax_grp = "Bird") %>%
    suppressMessages() %>%
    select(scenario_name, vuln_df) %>%
    tidyr::unnest(vuln_df)

  expect_silent(p <- plot_q_score(v))
  expect_s3_class(p, "plotly")
})

test_that("multi plot works", {
  exp_df2 <- bind_rows(test_exp_tbl(1), test_exp_tbl(2))
  exp_df2[2,1] <- "Scn2"
  v <- calc_vulnerability(exp_df2,
                          make_vuln_df("nm", 2, use_spatial = FALSE),
                          tax_grp = "Bird") %>%
    suppressMessages() %>%
    select(scenario_name, vuln_df) %>%
    tidyr::unnest(vuln_df)

  expect_silent(p <- plot_q_score(v))
  expect_s3_class(p, "plotly")

  # with 3 scenarios
  exp_df3 <- bind_rows(exp_df2, test_exp_tbl(3))
  exp_df3[3,1] <- "Scn3"
  v <- calc_vulnerability(exp_df3,
                          make_vuln_df("nm", 2, use_spatial = FALSE),
                          tax_grp = "Bird") %>%
    suppressMessages() %>%
    select(scenario_name, vuln_df) %>%
    tidyr::unnest(vuln_df)

  expect_silent(p <- plot_q_score(v))
  expect_s3_class(p, "plotly")
})
