
test_that("plot_conf_score works for single", {
  v <- calc_vulnerability(test_exp_tbl(1),
                          make_vuln_df("nm", 0),
                          tax_grp = "Bird") %>%
    suppressMessages()
  expect_silent(p <- plot_conf_score(v))
  expect_s3_class(p, "ggplot")
})

test_that("plot_conf_score multi plot works", {
  exp_df2 <- bind_rows(test_exp_tbl(1), test_exp_tbl(2))
  exp_df2[2,1] <- "Scn2"
  exp_df2$range_change <- c(NA, 50)
  exp_df2$range_overlap <- c(NA, 20)

  v <- calc_vulnerability(exp_df2,
                          make_vuln_df("nm", 1),
                          tax_grp = "Bird") %>%
    suppressMessages()

  expect_silent(p <- plot_conf_score(v))
  expect_s3_class(p, "ggplot")

  v <- calc_vulnerability(exp_df2,
                          make_vuln_df("nm", rep(c(0,1), length.out = 27),
                                       rep(c(3,2), length.out = 27)),
                          tax_grp = "Bird") %>%
    suppressMessages()

  expect_silent(p <- plot_conf_score(v))
  expect_s3_class(p, "ggplot")
})
