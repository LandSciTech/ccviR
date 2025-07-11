Exploring CCEI

library(terra)
library(purrr)

path_ccei <- fs::path_package("extdata", "ccei_test", package = "ccviR")

scenario <-  "ssp585"

f_paths <- combine_future(path_ccei)

f_rasts <- map(f_paths$file, \(x)rast(x) %>% sum()) %>%
  set_names(paste0(f_paths$model,"_", f_paths$var)) %>%
  rast()

plot(f_rasts %>% subset(stringr::str_which(names(f_rasts), "prec")))
plot(f_rasts %>% subset(stringr::str_which(names(f_rasts), "tmax")))
plot(f_rasts %>% subset(stringr::str_which(names(f_rasts), "tmin")))

f_vals <- values(f_rasts, dataframe = TRUE)

f_vals %>% mutate(id = 1:n()) %>% slice(1:50) %>%
  tidyr::pivot_longer(cols = -id) %>%
  tidyr::separate(name, into = c("name", "var"), sep = "_") %>%
  ggplot2::ggplot(ggplot2::aes(x = id, y = value+0.001, col = name))+
  ggplot2::geom_point()+
  # ggplot2::scale_y_log10()+
  ggplot2::scale_colour_brewer(palette = "Paired")+
  ggplot2::facet_wrap(~var, scales = "free_y")

m <- fs::dir_ls(fs::path(path_ccei, "intermediate"),
                regexp = paste0("future(.+)", scenario))

var_rasts <- map(m, rast) %>%
  set_names(stringr::str_extract(names(m), "C.*future_(.*).tif", group = 1))

cmd_rasts <- var_rasts %>% map(\(x)(terra::subset(x, "cmd"))) %>% rast()

tmean_rasts <- var_rasts %>% map(\(x)(terra::subset(x, "tmean"))) %>% rast()

cmd_vals <- values(cmd_rasts, dataframe = TRUE)

cmd_vals %>% mutate(id = 1:n()) %>% slice(1:50) %>%
  tidyr::pivot_longer(cols = -id) %>%
  ggplot2::ggplot(ggplot2::aes(x = id, y = value+0.001, col = name))+
  ggplot2::geom_point()+
  # ggplot2::scale_y_log10()+
  ggplot2::scale_colour_brewer(palette = "Paired")

# useing ccei for each scenario when debugging calc_ccei
ccei_df <- purrr::list_cbind(purrr::map(ccei, as.data.frame)) %>%
  purrr::set_names(names(ccei)) %>%
  rename_with(\(x)stringr::str_remove(x, "C.*future_"))%>%
  rename_with(\(x)stringr::str_remove(x, ".tif\\$.x\\[\\[i\\]\\]"))

ccei_df %>% mutate(id = 1:n()) %>% slice(1:50) %>%
  tidyr::pivot_longer(cols = -id) %>%
  ggplot2::ggplot(ggplot2::aes(x = id, y = value$`.x[[i]]`, col = name))+
  ggplot2::geom_point()+
  ggplot2::scale_y_log10()

# General exploration of CMD

cmd_NA <- terra::rast("../Climate_data/data/NA_NORM_6190_Bioclim_ASCII/CMD.asc")

plot(cmd_NA, breaks = c(0, 0.0001, 5, 20, 50, 100, 500, 1000, Inf))

cmd_NA_f <- terra::rast("../Climate_data/data/NA_ENSEMBLE_rcp45_2050s_Bioclim_ASCII/CMD.asc")

plot(cmd_NA_f, breaks = c(0, 0.0001, 5, 20, 50, 100, 500, 1000, Inf))

cmd_WH <- rast("misc/ccei/intermediate/hist_all_vars.tif") |> subset("cmd_mean")

plot(cmd_WH, breaks = c(0, 0.0001, 5, 20, 50, 100, 500, 1000, Inf))

cmd_WH_f <- rast("misc/ccei/intermediate/future_groups_cmd.tif")

plot(mean(cmd_WH_f), breaks = c(0, 0.0001, 5, 20, 50, 100, 500, 1000, Inf))
