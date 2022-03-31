
library(testthat)

test <- function(chart_function, is_comparison = F, is_line_chart = F) {

  ## ----------------------------  NON VISUAL TESTING --------------------------------------

  # Testing of df and group_names
  test_that("default parameters", {
    expect_is(chart_function(df = data_cluster, group_names = group_names), "gg")
  })

  test_that("df error", {
    expect_error(chart_function(df = NULL, group_names = group_names), regexp = ERROR_DF_WRONG_TYPE)
    expect_error(chart_function(df = "", group_names = group_names), regexp = ERROR_DF_WRONG_TYPE)
  })

  test_that("group_names error", {
    expect_error(chart_function(df = data_cluster, group_names = NULL), regexp = ERROR_GN_WRONG_TYPE)
  })

  ## -----

  # Testing of inner_label
  test_that("inner_label working cases", {
    expect_is(chart_function(df = data_cluster, group_names = group_names, inner_label = "green"), "gg")
  })

  test_that("inner_label error", {
    expect_error(chart_function(df = data_cluster, group_names = group_names, inner_label = NULL), regexp = ERROR_IL_WRONG_TYPE)
    expect_error(chart_function(df = data_cluster, group_names = group_names, inner_label = 123), regexp = ERROR_IL_WRONG_TYPE)
    expect_error(chart_function(df = data_cluster, group_names = group_names, inner_label = NA), regexp = ERROR_IL_WRONG_TYPE)
    expect_error(chart_function(df = data_cluster, group_names = group_names, inner_label = c(1,1)), regexp = ERROR_IL_WRONG_TYPE)
  })

  ## -----

  # Testing of color_inner_circle
  test_that("color_inner_circle working cases", {
    expect_is(chart_function(df = data_cluster, group_names = group_names, color_inner_circle = NULL), "gg") ## result: no color
    expect_is(chart_function(df = data_cluster, group_names = group_names, color_inner_circle = "green"), "gg") ## result: no color
    expect_is(chart_function(df = data_cluster, group_names = group_names, color_inner_circle = "123"), "gg")
    expect_is(chart_function(df = data_cluster, group_names = group_names, color_inner_circle = "1111111111111"), "gg") ## result: some color
  })

  test_that("color_inner_circle unknown color error", {
    expect_error(chart_function(df = data_cluster, group_names = group_names, color_inner_circle = ""), regexp = ERROR_CIC_UNKNOWN_COLOR)
    expect_error(chart_function(df = data_cluster, group_names = group_names, color_inner_circle = "blau"), regexp = ERROR_CIC_UNKNOWN_COLOR)
  })

  ## -----

  # Testing of scale_rng
  test_that("scale_rng working cases", {
    expect_is(chart_function(df = data_cluster, group_names = group_names, scale_rng = c(1,3)), "gg")
    expect_is(chart_function(df = data_cluster, group_names = group_names, scale_rng = c(1,3,5,-1,1000,0)), "gg")
  })

  test_that("scale_rng non numeric error", {
    expect_error(chart_function(df = data_cluster, group_names = group_names, scale_rng = NULL), regexp = ERROR_SR_NOT_NUMERIC)
    expect_error(chart_function(df = data_cluster, group_names = group_names, scale_rng = ""), regexp = ERROR_SR_NOT_NUMERIC)
  })

  test_that("scale_rng wrong order error", {
    expect_error(chart_function(df = data_cluster, group_names = group_names, scale_rng = c(1,-1)), regexp = ERROR_SR_WRONG_ORDER)
  })

  test_that("scale_rng same number error", {
    expect_error(chart_function(df = data_cluster, group_names = group_names, scale_rng = c(1,1)), regexp = ERROR_SR_SAME_VALUE)
  })

  ## -----

  # Testing of interactive
  test_that("interactive working cases", {
    expect_is(chart_function(df = data_cluster, group_names = group_names, interactive = TRUE), "girafe")
  })

  test_that("interactive error", {
    expect_error(chart_function(df = data_cluster, group_names = group_names, interactive = NULL), regexp = ERROR_INTACT_WRONG_TYPE)
    expect_error(chart_function(df = data_cluster, group_names = group_names, interactive = ""), regexp = ERROR_INTACT_WRONG_TYPE)
    expect_error(chart_function(df = data_cluster, group_names = group_names, interactive = 3), regexp = ERROR_INTACT_WRONG_TYPE)
  })

  ## -----

  # Testing of tooltip_bars
  test_that("tooltip_bars working cases", {
    expect_is(chart_function(df = data_cluster, group_names = group_names, interactive = TRUE, tooltip_bars = "sd"), "girafe")
    expect_is(chart_function(df = data_cluster, group_names = group_names, interactive = TRUE, tooltip_bars = 5), "girafe")
    expect_is(chart_function(df = data_cluster, group_names = group_names, interactive = TRUE, tooltip_bars = "test"), "girafe")
    expect_is(chart_function(df = data_cluster, group_names = group_names, interactive = TRUE, tooltip_bars = FALSE), "girafe")
    expect_is(chart_function(df = data_cluster, group_names = group_names, interactive = TRUE, tooltip_bars = TRUE), "girafe")
  })

  test_that("tooltip_bars error", {
    expect_error(chart_function(df = data_cluster, group_names = group_names, interactive = TRUE, tooltip_bars = NULL), regexp = ERROR_TTB_NULL_TYPE)
  })

  ## -----

  label = c("oven")
  description = c("test")
  ttl <- data.frame(label, description)
  # Testing of tooltip_labels
  test_that("tooltip_labels working cases", {
    expect_is(chart_function(df = data_cluster, group_names = group_names, interactive = TRUE, tooltip_labels = ttl), "girafe")
  })

  test_that("tooltip_labels wrong type error", {
    expect_error(chart_function(df = data_cluster, group_names = group_names, interactive = TRUE, tooltip_labels = "NULL"), regexp = ERROR_TTL_WRONG_TYPE)
    expect_error(chart_function(df = data_cluster, group_names = group_names, interactive = TRUE, tooltip_labels = c(1,1)), regexp = ERROR_TTL_WRONG_TYPE)
  })

  a = c("oven")
  b = c("test")
  ttl <- data.frame(a, b)
  test_that("tooltip_labels missing column error", {
    expect_error(chart_function(df = data_cluster, group_names = group_names, interactive = TRUE, tooltip_labels = ttl), regexp = ERROR_TTL_MISSING_COLUMN)
  })

  ## -----

  # Testing of show_group_names
  test_that("show_group_names working cases", {
    expect_is(chart_function(df = data_cluster, group_names = group_names, show_group_names = FALSE), "gg")
  })

  test_that("show_group_names error", {
    expect_error(chart_function(df = data_cluster, group_names = group_names, show_group_names = NULL), regexp = ERROR_SGN_WRONG_TYPE)
    expect_error(chart_function(df = data_cluster, group_names = group_names, show_group_names = ""), regexp = ERROR_SGN_WRONG_TYPE)
    expect_error(chart_function(df = data_cluster, group_names = group_names, show_group_names = 3), regexp = ERROR_SGN_WRONG_TYPE)
  })

  ## -----

  if (is_comparison) {
    # Testing of delta_threshold
    test_that("delta_threshold working cases", {
      expect_is(chart_function(df = data_cluster, group_names = group_names, delta_threshold = 0.37), "gg")
    })

    test_that("delta_threshold error", {
      expect_error(chart_function(df = data_cluster, group_names = group_names, delta_threshold = NULL), regexp = ERROR_DT_WRONG_TYPE)
      expect_error(chart_function(df = data_cluster, group_names = group_names, delta_threshold = "test"), regexp = ERROR_DT_WRONG_TYPE)
      expect_error(chart_function(df = data_cluster, group_names = group_names, delta_threshold = 1.1), regexp = ERROR_DT_NOT_IN_RANGE)
      expect_error(chart_function(df = data_cluster, group_names = group_names, delta_threshold = 0), regexp = ERROR_DT_NOT_IN_RANGE)
    })

    ## -----

    # Testing of id
    test_that("id working cases", {
      expect_is(chart_function(df = data_cluster %>% rename(test_id = .id), group_names = group_names, id = "test_id"), "gg")
    })

    test_that("id error", {
      expect_error(chart_function(df = data_cluster, group_names = group_names, id = NULL), regexp = ERROR_DF_MISSING_COLUMN_ID)
      expect_error(chart_function(df = data_cluster, group_names = group_names, id = "no"), regexp = ERROR_DF_MISSING_COLUMN_ID)
    })

    ## -----

    # Testing of phase
    test_that("phase working cases", {
      expect_is(chart_function(df = data_cluster %>% rename(test_phase = .phase), group_names = group_names, phase = "test_phase"), "gg")
    })

    test_that("phase error", {
      expect_error(chart_function(df = data_cluster, group_names = group_names, phase = NULL), regexp = ERROR_DF_MISSING_COLUMN_PHASE)
      expect_error(chart_function(df = data_cluster, group_names = group_names, phase = "no"), regexp = ERROR_DF_MISSING_COLUMN_PHASE)
    })

    ## -----
    test_that("feature_names working cases", {
      expect_is(chart_function(df = data_cluster, group_names = group_names, feature_names = setdiff(colnames(data_cluster),c(".id",".phase"))), "gg")
      expect_is(chart_function(df = data_cluster, group_names = group_names, feature_names = NULL), "gg")
    })

    test_that("feature_names error", {
      expect_error(chart_function(df = data_cluster, group_names = group_names, feature_names = c("test", "oven", "test2")), regexp = ERROR_DF_MISSING_FEATURE_NAME)
      expect_error(chart_function(df = data_cluster, group_names = group_names, feature_names = 3), regexp = ERROR_DF_MISSING_FEATURE_NAME)
      expect_error(chart_function(df = data_cluster, group_names = group_names, feature_names = "test"), regexp = ERROR_DF_MISSING_FEATURE_NAME)
    })
  }

  if (is_line_chart) {

    test_that("cluster error", {
      expect_error(chart_function(df = data_cluster, group_names = group_names, cluster = NULL), regexp = ERROR_DF_MISSING_COLUMN)
      expect_error(chart_function(df = data_cluster, group_names = group_names, cluster = "no"), regexp = ERROR_DF_MISSING_COLUMN)
    })

    ## -----

    # Testing of color_clusters
    test_that("color_clusters working cases", {
      expect_is(chart_function(df = data_cluster, group_names = group_names, color_clusters = "green"), "gg") ## result: no color
      expect_is(chart_function(df = data_cluster, group_names = group_names, color_clusters = "123"), "gg")
      expect_is(chart_function(df = data_cluster, group_names = group_names, color_clusters = "1111111111111"), "gg") ## result: some color
    })

    test_that("color_clusters unknown color error", {
      expect_error(chart_function(df = data_cluster, group_names = group_names, color_clusters = ""), regexp = ERROR_CC_UNKNOWN_COLOR)
      expect_error(chart_function(df = data_cluster, group_names = group_names, color_clusters = "blau"), regexp = ERROR_CC_UNKNOWN_COLOR)
    })
  }
}

## ----------------------------  TESTING SETUP --------------------------------------

library(tidyverse)
library(ggiraph)

mock = create_mockdata(Nf=55,n_treatments = 2,ngroups = 5)
df = mock$dfs
grs = mock$grs

df <- df %>%
  mutate(.cluster = factor(.cluster))
data_cluster <- df %>% mutate(.cluster = fct_drop(as.factor(.cluster))) %>%
  select(-c(.cluster, .phase, .id))

group_names = grs$.gr_name

radial_barchart_static(data_cluster, group_names,interactive = T)
test(chart_function = radial_barchart_static)

# -----

df = mock$dfs
grs = mock$grs


df <- df %>%
  mutate(.cluster = factor(.cluster)) %>%
  filter(.cluster == levels(.cluster)[2L])

data_cluster <- df %>% mutate(.cluster = fct_drop(as.factor(.cluster))) %>% select(-c(.cluster))

group_names = grs$.gr_name
test(chart_function = radial_barchart_compare, is_comparison = T)

## -----

mock = create_mockdata(Nf=55,n_treatments =1,ngroups = 5)
df = mock$dfs
grs = mock$grs



data_cluster <- df %>% mutate(.cluster = fct_drop(as.factor(.cluster))) %>%
  select(-c(.phase, .id))

test(chart_function = radial_line_chart, is_line_chart = T)
