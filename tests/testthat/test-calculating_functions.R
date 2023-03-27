test_that("calcul aggregate", {
  df_test <- as.data.table(USArrests)

  # make group var
  breaks <- as.vector(summary(USArrests$UrbanPop))

  df_test[, density := fcase(
    UrbanPop <= breaks[2], "low",
    UrbanPop > breaks[2] & UrbanPop <= breaks[4], "medium",
    UrbanPop > breaks[4], "strong")]

  num_vars <- setdiff(names(df_test), "density")

  # calcul
  df_calcul <- agreg_df(df_input = df_test, names_vars_by = "density", names_vars_num = num_vars)

  # tests
  testthat::expect_equal(any(class(df_calcul) %in% c("data.table", "data.frame")), TRUE)
})
