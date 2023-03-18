library(shinytest2)

test_that("{shinytest2} recording: rankings", {
  app <- AppDriver$new(variant = platform_variant(), name = "rankings", seed = 532, height = 674, width = 1004)
  app$set_window_size(width = 1619, height = 944)
  app$set_inputs(season = "S01")
  app$expect_values(output = "ranking")
  app$expect_values(output = "ranking")
})



test_that("{shinytest2} recording: winner", {
  app <- AppDriver$new(variant = platform_variant(), name = "winner", height = 944, width = 1619)
  app$set_inputs(variant = platform_variant(), other_categories = "winner")
  app$set_inputs(queens = character(0))
  app$expect_values(output = "hometown")
})



test_that("{shinytest2} recording: performance", {
  app <- AppDriver$new(variant = platform_variant(), name = "performance", height = 944, width = 1619)
  app$set_inputs(age = c(48, 52))
  app$set_inputs(queens = character(0))
  app$expect_values(output = "queen_challenge")
})


test_that("{shinytest2} recording: outcomes", {
  app <- AppDriver$new(variant = platform_variant(), name = "outcomes", seed = 532, height = 1001, width = 1619)
  app$set_inputs(other_categories = "missc")
  app$set_inputs(queens = character(0))
  app$set_inputs(age = c(32, 39))
  app$expect_values(output = "outcome_table")
  app$expect_values(output = "outcome_table")
  app$expect_values(output = "outcome_table")
})
