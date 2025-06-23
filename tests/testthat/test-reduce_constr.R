# Sample dataset
data_constr_coef <- data.frame(
  tag_outcome = c("ghge", "protein", "fiber"),
  coef_constrlwr = c(100, 50, 30),
  coef_construpr = c(200, 100, 60)
)

test_that("reduce_constr correctly reduces constraints for a given outcome", {
  result <- reduce_constr(data_constr_coef, tag_outcome_reduce = "ghge", coef_reduce = 0.8)
  
  expect_equal(nrow(result), 3)  # Same number of rows
  expect_equal(result$coef_constrlwr[result$tag_outcome == "ghge"], 80)  # 100 * 0.8
  expect_equal(result$coef_construpr[result$tag_outcome == "ghge"], 160) # 200 * 0.8
})

test_that("reduce_constr does not modify other outcomes", {
  result <- reduce_constr(data_constr_coef, tag_outcome_reduce = "ghge", coef_reduce = 0.8)
  
  expect_equal(result$coef_constrlwr[result$tag_outcome == "protein"], 50)  # Should remain unchanged
  expect_equal(result$coef_construpr[result$tag_outcome == "fiber"], 60)  # Should remain unchanged
})

test_that("reduce_constr handles a reduction factor of 1 (no change)", {
  result <- reduce_constr(data_constr_coef, tag_outcome_reduce = "ghge", coef_reduce = 1)
  
  expect_equal(result$coef_constrlwr[result$tag_outcome == "ghge"], 100)  # No change
  expect_equal(result$coef_construpr[result$tag_outcome == "ghge"], 200)  # No change
})

test_that("reduce_constr returns the same dataframe if outcome is not found", {
  result <- reduce_constr(data_constr_coef, tag_outcome_reduce = "sodium", coef_reduce = 0.8)
  
  expect_equal(result, data_constr_coef)  # No changes should be made
})

test_that("reduce_constr handles an empty dataframe correctly", {
  empty_data <- tibble::tibble(tag_outcome = character(0), coef_constrlwr = numeric(0), coef_construpr = numeric(0))
  
  result <- reduce_constr(empty_data, tag_outcome_reduce = "ghge", coef_reduce = 0.8)
  
  expect_equal(nrow(result), 0)  # Should return an empty dataframe
  expect_equal(colnames(result), colnames(empty_data))  # Column names should match
})

test_that("reduce_constr handles edge cases with zero and negative reduction factors", {
  result_zero <- reduce_constr(data_constr_coef, tag_outcome_reduce = "ghge", coef_reduce = 0)
  expect_equal(result_zero$coef_constrlwr[result_zero$tag_outcome == "ghge"], 0)  # Should be zero
  expect_equal(result_zero$coef_construpr[result_zero$tag_outcome == "ghge"], 0)  # Should be zero
  
  result_negative <- reduce_constr(data_constr_coef, tag_outcome_reduce = "ghge", coef_reduce = -1)
  expect_equal(result_negative$coef_constrlwr[result_negative$tag_outcome == "ghge"], -100)  # Negative values
  expect_equal(result_negative$coef_construpr[result_negative$tag_outcome == "ghge"], -200)  # Negative values
})