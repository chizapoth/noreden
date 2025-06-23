library(dplyr)

test_that("compute_constr correctly calculates constraint contributions", {
  # Sample input data
  data_total_contrib <- tibble::tibble(
    tag_outcome = c("vitamin_c", "fiber"),
    total_contrib = c(100, 200)
  )
  
  data_constr_coef <- tibble::tibble(
    tag_outcome = c("vitamin_c", "fiber"),
    coef_constrlwr = c(0.8, 0.9),  # Example lower constraint coefficients
    coef_construpr = c(1.2, 1.1)   # Example upper constraint coefficients
  )
  
  # Manually compute expected results for constraints
  expected_constr_lwr <- data_total_contrib$total_contrib * data_constr_coef$coef_constrlwr
  expected_constr_upr <- data_total_contrib$total_contrib * data_constr_coef$coef_construpr
  
  # Run function
  result <- compute_constr(data_total_contrib, data_constr_coef)
  
  # Check output structure
  expect_equal(nrow(result), 2)  # Should have 2 rows (for vitamin_c and fiber)
  expect_equal(colnames(result), c("tag_outcome", "total_contrib", "coef_constrlwr", "coef_construpr", "constr_lwr", "constr_upr"))  # Correct columns
  
  # Check computed constraint values
  expect_equal(result$constr_lwr, expected_constr_lwr)  # Lower constraint should match
  expect_equal(result$constr_upr, expected_constr_upr)  # Upper constraint should match
  
  # Check that the input data is properly merged
  expect_equal(result$tag_outcome, data_total_contrib$tag_outcome)  # tag_outcome should be consistent
})
