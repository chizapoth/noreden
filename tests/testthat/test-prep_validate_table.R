library(dplyr)

test_that("prep_validate_table produces the correct output", {
  
  # Sample data for the function to process
  data_validate_diet <- tibble::tibble(
    tag_outcome = c("Outcome1", "Outcome2", "Outcome3"),
    total_contrib_new = c(120, 150, 130),
    total_contrib = c(100, 160, 120),
    constr_lwr = c(90, 140, 110),
    constr_upr = c(130, 170, 140),
    check = c("beyond_upper", "Ok", "beyond_lower"),
    deviation = c(10, 0, 5)
  )
  
  # Run the function
  result <- prep_validate_table(data_validate_diet)
  
  # Extract the output table
  td <- result$gt_data
  
  # Check if the result is a data frame
  expect_s3_class(td, "data.frame")
  
  # Check that the selected columns are present
  expect_true(all(c("tag_outcome", "tc_new", "tc_current", "min", "max", "check") %in% colnames(td)))
  
  # Check that numeric columns are rounded to 2 decimal places
  expect_equal(round(td$tc_new, 2), td$tc_new)
  expect_equal(round(td$tc_current, 2), td$tc_current)
  expect_equal(round(td$min, 2), td$min)
  expect_equal(round(td$max, 2), td$max)
  
  # Check that the 'check' column has been updated correctly
  expect_equal(td$check[1], "10% above")  # First row: beyond_upper
  expect_equal(td$check[2], "Ok")        # Second row: Ok
  expect_equal(td$check[3], "5% below")  # Third row: beyond_lower
  
  # Check that the 'deviation' column is removed
  expect_false("deviation" %in% colnames(td))
  
  # Check that the result contains the original data in the list
  expect_equal(result$data_validate_diet, data_validate_diet)
  
})