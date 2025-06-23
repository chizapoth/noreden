library(dplyr)

test_that("return_new_diet creates a data frame with correct columns and values", {
  
  # Sample data for the function to process
  data_current_diet <- tibble::tibble(
    food_name = c("Food1", "Food2", "Food3"),
    intake_mean = c(100, 200, 150)
  )
  
  result_obj <- list(
    solution = c(110, 190, 160)  # Simulate new diet values
  )
  
  # Call the function with the sample data
  result <- return_new_diet(result_obj, data_current_diet)
  
  # Check if the result is a data frame
  expect_s3_class(result, "data.frame")
  
  # Check if the data frame has the correct columns
  expect_true(all(c("food_name", "new", "current") %in% colnames(result)))
  
  # Check if the 'food_name' column matches the current diet's 'food_name'
  expect_equal(result$food_name, data_current_diet$food_name)
  
  # Check if the 'new' values match the solution provided
  expect_equal(result$new, result_obj$solution)
  
  # Check if the 'current' values match the 'intake_mean' from the current diet
  expect_equal(result$current, data_current_diet$intake_mean)
})
