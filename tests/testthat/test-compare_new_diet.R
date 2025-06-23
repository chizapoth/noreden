library(dplyr)

test_that("compare_new_diet produces correct output", {
  
  # Sample current diet data (including food_name, intake_mean, intake_lwr, and intake_upr)
  data_current_diet <- tibble::tibble(
    food_name = c("Food1", "Food2", "Food3"),
    intake_mean = c(100, 200, 150),
    intake_lwr = c(90, 180, 140),
    intake_upr = c(110, 220, 160)
  )
  
  # Sample new diet data (including food_name and new diet values)
  data_new_diet <- tibble::tibble(
    food_name = c("Food1", "Food2", "Food3"),
    new = c(110, 180, 160), 
    current = c(100, 200, 150)
  )
  
  # Call the compare_new_diet function
  result <- compare_new_diet(data_new_diet, data_current_diet)
  
  # Check if the result is a data frame
  expect_s3_class(result, "data.frame")
  
  # Check if the data frame has the correct columns
  expect_true(all(c("food_name", "new", "current", "current_lwr", "current_upr", "abs_change", "percent_change") %in% colnames(result)))
  
  # Check if the values are correct for abs_change and perc_change
  expect_equal(result$abs_change, c(10, -20, 10))
  expect_equal(result$percent_change, c(0.10, -0.10, 0.07), tolerance = 0.01)
  
  # Check if the current values are correctly assigned
  expect_equal(result$current, data_current_diet$intake_mean)
  
  # Check if the lwr and upr values are correctly assigned
  expect_equal(result$current_lwr, data_current_diet$intake_lwr)
  expect_equal(result$current_upr, data_current_diet$intake_upr)
})