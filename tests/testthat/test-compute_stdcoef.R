test_that("compute_stdcoef correctly calculates standardized coefficients using SD method", {
  # Sample input data
  data_perunit_contrib <- data.frame(
    food_name = c("Apple", "Banana", "Carrot"),
    vitamin_c = c(0.5, 0.2, 0.8),
    fiber = c(1.2, 1.0, 2.5)
  )
  
  # Manually compute expected standard deviation and standardized coefficient
  expected_sd <- apply(dplyr::select(data_perunit_contrib, -food_name), 2, sd)
  expected_std_coef <- 1 / expected_sd
  
  # Run function
  result <- compute_stdcoef(data_perunit_contrib, method = "sd")
  
  # Check output structure
  expect_equal(nrow(result$std_coef), 2)  # Should have 2 outcomes
  expect_equal(result$std_coef$tag_outcome, c("vitamin_c", "fiber"))  # Correct outcome names
  
  # Check computed values
  expect_equal(result$std_coef$std_coef, as.numeric(expected_std_coef))
  expect_equal(result$method, "sd")  # Method should be "sd"
})

