test_that("f_make_objective_function returns the correct objective function", {
  
  # Create a sample diet0 vector (true values)
  diet0 <- c(100, 200, 150, 180)
  
  # Generate an objective function using the default 'ss' method
  objective_function <- f_make_objective_function(diet0, method = 'ss')
  
  # Test that the objective function is a function
  expect_type(objective_function, "closure")
  
  # Create a test vector x (predicted values)
  x_test <- c(110, 190, 160, 175)
  
  # Calculate the expected result manually (sum of squared differences)
  expected_result <- sum((x_test - diet0)^2)
  
  # Call the objective function and check if it matches the expected result
  actual_result <- objective_function(x_test)
  
  # Test if the function returns the correct value
  expect_equal(actual_result, expected_result)
  
  # Check if the method is 'ss' (sum of squares) and nothing else is implemented
  # No additional methods should exist in the current function definition
  expect_equal(formals(objective_function)$method, NULL)
})
