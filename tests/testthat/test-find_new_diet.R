# test_that("find_new_diet works correctly", {
#   
#   # Sample input data
#   diet0 <- c(100, 50, 200)  # Example initial diet values
#   diet0_upr <- c(120, 70, 220)  # Upper bounds for the diet
#   diet0_lwr <- c(80, 30, 180)  # Lower bounds for the diet
#   
#   # Example constraint values (values would be specific to your problem)
#   constraint_val <- list(
#     energy = list(unit_contrib = c(1, 2), lwr = 100, upr = 200),
#     protein = list(unit_contrib = c(1, 2), lwr = 50, upr = 150),
#     ghge = list(unit_contrib = c(1, 0.5), lwr = 10, upr = 40)
#   )
#   
#   # Example tag_outcomes
#   tag_outcomes <- c('energy', 'protein', 'ghge')
#   
#   # Run the function
#   result <- find_new_diet(diet0 = diet0, 
#                           diet0_upr = diet0_upr, 
#                           diet0_lwr = diet0_lwr, 
#                           tag_outcomes = tag_outcomes, 
#                           constraint_val = constraint_val, 
#                           print_runtime = FALSE)
#   
#   # Test that the function returns a list
#   expect_type(result, "list")
#   
#   # Test that the result contains 'run_optim' (the optimization result)
#   expect_true("run_optim" %in% names(result))
#   
#   # Check that the result contains 'runtime' if print_runtime is TRUE
#   # (If print_runtime is FALSE, the 'runtime' element should not exist)
#   if (result$runtime) {
#     expect_true("runtime" %in% names(result))
#   }
#   
#   # Test that the 'run_optim' has necessary components (e.g., solution, status, etc.)
#   expect_true("solution" %in% names(result$run_optim))
#   expect_true("status" %in% names(result$run_optim))
#   
#   # Check if the solution vector returned by the optimizer is of the correct length
#   expect_equal(length(result$run_optim$solution), length(diet0))
#   
#   # Test if the function respects the bounds (check if solution is within bounds)
#   solution <- result$run_optim$solution
#   expect_true(all(solution >= diet0_lwr))
#   expect_true(all(solution <= diet0_upr))
#   
# })