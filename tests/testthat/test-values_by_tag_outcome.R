# library(dplyr)
# 
# test_that("values_by_tag_outcome correctly associates unit contributions with constraints", {
#   # Sample input data
#   data_unit_contrib <- tibble::tibble(
#     food_name = c("Apple", "Banana", "Carrot"),
#     vitamin_c = c(0.5, 0.2, 0.8),
#     fiber = c(1.2, 1.0, 2.5)
#   )
#   
#   data_constr <- tibble::tibble(
#     tag_outcome = c("vitamin_c", "fiber"),
#     constr_lwr = c(0.8, 0.9),  # Example lower constraint coefficients
#     constr_upr = c(1.2, 1.1)   # Example upper constraint coefficients
#   )
#   
#   # Run function
#   result <- values_by_tag_outcome(data_unit_contrib, data_constr)
#   
#   # Check that the output list has the correct structure
#   expect_equal(length(result), 3)  # Should contain food_name, tag_outcome, and val
#   expect_equal(names(result), c("food_name", "tag_outcome", "val"))  # Correct names
#   
#   # Check food_name and tag_outcome are correct
#   expect_equal(result$food_name, c("Apple", "Banana", "Carrot"))  # Correct food names
#   expect_equal(result$tag_outcome, c("vitamin_c", "fiber"))  # Correct tag_outcomes
#   
#   # Check that 'val' is a list of lists with unit contributions and constraints
#   expect_equal(length(result$val), 2)  # Two tag_outcomes: vitamin_c and fiber
#   
#   # Check the structure for vitamin_c (same for fiber)
#   vitamin_c_vals <- result$val[["vitamin_c"]]
#   expect_equal(vitamin_c_vals$unit_contrib, c(0.5, 0.2, 0.8))  # Unit contributions for vitamin_c
#   expect_equal(vitamin_c_vals$lwr, 0.8)  # Lower constraint for vitamin_c
#   expect_equal(vitamin_c_vals$upr, 1.2)  # Upper constraint for vitamin_c
#   
#   # Check the structure for fiber
#   fiber_vals <- result$val[["fiber"]]
#   expect_equal(fiber_vals$unit_contrib, c(1.2, 1.0, 2.5))  # Unit contributions for fiber
#   expect_equal(fiber_vals$lwr, 0.9)  # Lower constraint for fiber
#   expect_equal(fiber_vals$upr, 1.1)  # Upper constraint for fiber
# })
