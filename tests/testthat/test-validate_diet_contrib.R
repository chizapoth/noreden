# library(dplyr)
# 
# test_that("validate_diet_contrib produces correct output", {
#   
#   # Sample new diet data (food_name and new intake values)
#   data_new_diet <- tibble::tibble(
#     food_name = c("Food1", "Food2", "Food3"),
#     new = c(120, 190, 160)
#   )
#   
#   # Sample unit contribution data (food_name and contributions)
#   data_unit_contrib <- tibble::tibble(
#     food_name = c("Food1", "Food2", "Food3"),
#     tag_outcome = c("Outcome1", "Outcome2", "Outcome3"),
#     contribution = c(1.1, 2.2, 1.5)
#   )
#   
#   # Sample constraints data (food_name, constr_lwr, constr_upr, and tag_outcome)
#   data_constr <- tibble::tibble(
#     tag_outcome = c("Outcome1", "Outcome2", "Outcome3"),
#     constr_lwr = c(100, 150, 130),
#     constr_upr = c(140, 200, 170)
#   )
#   
#   # Call the validate_diet_contrib function
#   result <- validate_diet_contrib(data_new_diet, 
#                                   data_unit_contrib, 
#                                   data_constr)
#   
#   # Check if the result is a data frame
#   expect_s3_class(result, "data.frame")
#   
#   # Check if the data frame has the expected columns
#   expect_true(all(c("tag_outcome", "total_contrib_new", "constr_lwr", 
#                     "constr_upr", "check", "deviation") %in% colnames(result)))
#   
#   # Check if the "check" column is correctly populated based on constraints
#   expect_equal(result$check[1], "beyond_upper")  # Food1 exceeds the upper limit (120*1.1 > 140)
#   expect_equal(result$check[2], "ok")           # Food2 is within the range
#   expect_equal(result$check[3], "beyond_upper")  # Food3 exceeds the upper limit (160*1.5 > 170)
#   
#   # Check if the "deviation" column is calculated correctly for values beyond upper and lower limits
#   expect_equal(result$deviation[1], round((120 - 140) / 140, 3)) # Deviation for Food1
#   expect_equal(result$deviation[2], 0)  # Food2 is within the limit
#   expect_equal(result$deviation[3], round((160 - 170) / 170, 3)) # Deviation for Food3
#   
# })
