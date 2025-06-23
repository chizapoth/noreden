# library(dplyr)
# test_that("compute_std_unit_contrib correctly computes standardized unit contributions", {
#   # Sample input data
#   uc_raw <- data.frame(
#     food_name = c("Apple", "Banana", "Carrot"),
#     vitamin_c = c(0.5, 0.2, 0.8),
#     fiber = c(1.2, 1.0, 2.5)
#   )
#   
#   std_coef <- data.frame(
#     tag_outcome = c("vitamin_c", "fiber"),
#     std_coef = c(2, 0.5)  # Example standardization factors
#   )
#   
#   # Manually compute expected standardized contributions
#   expected_uc_std <- uc_raw %>%
#     select(-food_name) %>%
#     mutate(across(everything(), ~ .x * std_coef$std_coef))
#   
#   # Run function
#   result <- compute_std_unit_contrib(uc_raw, std_coef)
#   
#   # Check output structure
#   expect_equal(nrow(result$uc_std), 3)  # Should have 3 rows (foods)
#   expect_equal(colnames(result$uc_std), colnames(uc_raw))  # Same columns, including food_name
#   
#   # Check computed values
#   expect_equal(select(result$uc_std, -food_name), expected_uc_std)  # Standardized values should match
#   
#   # Check input data remains unchanged
#   expect_equal(result$uc_raw, uc_raw)
#   expect_equal(result$std_coef, std_coef)
# })