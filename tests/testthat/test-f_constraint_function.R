# test_that("f_make_constraint_function generates correct constraint outputs", {
#   
#   # Sample input data for constraints
#   constraint_values <- list(
#     energy = list(unit_contrib = c(1, 2), lwr = 100, upr = 200),
#     protein = list(unit_contrib = c(1, 2), lwr = 50, upr = 150),
#     carbs = list(unit_contrib = c(1, 1), lwr = 30, upr = 120),
#     fat = list(unit_contrib = c(1, 1), lwr = 20, upr = 80),
#     vitaminc = list(unit_contrib = c(1, 0.5), lwr = 10, upr = 30),
#     calcium = list(unit_contrib = c(1, 1), lwr = 10, upr = 50),
#     ghge = list(unit_contrib = c(1, 0.1), lwr = 5, upr = 15)
#   )
#   
#   # Sample tag_outcomes
#   tag_outcomes <- c('energy', 'protein', 'carbs', 'fat', 'vitaminc', 'calcium', 'ghge')
#   
#   # Create the constraint function
#   f_constr <- f_make_constraint_function(constraint_values, tag_outcomes)
#   
#   # Sample new diet input x (this could be the diet to evaluate against the constraints)
#   x_test <- c(150, 100, 60, 40, 20, 30, 100)
#   
#   # Calculate the expected result for energy_lwr, energy_upr, etc.
#   # Energy lwr = - (150*1 + 100*2) + 100
#   expected_energy_lwr <- - (150 * 1 + 100 * 2) + 100
#   expected_energy_upr <- (150 * 1 + 100 * 2) - 200
#   
#   # Repeat the above calculation for each of the constraints (protein, carbs, fat, etc.)
#   expected_protein_lwr <- - (100 * 1 + 100 * 2) + 50
#   expected_protein_upr <- (100 * 1 + 100 * 2) - 150
#   
#   expected_carbs_lwr <- - (60 * 1 + 40 * 1) + 30
#   expected_carbs_upr <- (60 * 1 + 40 * 1) - 120
#   
#   expected_fat_lwr <- - (40 * 1 + 20 * 1) + 20
#   expected_fat_upr <- (40 * 1 + 20 * 1) - 80
#   
#   expected_vitaminc_lwr <- - (20 * 1 + 30 * 0.5) + 10
#   expected_vitaminc_upr <- (20 * 1 + 30 * 0.5) - 30
#   
#   expected_calcium_lwr <- - (30 * 1 + 30 * 1) + 10
#   expected_calcium_upr <- (30 * 1 + 30 * 1) - 50
#   
#   expected_ghge_lwr <- - (100 * 1 + 100 * 0.1) + 5
#   expected_ghge_upr <- (100 * 1 + 100 * 0.1) - 15
#   
#   # Call the constraint function on the test diet `x_test`
#   actual_result <- f_constr(x_test)
#   
#   # Create the expected result vector manually for the specific constraints we want
#   expected_result <- c(
#     energy_lwr = expected_energy_lwr, 
#     energy_upr = expected_energy_upr,
#     protein_lwr = expected_protein_lwr, 
#     protein_upr = expected_protein_upr,
#     carbs_lwr = expected_carbs_lwr, 
#     carbs_upr = expected_carbs_upr,
#     fat_lwr = expected_fat_lwr, 
#     fat_upr = expected_fat_upr,
#     vitaminc_lwr = expected_vitaminc_lwr, 
#     vitaminc_upr = expected_vitaminc_upr,
#     calcium_lwr = expected_calcium_lwr, 
#     calcium_upr = expected_calcium_upr,
#     ghge_lwr = expected_ghge_lwr, 
#     ghge_upr = expected_ghge_upr
#   )
#   
#   # Test if the result matches the expected result
#   expect_equal(actual_result, expected_result)
#   
# })
