test_that("set_constr_coef creates a dataframe with correct values", {
  result <- set_constr_coef(tag_outcome = c("vitamin_c", "fiber"), 
                            coef_lwr = c(10, 5), 
                            coef_upr = c(20, 15))
  
  expect_equal(nrow(result), 2)  # Should return 2 rows
  expect_equal(colnames(result), c("tag_outcome", "coef_constrlwr", "coef_construpr"))  # Correct columns
  expect_equal(result$tag_outcome, c("vitamin_c", "fiber"))
  expect_equal(result$coef_constrlwr, c(10, 5))
  expect_equal(result$coef_construpr, c(20, 15))
})

test_that("set_constr_coef handles single input values", {
  result <- set_constr_coef(tag_outcome = "protein", coef_lwr = 8, coef_upr = 18)
  
  expect_equal(nrow(result), 1)  # Should return 1 row
  expect_equal(result$tag_outcome, "protein")
  expect_equal(result$coef_constrlwr, 8)
  expect_equal(result$coef_construpr, 18)
})

test_that("set_constr_coef returns an empty dataframe when empty vectors are provided", {
  result <- set_constr_coef(tag_outcome = character(0), coef_lwr = numeric(0), coef_upr = numeric(0))
  
  expect_equal(nrow(result), 0)  # Should return 0 rows
  expect_equal(colnames(result), c("tag_outcome", "coef_constrlwr", "coef_construpr"))  # Columns should exist
})

#test_that("set_constr_coef throws an error for mismatched vector lengths", {
#  expect_error(set_constr_coef(tag_outcome = c("vitamin_c", "fiber"), coef_lwr = c(10), coef_upr = c(20, 15)),
#               "arguments imply differing number of rows")  # Expect error due to mismatched lengths
#})