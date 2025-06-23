# Sample dataset
data_perunit_contrib <- data.frame(
  food_name = c("Apple", "Banana", "Carrot", "Doughnut"),
  vitamin_c = c(5, 10, 7, 2),
  fiber = c(3, 2, 5, 1)
)

test_that("select_perunit selects correct columns", {
  result <- select_perunit(data_perunit_contrib, tag_food = c("Apple", "Carrot"), tag_outcome = "vitamin_c")
  
  expect_equal(nrow(result), 2)  # Should return 2 rows
  expect_equal(colnames(result), c("food_name", "vitamin_c"))  # Selected columns
})

test_that("select_perunit handles multiple outcomes", {
  result <- select_perunit(data_perunit_contrib, tag_food = c("Banana", "Doughnut"), tag_outcome = c("fiber", "vitamin_c"))
  
  expect_equal(nrow(result), 2)  # Should return 2 rows
  expect_equal(colnames(result), c("food_name", "fiber", "vitamin_c"))
})

test_that("select_perunit returns empty dataframe if food is not found", {
  result <- select_perunit(data_perunit_contrib, tag_food = c("Pizza"), tag_outcome = "fiber")
  
  expect_equal(nrow(result), 0)  # No matching food
})

test_that("select_perunit handles an empty tag_food vector correctly", {
  result <- select_perunit(data_perunit_contrib, tag_food = c(), tag_outcome = "fiber")
  
  expect_equal(nrow(result), 0)  # No matching food
})

