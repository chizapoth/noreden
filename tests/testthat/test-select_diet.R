# Sample dataset
data_diet <- data.frame(
  food_name = c("Apple", "Banana", "Carrot", "Doughnut"),
  intake_mean = c(50, 100, 30, 200),
  intake_lwr = c(40, 90, 20, 180),
  intake_upr = c(60, 110, 40, 220)
)

test_that("select_diet selects correct columns with minmax = TRUE", {
  result <- select_diet(data_diet, tag_food = c("Apple", "Carrot"), minmax = TRUE)
  
  expect_equal(nrow(result), 2)  # Should return 2 rows
  expect_equal(colnames(result), c("food_name", "intake_mean", "intake_lwr", "intake_upr"))
})

test_that("select_diet selects correct columns with minmax = FALSE", {
  result <- select_diet(data_diet, tag_food = c("Banana", "Doughnut"), minmax = FALSE)
  
  expect_equal(nrow(result), 2)  # Should return 2 rows
  expect_equal(colnames(result), c("food_name", "intake_mean"))
})

test_that("select_diet returns empty dataframe if food is not found", {
  result <- select_diet(data_diet, tag_food = c("Pizza"), minmax = TRUE)
  
  expect_equal(nrow(result), 0)  # No matching food
})

test_that("select_diet handles an empty tag_food vector correctly", {
  result <- select_diet(data_diet, tag_food = c(), minmax = TRUE)
  
  expect_equal(nrow(result), 0)  # No matching food
})