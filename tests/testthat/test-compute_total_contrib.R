# Sample data
data_diet <- data.frame(
  food_name = c("Apple", "Banana", "Carrot"),
  intake_mean = c(100, 200, 150)
)

data_perunit_contrib <- data.frame(
  food_name = c("Apple", "Banana", "Carrot"),
  vitamin_c = c(0.5, 0.2, 0.8),
  fiber = c(1.2, 1.0, 2.5)
)

test_that("compute_total_contrib correctly calculates total contributions", {
  result <- compute_total_contrib(data_diet, data_perunit_contrib)

  expected_total_vitamin_c <- sum(data_diet$intake_mean * data_perunit_contrib$vitamin_c)
  expected_total_fiber <- sum(data_diet$intake_mean * data_perunit_contrib$fiber)

  expect_equal(nrow(result$total_contrib), 2)  # Should have two outcomes
  expect_equal(result$total_contrib$total_contrib[result$total_contrib$tag_outcome == "vitamin_c"], expected_total_vitamin_c)
  expect_equal(result$total_contrib$total_contrib[result$total_contrib$tag_outcome == "fiber"], expected_total_fiber)

  expect_equal(result$tag_food, data_diet$food_name)  # Should return correct food names
  expect_equal(result$tag_outcome, colnames(data_perunit_contrib)[-1])  # Outcomes should match column names
})