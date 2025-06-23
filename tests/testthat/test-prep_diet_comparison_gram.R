test_that("prep_diet_comparison_gram correctly processes data for diet comparison", {
  
  # Sample input data for data_dietsummary (replace with actual columns)
  data_dietsummary <- tibble::tibble(
    food_name = c("Apple", "Banana", "Carrot"),
    current = c(100, 200, 150),
    new = c(110, 180, 160)
  )
  
  # Sample reference food group (mock for testing, you can replace with actual data)
  dref_foodgroup <- tibble::tibble(
    food_name = c("Apple", "Banana", "Carrot"),
    group_macro = c("Fruit", "Fruit", "Vegetable")
  )
  
  # Run the function
  result <- prep_diet_comparison_gram(data_dietsummary, dref_foodgroup)
  
  # Check that the result has the correct structure
  expect_equal(length(result), 2)  # Should contain data_dietsummary and plot_data
  expect_equal(names(result), c("data_dietsummary", "plot_data"))  # Correct names
  
  # Check if the plot_data contains the correct columns
  expect_equal(colnames(result$plot_data), c("food_name", "current", "new", "group_macro", "food_name_ordered"))
  
  # Check if the food_name_ordered factor has the correct levels and labels
  expect_equal(levels(result$plot_data$food_name_ordered), c("Apple", "Banana", "Carrot"))
  
  # Check if the data_dietsummary remains unchanged
  expect_equal(result$data_dietsummary, data_dietsummary)
  
  # Check if the class attribute for the result is correct
  expect_equal(class(result), c("diet_comparison_gram"))
})