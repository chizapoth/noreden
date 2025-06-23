test_that("plot_diet_comparison_gram generates a valid ggplot object", {
  
  # Sample data to simulate the plot_obj$plot_data structure
  plot_obj <- list(
    plot_data = tibble::tibble(
      food_name_ordered = c("Apple", "Banana", "Carrot"),
      new = c(110, 180, 160),
      current = c(100, 200, 150),
      group_macro = c("Fruit", "Fruit", "Vegetable")
    )
  )
  
  # Title and axis labels for the plot
  title_text <- "Diet Comparison"
  axis_x_text <- "Food"
  axis_y_text <- "Nutrient Amount"
  
  # Run the function
  p <- plot_diet_comparison_gram(plot_obj, title_text, axis_x_text, axis_y_text)
  
  # Check that the result is a ggplot object
  expect_s3_class(p, "gg")  # Ensures the plot is of class 'gg'
  
  # Check that the plot contains the correct title
  expect_equal(p$labels$title, title_text)  # Title should match the provided text
  
  # Check that the plot contains the correct x-axis and y-axis labels
  expect_equal(p$labels$x, axis_x_text)  # x-axis label should match the provided text
  expect_equal(p$labels$y, axis_y_text)  # y-axis label should match the provided text
  
  # Check that the plot contains the correct fill color legend (group_macro)
  expect_true("group_macro" %in% names(p$plot_env$pd))  # Ensure that 'group_macro' is in the data
  
  # # Check that the bar plot contains the expected aesthetics (x, y, fill)
  # p_layers <- p$layers[[1]]$mapping
  # expect_equal(as.character(p_layers$x), "food_name_ordered")  # x aesthetic should be food_name_ordered
  # expect_equal(as.character(p_layers$y), "new")  # y aesthetic should be 'new'
  # expect_equal(as.character(p_layers$fill), "group_macro")  # fill aesthetic should be 'group_macro'
  # 
  # # Check that the plot includes the geom_text for numbers at the end of the bars
  # expect_true(any(sapply(p$layers, function(layer) inherits(layer$geom, "GeomTextRepel"))))  # Check if geom_text is used
  # 
  # # Check that the plot includes the geom_errorbar for the 'current' values
  # expect_true(any(sapply(p$layers, function(layer) inherits(layer$geom, "GeomErrorbar"))))  # Check if geom_errorbar is used
})
