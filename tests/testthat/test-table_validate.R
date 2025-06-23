# library(gt)
# library(dplyr)
# 
# test_that("table_validate creates a formatted gt table", {
#   
#   # Sample data for the function to process
#   data <- tibble::tibble(
#     tag_outcome = c("Outcome1", "Outcome2", "Outcome3"),
#     tc_new = c(120, 150, 130),
#     tc_current = c(100, 160, 120),
#     min = c(90, 140, 110),
#     max = c(130, 170, 140),
#     check = c("beyond_upper", "Ok", "beyond_lower")#,
#     #deviation = c(10, 0, 5)
#   )
#   
#   # Prepare the data to match the expected structure of the function
#   # result <- prep_validate_table(data)
#   
#   # Pass the processed data into the table_validate function
#   table_result <- table_validate(result)
#   
#   # Ensure that the result is a gt table
#   expect_s3_class(table_result, "gt_tbl")
#   
#   # Check that the correct number of columns is present
#   expect_true(all(c("Outcome", "New diet", "Current diet", "Range", "Comments") %in% colnames(table_result)))
#   
#   # Check that the 'min' and 'max' columns are merged correctly into a single 'Range' column
#   merged_range_column <- table_result %>%
#     gt::render_formats_table() %>%
#     stringr::str_detect("–")
#   expect_true(all(merged_range_column))
#   
#   # Check that the 'check' column has been aligned to the center
#   # Note: You can't directly test this in unit test, so we will focus on indirect effects
#   expect_equal(table_result$cols_align$check, 'center')
#   
#   # Check the 'New diet' column is bold
#   # This can be verified indirectly since it's difficult to test styles directly
#   expect_true(any(table_result$tab_style$locations == "cells_body(columns = tc_new)"))
#   
#   # Check that cells with new diet lower than current diet are filled with light blue
#   light_blue_cells <- table_result %>%
#     gt::render_formats_table() %>%
#     stringr::str_detect("lightblue")
#   expect_true(any(light_blue_cells))
#   
#   # Check that cells with new diet higher than current diet are filled with light coral
#   light_coral_cells <- table_result %>%
#     gt::render_formats_table() %>%
#     stringr::str_detect("lightcoral")
#   expect_true(any(light_coral_cells))
#   
# })