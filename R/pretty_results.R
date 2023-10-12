
# plot diet ----

#' Make diet comparison summary table (intake in grams)
#'
#' @param data_dietsummary a dataframe that contains x,y,z
#'
#' @return a list of items (pending description)
#' @export
#'
#' @examples
#' ddd <- prep_diet_comparison_gram(data_dietsummary = data_newdiet)
prep_diet_comparison_gram <- function(data_dietsummary){
  
  d <- data_dietsummary
  # add group_macro
  if(!"group_macro" %in% colnames(data_dietsummary)){
    d <- dplyr::left_join(data_dietsummary,
                          foodname_group,
                          by = 'food_name')
  }
  # select
  pd <- dplyr::select(d, c(food_name, current, new, group_macro))
  # order
  name_ordered <- foodname_group$food_name
  pd$food_name_ordered <- factor(pd$food_name, 
                                 levels = name_ordered, 
                                 labels = name_ordered)
  pd
  
  result <- list(data_dietsummary = data_dietsummary, 
                 plot_data = pd)
  
  # attach class attribute for s3
  result <- structure(result, 
                      class = 'diet_comparison_gram')
  return(result)
}





#' Plotting function for diet comparison summary
#'
#' @param plot_obj an object of class <diet_comparison_percent>
#' @param title_text Title for the plot
#' @param axis_x_text X axis title
#' @param axis_y_text Y axis title
#'
#' @return A plot 
#' @export
#'
#' @examples
#' library(ggplot2)
#' ddd <- prep_diet_comparison_gram(data_dietsummary = data_newdiet)
#' plot(plot_obj = ddd,
#'                           title_text = 'New diet',
#'                          axis_x_text = 'Food groups',
#'                           axis_y_text = 'Intake (grams)')
plot.diet_comparison_gram <- function(plot_obj, 
                                      title_text, 
                                      axis_x_text,
                                      axis_y_text){
  

  # need to match the column names
  pd <- plot_obj$plot_data
  p <- ggplot(data = pd, aes(x = food_name_ordered, 
                             y = new, 
                             fill = group_macro))
  p <- p + geom_bar(position = 'dodge', 
                    stat = 'identity')
  p <- p + coord_flip()
  
  # include numbers at the end of bar
  p <- p + geom_text(aes(label = round(new, 1)), 
                       hjust = -0.8, 
                       color = 'black', 
                       size = 4)
  p <- p + ylim(0, max(pd$new)*1.1)
  
  # include current diet (as vertical bars)
  p <- p + geom_errorbar(aes(ymin = current, 
                             ymax = current+1), 
                           width = 1)
  
  # themes
  p <- p + theme_minimal()
  p <- p + theme(panel.grid.major.y = element_blank())
  p <- p + scale_fill_brewer(palette = 'Dark2')
  p <- p + labs(title = title_text, 
                  x = axis_x_text, 
                  y = axis_y_text)
  p <- p + theme(axis.text = element_text(size = 12), 
                   axis.title = element_text(size = 12), 
                   axis.title.y = element_blank(),
                   plot.title = element_text(size = 20), 
                   legend.position = 'none')
  
  p
  return(p)
}



#' Make diet comparison summary table (percent change)
#'
#' @param data_dietsummary a dataframe that contains x,y,z
#'
#' @return a list of items (pending description)
#' @export
#'
#' @examples
#' ddd <- prep_diet_comparison_percent(data_dietsummary = data_newdiet)
prep_diet_comparison_percent <- function(data_dietsummary){
  
  d <- data_dietsummary
  # add group_macro
  if(!"group_macro" %in% colnames(data_dietsummary)){
    d <- dplyr::left_join(data_dietsummary,
                          foodname_group,
                          by = 'food_name')
  }
  # select
  pd <- dplyr::select(d, c(food_name, group_macro, percent_change))
  # order
  name_ordered <- foodname_group$food_name
  pd$food_name_ordered <- factor(pd$food_name, 
                                 levels = name_ordered, 
                                 labels = name_ordered)
  pd
  
  result <- list(data_dietsummary = data_dietsummary, 
                 plot_data = pd)
  
  # attach class attribute for s3
  result <- structure(result, 
                      class = 'diet_comparison_percent')
  return(result)
}



#' Plotting function for diet comparison summary
#'
#' @param plot_obj an object of class <diet_comparison_percent>
#' @param title_text Title for the plot
#' @param axis_x_text X axis title
#' @param axis_y_text Y axis title
#'
#' @return A plot 
#' @export
#'
#' @examples
#' ddd <- prep_diet_comparison_percent(data_dietsummary = data_newdiet)
#' plot(plot_obj = ddd,
#'                           title_text = 'Percent change',
#'                          axis_x_text = 'Food groups',
#'                           axis_y_text = 'Percent')
plot.diet_comparison_percent <- function(plot_obj, 
                                      title_text, 
                                      axis_x_text,
                                      axis_y_text){
  
  pd <- plot_obj$plot_data
  # need to match the column names
  
  p <- ggplot(data = pd, aes(x = food_name_ordered, 
                               y = percent_change, 
                               fill = group_macro))
  
  p <- p + geom_bar(position = 'dodge', 
                    stat = 'identity', 
                      alpha = 0.7)
  p <- p + coord_flip()
  # add percentage text to the right
  p <- p + geom_text(aes(label = round(percent_change, 1)), 
                       y = max(pd$percent_change)*1.3,
                       color = 'black', 
                       size = 4)
  
  # set limit 
  pc_max <- max(pd$percent_change)
  pc_min <- min(pd$percent_change)
  p <- p + ylim(pc_min - 0.1*(pc_max - pc_min), 
                pc_max + 0.1*(pc_max - pc_min))
  # theme
  p <- p + theme_minimal()
  p <- p + theme(panel.grid.major.y = element_blank())
  p <- p + scale_fill_brewer(palette = 'Dark2')
  p <- p + labs(title = title_text, 
                  x = axis_x_text, 
                  y = axis_y_text)
  p <- p + theme(axis.text = element_text(size = 12), 
                   axis.title = element_text(size = 12), 
                   plot.title = element_text(size = 20), 
                   legend.position = 'none', 
                   axis.text.y = element_blank(), 
                   axis.title.y = element_blank())
  p
  
}




# table contrib ----

#' Summary table preparation for fast checking constraint fulfilment
#'
#' @param data_contrib summary table from the results
#' @param demo T or F, is this for demonstration
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' dtt <- prep_contrib(data_contrib = data_contrib, demo = T)
prep_contrib <- function(data_contrib, demo = F){
  
  td <- data_contrib[, c("tag_outcome",
                         "tc_new_diet", 
                         "total_contrib_raw", 
                         "min", 
                         "max", 
                         "is_ok",
                         "dev_if_not_ok")]
  
  if(demo == T){
    # add a fake row for exceeding upper threshold
    nr <- nrow(td)+1
    td[nr, ] <- td[2, ]
    td[nr, ]$tag_outcome <- 'fake_metric'
    td[nr, ]$tc_new_diet <- 90.21
    td[nr, ]$is_ok <- 'beyond_upper'
    td[nr, ]$dev_if_not_ok <- 0.018
    
  }
  
  td <- data.table::setDT(td)
  data.table::setnames(td, old = 'tc_new_diet', 'tc_new')
  data.table::setnames(td, old = 'total_contrib_raw', 'tc_current')
  # td$min <- round(td$min, 1)
  # td$max <- round(td$max, 1)
  # td$dev_if_not_ok <- td$dev_if_not_ok*100
  # td$checker <- td$is_ok
  td[, min := round(min, digits = 1)]
  td[, max := round(max, digits = 1)]
  td[, dev_if_not_ok := dev_if_not_ok*100]
  td[, checker := is_ok]
  
  #td$checker[which(td$checker == 'Yes')] <- '-'
  #td$checker[which(td$checker == 'beyond_lower')] <- paste0(abs(), '% below')
  
  td[checker == 'Yes', checker := '-']
  td[checker == 'beyond_lower', checker := paste0(abs(dev_if_not_ok), '% below')]
  td[checker == 'beyond_upper', checker := paste0(abs(dev_if_not_ok), '% above')]
  
  # drop columns
  td[, is_ok := NULL]
  td[, dev_if_not_ok := NULL]
  
  td <- data.frame(td)
  result <- list(data_contrib = data_contrib, 
                 gt_data = td, 
                 demo = demo)
  # result <- structure(result, class = 'diet_comparison_percent')
  return(result)
}



#' Make `gt` table for the diet contribution summary
#'
#' @param tab_obj a table object from prep_contrib()
#'
#' @return a `gt` table with coloring
#' @export
#'
#' @examples
#' tab_contrib(tab_obj = dtt)
tab_contrib <- function(tab_obj){
  
  td <- tab_obj$gt_data
  # make gt table
  gtt <- gt(td)
  # merge min and max in one col
  gtt <- cols_merge(gtt, 
                    columns = c(min, max), 
                    pattern = "{1}&dash;{2}")
  
  # add header
  gtt <- tab_spanner(gtt, 
                     label = md("**Total contribution**"), 
                     columns = c(tc_new, tc_current))
  
  gtt <- tab_spanner(gtt, 
                     label = md("**Constraint**"), 
                     columns = c(min, checker))
  # change text
  gtt <- cols_label(gtt, 
                    tag_outcome = 'Outcome', 
                    tc_new = 'New diet', 
                    tc_current = "Current diet", 
                    min = "Range", 
                    checker = 'Comments')

  # center checker column
  gtt <- cols_align(gtt, align = 'center', columns = checker)
  
  # boldface tc new
  gtt <- tab_style(gtt, 
                   locations = cells_body(columns = tc_new), 
                   style = cell_text(weight = 'bold'))
  # add color: blue for below, red for above
  gtt <- tab_style(gtt, 
                   locations = cells_body(
                     columns = c(tc_new, checker), 
                     rows = (tc_new < tc_current)&checker != '-'), 
                   style = cell_fill(color = 'lightblue', alpha = 0.8))
  
  gtt <- tab_style(gtt, 
                   locations = cells_body(
                     columns = c(tc_new, checker), 
                     rows = (tc_new > tc_current)&checker != '-'), 
                   style = cell_fill(color = 'lightcoral', alpha = 0.8))
  gtt
  
  return(gtt)
}






