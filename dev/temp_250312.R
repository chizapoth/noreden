#' Make diet comparison summary table (percent change)
#'
#' @param data_dietsummary a dataframe that contains x,y,z
#' @param dref_foodgroup a reference dataframe to provide food group information
#' @return a list of items (pending description)
#' @export
#'
#' @examples
#' ddd <- prep_diet_comparison_percent(data_dietsummary = data_newdiet)
prep_diet_comparison_percent <- function(data_dietsummary, 
                                         dref_foodgroup = noreden::foodname_group){
  
  
  food_name <- NULL
  group_macro <- NULL
  perc_change <- NULL
  
  
  d <- data_dietsummary
  # add group_macro
  if(!"group_macro" %in% colnames(data_dietsummary)){
    d <- dplyr::left_join(data_dietsummary,
                          dref_foodgroup,
                          by = 'food_name')
  }
  # select
  pd <- dplyr::select(d, c(food_name, group_macro, perc_change))
  # order
  name_ordered <- dref_foodgroup$food_name
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
#' @import ggplot2
#' @export
#'
#' @examples
#' ddd <- prep_diet_comparison_percent(data_dietsummary = data_newdiet)
#' plot_diet_comparison_percent(plot_obj = ddd,
#'                           title_text = 'Percent change',
#'                          axis_x_text = 'Food groups',
#'                           axis_y_text = 'Percent')
plot_diet_comparison_percent <- function(plot_obj, 
                                         title_text, 
                                         axis_x_text,
                                         axis_y_text){
  
  # later make it plot.diet_comparison_percent (S3)
  
  food_name_ordered <- NULL
  percent_change <- NULL
  group_macro <- NULL
  
  pd <- plot_obj$plot_data
  # need to match the column names
  
  p <- ggplot(data = pd, aes(x = food_name_ordered, 
                             y = perc_change, 
                             fill = group_macro))
  
  p <- p + geom_bar(position = 'dodge', 
                    stat = 'identity', 
                    alpha = 0.7)
  p <- p + coord_flip()
  # add percentage text to the right
  p <- p + geom_text(aes(label = round(perc_change, 1)), 
                     y = max(pd$perc_change)*1.3,
                     color = 'black', 
                     size = 4)
  
  # set limit 
  pc_max <- max(pd$perc_change)
  pc_min <- min(pd$perc_change)
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


