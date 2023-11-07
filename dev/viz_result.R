# development code for result visualization 

data_newdiet <- readRDS('./inst/rawdata/demo_12foods_res1.rda')
data_contrib <- readRDS('./inst/rawdata/demo_12foods_res2.rda')


data_newdiet
data_contrib


# need to compare 

library(ggplot2)
library(gt)
library(gtExtras)


# data manip (put to data-raw) ----

foodname_group

# attach to data_newdiet
# data_newdiet

newdiet <- dplyr::left_join(data_newdiet, 
                            foodname_group, 
                            by = 'food_name')

# new diet ----

# p1: only compare current and new
name_ordered <- foodname_group$food_name

pd <- select(newdiet, c(food_name, current, new, group_macro))
pd$food_name_ordered <- factor(pd$food_name, 
                             levels = name_ordered, 
                             labels = name_ordered)
pd
# summary(pd)
# pd <- tidyr::pivot_longer(data = newdiet, 
#                           cols = -c(food_name, group_macro), 
#                           names_to = 'metric')


p1 <- ggplot(data = pd, aes(x = food_name_ordered, y = new, 
                              fill = group_macro))
p1 <- p1 + geom_bar(position = 'dodge', stat = 'identity')
p1 <- p1 + coord_flip()
p1 <- p1 + geom_text(aes(label = round(new, 1)), 
                     hjust = -0.8, 
                     color = 'black', 
                     size = 4)
p1 <- p1 + ylim(0, max(pd$new)*1.1)
p1 <- p1 + geom_errorbar(aes(ymin = current, ymax = current+1), 
                         width = 1)
p1 <- p1 + theme_minimal()
p1 <- p1 + theme(panel.grid.major.y = element_blank())
p1 <- p1 + scale_fill_brewer(palette = 'Dark2')
p1 <- p1 + labs(title = 'New diet', 
                x = 'Food groups', 
                y = 'Intake (grams)')
p1 <- p1 + theme(axis.text = element_text(size = 12), 
                 axis.title = element_text(size = 12), 
                 axis.title.y = element_blank(),
                 plot.title = element_text(size = 20), 
                 legend.position = 'none')

p1




# p2 ----

name_ordered <- foodname_group$food_name

pd2 <- select(newdiet, c(food_name, group_macro, percent_change))
pd2$food_name_ordered <- factor(pd2$food_name, 
                               levels = name_ordered, 
                               labels = name_ordered)


p2 <- ggplot(data = pd2, aes(x = food_name_ordered, 
                             y = percent_change, 
                             fill = group_macro))

p2 <- p2 + geom_bar(position = 'dodge', stat = 'identity', 
                    alpha = 0.7)
p2 <- p2 + coord_flip()
p2 <- p2 + geom_text(aes(label = round(percent_change, 1)), 
                     y = max(pd2$percent_change)*1.3,
                     color = 'black', 
                     size = 4)
p2 <- p2 + ylim(min(pd2$percent_change) - abs(0.1*(pd2$percent_change)), 
                max(pd2$percent_change)*1.3)

p2 <- p2 + theme_minimal()
p2 <- p2 + theme(panel.grid.major.y = element_blank())
p2 <- p2 + scale_fill_brewer(palette = 'Dark2')
p2 <- p2 + labs(title = 'Change by percent', 
                x = 'Food groups', 
                y = 'Percent change ')
p2 <- p2 + theme(axis.text = element_text(size = 12), 
                 axis.title = element_text(size = 12), 
                 plot.title = element_text(size = 20), 
                 legend.position = 'none', 
                 axis.text.y = element_blank(), 
                 axis.title.y = element_blank())
p2

# library(patchwork)
p1+p2+plot_layout(nrow = 1)









# validation -----

data_contrib

# reorder
td <- data_contrib[, c("tag_outcome",
                       "tc_new_diet", 
                       "total_contrib_raw", 
                       "min", 
                       "max", 
                       "is_ok",
                       "dev_if_not_ok")]

# add a fake row for exceeding upper threshold
nr <- nrow(td)+1
td[nr, ] <- td[2, ]
td[nr, ]$tag_outcome <- 'fake_metric'
td[nr, ]$tc_new_diet <- 90.21
td[nr, ]$is_ok <- 'beyond_upper'
td[nr, ]$dev_if_not_ok <- 0.018


td <- data.table::setDT(td)
data.table::setnames(td, old = 'tc_new_diet', 'tc_new')
data.table::setnames(td, old = 'total_contrib_raw', 'tc_current')
td[, min := round(min, digits = 1)]
td[, max := round(max, digits = 1)]
td[, dev_if_not_ok := dev_if_not_ok*100]
td[, checker := is_ok]

td[checker == 'Yes', checker := '-']
td[checker == 'beyond_lower', checker := paste0(abs(dev_if_not_ok), '% below')]
td[checker == 'beyond_upper', checker := paste0(abs(dev_if_not_ok), '% above')]

# drop columns
td[, is_ok := NULL]
td[, dev_if_not_ok := NULL]

td

# demo ----
data_contrib

td <- data.frame(td)
demo <- T
result <- list(data_contrib = data_contrib, 
               gt_data = td, 
               demo = demo)
result
tab_contrib(result)

# make table

gtt <- gt(td)
gtt <- cols_merge(gtt, 
                  columns = c(min, max), 
                  pattern = "{1}&dash;{2}")

gtt <- tab_spanner(gtt, 
                   label = md("**Total contribution**"), 
                   columns = c(tc_new, tc_current))

gtt <- tab_spanner(gtt, 
                   label = md("**Constraint**"), 
                   columns = c(min, checker))

gtt <- cols_label(gtt, 
                  tag_outcome = 'Outcome', 
                  tc_new = 'New diet', 
                  tc_current = "Current diet", 
                  min = "Range", 
                  checker = 'Comments')
gtt



# put new tc first, and boldface
# original (min, max)
gtt <- cols_align(gtt, align = 'center', columns = checker)

gtt <- tab_style(gtt, 
                 locations = cells_body(columns = tc_new), 
                 style = cell_text(weight = 'bold'))

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





# _______ LATER ______ ----

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
#' @import gt
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










