# foodname_group ----
#' Macro food group (used for plotting purpose)
#'
#' This dataset contains the information for bigger food groups. We have 7 big groups, and 28 sub-groups.
#'
#' @format
#' \describe{
#' \item{group_macro}{7 macro food groups}
#' \item{food_name}{28 food subgroups}
#' }
"foodname_group"


#' Demo data: new diet comparison
#'
#' Demo data for visualization development purposes.
#'
#' @format
#' \describe{
#' \item{food_name}{Food name}
#' \item{current}{}
#' \item{new}{}
#' \item{absolute_change}{}
#' \item{percent_change}{}
#' \item{diet_bound_lwr}{}
#' \item{diet_bound_upr}{}
#' }
"data_newdiet"

#' Demo data: new diet contribution to nutrients and env impact
#'
#' Demo data for visualization development purposes.
#'
#' @format
#' \describe{
#' \item{tag_outcome}{Nutrient and env impact name}
#' \item{total_contrib_raw}{}
#' \item{min}{}
#' \item{max}{}
#' \item{tc_new_diet}{}
#' \item{is_ok}{}
#' \item{dev_if_not_ok}{}
#' }
"data_contrib"

