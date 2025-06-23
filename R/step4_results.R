# step 4: gather results

#' Collect results from the new diet discovery
#' 
#' @description
#' Collect results from the new diet discovery, along with the current diet.
#' 
#' @param result_obj Result object from the new diet discovry
#' @param data_current_diet Current diet that has food name and average intake
#' 
#' @return A dataframe with food names, current diet and new diet
#' @export

return_new_diet <- function(result_obj, data_current_diet){
  
  new_diet <- result_obj$solution
  
  res <- data.frame(food_name = data_current_diet$food_name, 
                    new = new_diet, 
                    current = data_current_diet$intake_mean)
  return(res)
}






#' Compare new diet with current diet
#' 
#' @description
#' Compare new and current diet in terms of absolute and relative (percent) change
#' 
#' @param data_new_diet New diet intake
#' @param data_current_diet Current diet that has food name and average intake
#' 
#' @return A dataframe with food names, current and new diet intake and comparsion metrics
#' @export

compare_new_diet <- function(data_new_diet, 
                             data_current_diet){
  
  # data_current_diet <- diet_s
  # data_new_diet <- new_diet
  # check current diet names, should have mean and lwr upr
  coln_input <- c('food_name', 'intake_mean', 'intake_lwr', 'intake_upr')
  if(sum(coln_input %in% colnames(data_current_diet)) != length(coln_input)){
    stop('Must supply food_name, mean and lwr upr in the current diet data')
  }
  # remove current, since we add it back from the other table
  data_new_diet <- dplyr::select(data_new_diet, -current)
  
  d <- dplyr::left_join(data_new_diet, data_current_diet, by = 'food_name')
  
  d <- dplyr::rename(d, 
                     current = intake_mean, 
                     current_lwr = intake_lwr, 
                     current_upr = intake_upr)
  d <- dplyr::mutate(d, 
                     abs_change = (new - current), 
                     percent_change = (new - current)/current)|> 
    dplyr::mutate_if(is.numeric, round, 2)
  
  return(d)
}




#' Compare new diet with current diet
#' 
#' @description
#' Compare new and current diet in terms of absolute and relative (percent) change
#' 
#' @param data_new_diet New diet intake
#' @param data_current_diet Current diet that has food name and average intake
#' @param data_constr Constraint values to compare the new total contribution
#' 
#' @return A dataframe with tag_outcome, total contribution from the diet and whether it is within constraints
#' @export

validate_diet_contrib <- function(data_new_diet, 
                                  data_unit_contrib, 
                                  data_constr){
  
  # on original scale is fine
  # data_new_diet <- new_diet
  # data_unit_contrib <- puc_s
  
  # data_constr <- cd_constr_raw_red # original
  
  # compute new total contri 
  # consistent name
  d <- dplyr::select(data_new_diet, 
                     food_name,
                     intake_mean = new)
  tc_new <- compute_total_contrib(data_diet = d, 
                                  data_perunit_contrib = data_unit_contrib)
  
  d_tc_new <- tc_new$total_contrib
  d_tc_new <- dplyr::rename(d_tc_new, total_contrib_new = total_contrib)
  
  # put together, modify name
  tc_both <- dplyr::left_join(d_tc_new, data_constr, by = 'tag_outcome')
  
  tc_both <- dplyr::mutate(tc_both, check = dplyr::case_when(
    total_contrib_new > constr_upr ~ 'beyond_upper', 
    total_contrib_new < constr_lwr ~ 'beyond_lwr',
    .default = 'ok'
  ))
  
  tc_both <- dplyr::mutate(tc_both, deviation = dplyr::case_when(
    check == 'beyond_upper' ~ round((total_contrib_new - constr_upr)/constr_upr,3), 
    check == 'beyond_lower' ~ round((total_contrib_new - constr_lwr)/constr_lwr,3), 
    .default = 0
  ))|> 
    dplyr::mutate_if(is.numeric, round, 2)
  
  return(tc_both)
  
}
