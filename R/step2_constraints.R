# step 2: constraint set up and computation

#' Set constraint coefficients
#' 
#' @description
#' The constraint coefficients are used to compute the inequality constraints based on the total contribution.
#' The coefficients come in pairs: for each tag_outcome, there are lower and upper bounds for the constraint values.
#'
#' @param tag_outcome A vector of tag_outcome.
#' @param coef_lwr A vector of values to set the lower bound, used to multiply by total contribution.
#' @param coef_upr A vector of values to set the upper bound, used to multiply by total contribution.
#' @return A dataframe with tag_outcome with the coefficients.
#' @export
#'
#' @examples
#' tag_outcomes <- c('energy', 'protein', 'ghge')
#' set_constr_coef(tag_outcome = tag_outcomes, 
#'   coef_lwr = rep(0.9, length(tag_outcomes)), 
#'   coef_upr = rep(1.0, length(tag_outcomes)))

set_constr_coef <- function(tag_outcome, 
                            coef_lwr, 
                            coef_upr){
  
  constr_coef <- data.frame(tag_outcome = tag_outcome, 
                            coef_constrlwr = coef_lwr, 
                            coef_construpr = coef_upr)
  
  return(constr_coef)
}


#' Compute reduced constraint coefficients
#' 
#' @description
#' Reduce constraint coefficient for one specific tag_outcome
#'
#' @param data_constr_coef A dataframe where constraint coefficients are stored
#' @param tag_outcome_reduce Name of the tag_outcome to reduce
#' @param coef_reduce Numeric value to multiply the constraint coefficients of the selected tag_outcome
#' @return A dataframe with tag_outcome with the updated coefficients.
#' @export
#'
#' @examples
#' tag_outcomes <- c('energy', 'protein', 'ghge')
#' constr_coef_df <- set_constr_coef(tag_outcome = tag_outcomes, 
#'   coef_lwr = rep(0.9, length(tag_outcomes)), 
#'   coef_upr = rep(1.0, length(tag_outcomes)))
#' # reduce the coefficients of ghge to 90% its original value
#' reduce_constr(data_constr_coef = constr_coef_df, 
#'   tag_outcome_reduce = 'ghge', 
#'   coef_reduce = 0.9)
#' 

reduce_constr <- function(data_constr_coef, 
                          tag_outcome_reduce, 
                          coef_reduce){
  
  # data_constr_coef <- constr_df
  # tag_outcome_reduce <- 'ghge'
  # coef_reduce <- 0.8  
  
  id <- which(data_constr_coef$tag_outcome == tag_outcome_reduce)
  
  # multiply by a factor
  data_constr_coef[id, ]$coef_constrlwr <- 
    data_constr_coef[id, ]$coef_constrlwr *coef_reduce
  
  data_constr_coef[id, ]$coef_construpr <- 
    data_constr_coef[id, ]$coef_construpr *coef_reduce
  
  return(data_constr_coef)
  
}


#' Compute total contribution for outcomes
#' 
#' @description
#' Compute total contribution of the current diet regarding each outcome of interest.
#'
#' @param data_diet A dataframe where diet intake information is stored
#' @param data_perunit_contrib A dataframe where contribution per unit for each tag_outcome is stored. This could also be the standardized contribution per unit.
#' @return A list of outputs:
#' * `total_contrib` stores the total contribution of the current diet
#' * `tag_food` stores the names of foods
#' * `tag_outcome` stores the names of outcomes
#' 
#' @export
#'
#' @examples
#' diet_selected <- select_diet(data_diet = all_diet,
#'                       tag_food = c('Bread', 'Vegetables', 'Red meat'))
#' 
#' cpu_selected <- select_perunit(data_perunit_contrib = contrib_per_unit, 
#'                         tag_food = c('Bread', 'Vegetables', 'Red meat'), 
#'                         tag_outcome = c('energy', 'protein', 'ghge'))
#' 
#' compute_total_contrib(data_diet = diet_selected, 
#'                       data_perunit_contrib = cpu_selected)

compute_total_contrib <- function(data_diet, 
                                  data_perunit_contrib){
  
  # compute total contrib X foods (contrib_pu * intake)
  # data_diet <- diet_s
  # data_perunit_contrib <- puc_s
  
  # check if names match 
  if(!all.equal(data_diet$food_name, data_perunit_contrib$food_name)){
    stop('Food names do not match, check input data')
  }
  # at least have mean intake in the diet data
  if(!'intake_mean' %in% colnames(data_diet)){
    stop('Need to supply intake_mean for diet computation')
  }
  
  # remove food_nname for computation
  uc_tb <- dplyr::select(data_perunit_contrib, -c('food_name'))
  
  # total contrib
  # 1:9 times 9:9
  tc <- t(as.matrix(data_diet$intake_mean)) %*% as.matrix(uc_tb)
  
  tag_outcome <- colnames(uc_tb)
  food_name <- data_diet$food_name
  # make it in df 
  tc_table <- data.frame(tag_outcome = tag_outcome, 
                         total_contrib = as.numeric(tc))
  
  # print(tc_table)
  res <- list(total_contrib = tc_table, 
              tag_food = food_name, 
              tag_outcome = tag_outcome)
  return(res)
  
}


#' Compute standardise coefficients for contribution per unit
#' 
#' @description
#' Compute standardise coefficients for contribution per unit for each `tag_outcome`.
#'
#' @param data_perunit_contrib A dataframe where contribution per unit for each tag_outcome is stored. 
#' @param method Method to compute the coefficient. So far only `method = 'sd'` is implemented.
#' @return A list of outputs:
#' * `std_coef` stores the standardization coefficients to scale the contribution per unit for each tag_outcome
#' * `method` returns the method used for the computation
#' 
#' @export
#'
#' @examples
#' cpu_selected <- select_perunit(data_perunit_contrib = contrib_per_unit, 
#'                         tag_food = c('Bread', 'Vegetables', 'Red meat'), 
#'                         tag_outcome = c('energy', 'protein', 'ghge'))
#' compute_stdcoef(data_perunit_contrib = cpu_selected)

compute_stdcoef <- function(data_perunit_contrib, 
                            method = 'sd'){
  
  # data_perunit_contrib <- puc_s
  
  if('food_name' %in% colnames(data_perunit_contrib)){
    uc_tb <- dplyr::select(data_perunit_contrib, -c('food_name'))
  }else{
    uc_tb <- data_perunit_contrib
  }
  
  # take the smaller subset
  
  if(method == 'sd'){
    # print('Method: divide by standard deviation of current diet wrt tag_outcome')
    stopifnot('Must have more than 1 food to compute sd' = 
                nrow(uc_tb)>1)
    
    sd_vec <- apply(uc_tb, MARGIN = 2, sd)
    std_coef <- 1/sd_vec
  }
  
  tag_outcome <- colnames(uc_tb)
  
  std_df <- data.frame(tag_outcome = tag_outcome, 
                       std_coef = as.numeric(std_coef))
  
  res <- list(std_coef = std_df, 
              method = method)
  
  return(res)
}


#' Compute standardised contribution per unit based on coefficients
#' 
#' @description
#' Compute standardised contribution per unit for each `tag_outcome`, based on standardise coefficients.
#'
#' @param uc_raw A dataframe where contribution per unit for each `tag_outcome` is stored. 
#' @param std_coef Standardise coefficients for each `tag_outcome`, stored as a dataframe.
#' @return A list of outputs:
#' 
#' * `uc_raw` stores the original values of contribution per unit for each food and tag_outcome
#' * `std_coef` standardise coefficients used to compute the standardized contribution per unit
#' * `uc_std` stores the standardised values of contribution per unit
#' 
#' @export
#'
#' @examples
#' cpu_selected <- select_perunit(data_perunit_contrib = contrib_per_unit, 
#'                         tag_food = c('Bread', 'Vegetables', 'Red meat'), 
#'                         tag_outcome = c('energy', 'protein', 'ghge'))
#' coefs <- compute_stdcoef(data_perunit_contrib = cpu_selected)
#' compute_std_unit_contrib(uc_raw = cpu_selected, std_coef = coefs$std_coef)

compute_std_unit_contrib <- function(uc_raw, std_coef){
  
  # check tag_outcome name consistency
  if('food_name' %in% colnames(uc_raw)){
    uc_tb <- dplyr::select(uc_raw, -c('food_name'))
  }else{
    uc_tb <- uc_raw
  }
  if(!all.equal(colnames(uc_tb), std_coef$tag_outcome)){
    stop('tag_outcome names do not match')
  }
  
  # col1 * coef1, col2 * coef2, ...
  # t(t(matrix(c(1,1,1,2,2,2), nrow = 3)) * c(3,4))
  
  uc_tb_std <- data.frame(t(t(uc_tb) * std_coef$std_coef))
  
  if('food_name' %in% colnames(uc_raw)){
    # attach food name if it's in the original data
    uc_tb_std <- cbind(food_name = uc_raw$food_name, 
                       uc_tb_std)
  }
  
  
  res <- list(uc_raw = uc_raw, 
              std_coef = std_coef,
              uc_std = uc_tb_std)
  
  
  return(res)
}



#' Compute constraint values for outcomes
#' 
#' @description
#' Compute constraint values for each tag_outcomes, used for the optimisation algorithm. 
#' The computation is based on constraint coefficients, where the basis to multiply the coefficients are total contribution (either raw or standardized).
#' 
#' @param data_total_contrib A dataframe where total contribution (raw or standardised) for each `tag_outcome` is stored 
#' @param data_constr_coef A dataframe of lower and upper constraint coefficients for each `tag_outcome`
#' @return A dataframe of calculated constraint values
#' 
#' @export
#'
#' @examples
#' diet_selected <- select_diet(data_diet = all_diet,
#'                       tag_food = c('Bread', 'Vegetables', 'Red meat'))
#' 
#' cpu_selected <- select_perunit(data_perunit_contrib = contrib_per_unit, 
#'                         tag_food = c('Bread', 'Vegetables', 'Red meat'), 
#'                         tag_outcome = c('energy', 'protein', 'ghge'))
#' 
#' tc <- compute_total_contrib(data_diet = diet_selected, 
#'                       data_perunit_contrib = cpu_selected)
#' constr_coef_df <- set_constr_coef(tag_outcome = c('energy', 'protein', 'ghge'), 
#'                                   coef_lwr = rep(0.9, 3), 
#'                                   coef_upr = rep(1.0, 3))
#' 
#' # reduce ghge to 0.9
#' constr_coef_df_red <- reduce_constr(data_constr_coef = constr_coef_df, 
#'                                     tag_outcome_reduce = 'ghge', 
#'                                     coef_reduce = 0.9)
#' compute_constr(data_total_contrib = tc$total_contrib, 
#'                data_constr_coef = constr_coef_df_red)

compute_constr <- function(data_total_contrib, 
                           data_constr_coef){
  
  # data_total_contrib <- tc_std$total_contrib
  # data_constr_coef <- constr_df
  
  # check if names are consistent
  if(sum(data_total_contrib$tag_outcome %in% data_constr_coef$tag_outcome) !=
     nrow(data_constr_coef)){
    stop('tag_outcome do not match, check')
  }
  
  d <- dplyr::left_join(data_total_contrib, data_constr_coef, by = 'tag_outcome')
  # multiply
  d$constr_lwr <- d$total_contrib * d$coef_constrlwr
  d$constr_upr <- d$total_contrib * d$coef_construpr
  
  
  return(d)
}




#' Organize input values for outcomes 
#' 
#' @description
#' The optimization algorithm requires three inputs per tag_outcome, such as 'energy': 
#' * contribution per unit (either standardized or raw) for each food
#' * upper bound for total contribution
#' * lower bound for total contribution
#' 
#' @param data_unit_contrib A dataframe where total contribution (raw or standardised) for each `tag_outcome` is stored 
#' @param data_constr A dataframe of lower and upper constraint coefficients for each `tag_outcome`
#' @return A list of outputs, organized by tag_outcome
#' 
#' @export

values_by_tag_outcome <- function(data_unit_contrib, data_constr){
  # this should be independent of whether std or not
  
  # data_unit_contrib <- cd_unit_contrib_std
  # data_constr <- cd_constr_std
  
  food_name <- data_unit_contrib$food_name
  tag_outcome_uc <- colnames(data_unit_contrib)[colnames(data_unit_contrib)!='food_name']
  tag_outcome_constr <- data_constr$tag_outcome
  
  if(sum(tag_outcome_uc %in% tag_outcome_constr) != length(tag_outcome_uc)){
    stop('tag_outcome do not match. check')
  }
  
  to <- tag_outcome_uc
  
  val <- list()
  for (i in 1:length(to)){
    
    # i <- 1
    # per unit contrib (n foods)
    # select the column with matching tag_outcome
    # d_unit_contrib[, 2]
    id <- which(colnames(data_unit_contrib) == to[i])
    unit_contrib <- data_unit_contrib[, id]
    
    # lwr, upr constraint (n tag)
    id2 <- which(data_constr$tag_outcome == to[i])
    lwr <- data_constr[id2, ]$constr_lwr
    upr <- data_constr[id2, ]$constr_upr
    
    val[[i]] <- list(unit_contrib = unit_contrib, 
                     lwr = lwr, 
                     upr = upr)
  }
  
  names(val) <- to
  
  return(list(food_name = food_name, 
              tag_outcome = to, 
              val = val))
  
}




