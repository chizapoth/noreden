# step 1: input processing


#' Select a set of foods
#'
#' @description
#' Food selector based on a bigger table, used to provide current diet information.
#'
#' @param data_diet Data that contain food (group) names and current diet information.
#' @param tag_food A vector of food names to choose from
#' @param minmax Whether to return min and max limit of feasible set for new diet
#' @return A dataframe with food names and diet intake for selected foods
#' @export
#'
#' @examples
#' foods <- c('Bread', 'Vegetables', 'Red meat')
#' select_diet(data_diet = all_diet, tag_food = foods)

select_diet <- function(data_diet, tag_food, minmax = T){
  
  diet_selected <- dplyr::filter(data_diet, food_name %in% tag_food) |> 
    dplyr::select(dplyr::all_of(c('food_name','intake_mean')))
  
  if(minmax == T){
    
    diet_selected <- dplyr::filter(data_diet, food_name %in% tag_food) |> 
      dplyr::select(dplyr::all_of(c('food_name',
                                    'intake_mean', 
                                    'intake_lwr', 
                                    'intake_upr')))
    
  }
  
  return(diet_selected)
}


#' Select contribution per unit for nutrition and environmental impact outcomes
#'
#' @description
#' Select contribution per unit information based on a bigger table.
#' 
#' @param data_perunit_contrib Data that contain food (group) names and current diet information.
#' @param tag_food A vector of food names to choose from
#' @param tag_outcome Whether to return min and max limit of feasible set for new diet
#' @return A dataframe with food names and diet intake for selected foods
#' @export
#'
#' @examples
#' foods <- c('Bread', 'Vegetables', 'Red meat')
#' tag_outcomes <- c('energy', 'protein', 'ghge')
#' select_perunit(data_perunit_contrib = contrib_per_unit, tag_food = foods, tag_outcome = tag_outcomes)

select_perunit <- function(data_perunit_contrib, 
                           tag_food, 
                           tag_outcome){
  
  perunit_selected <- dplyr::filter(data_perunit_contrib, 
                                    food_name %in% tag_food) |> 
    dplyr::select(dplyr::all_of(c('food_name', tag_outcome)))
  
  return(perunit_selected)
}



