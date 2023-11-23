# step 3: optimization algorithm


#' Make objective function based on current diet
#' 
#' @description
#' Make objective function based on current diet, used as inputs for the optimization.
#'
#' @param diet0 A vector of diet intake
#' @param method Specify method to use to compute the objective function values. So far only 'ss' (sum of squares) is implemented.
#' @return A function to be used in the optimization
#' @export
#'
#' @examples
#' # a demo example with 3 foods intakes
#' funobj <- f_make_objective_function(diet0 = c(187, 200, 55))
#' # to evaluate a new diet
#' funobj(x = c(187, 200, 50))

f_make_objective_function <- function(diet0, method = 'ss'){
  
  if(method == 'ss'){
    # sum of square difference
    f_obj <- function(x){
      res <- sum((x - diet0)^2)
      return(res)
    }
    # if there are other methods, implement later  
    
  }
  return(f_obj) 
}





#' Make inequality constraint functions based on constraint values
#' 
#' @description
#' Make inequality constraint functions, used as inputs for the optimization.
#' So far only offers possibility for 7 tag_outcomes; but shall expand.
#' 
#' @param constraint_values A list of constraint values organized by tag_outcome. Output from values_by_tag_outcome()
#' @param tag_outcomes A vector of tag_outcome of interest
#' @return A function to be used in the optimization
#' @export

f_make_constraint_function <- function(constraint_values, tag_outcomes){
  
  # constraint_values <- constval$val
  # tag_outcomes <- c('energy', 'ghge')
  
  
  # this is the function we want to return
  f_constr <- function (x) {
    
    energy <- constraint_values$energy
    protein <- constraint_values$protein
    carbs <- constraint_values$carbs
    fat <- constraint_values$fat
    vitaminc <- constraint_values$vitaminc
    calcium <- constraint_values$calcium
    ghge <- constraint_values$ghge
    
    # a few computed constraints, where x is the new diet
    # it should be the complete set of constrants, 
    # as we select in the last step from 
    energy_output_lwr <- - sum(x * energy$unit_contrib) + energy$lwr
    energy_output_upr <- sum(x * energy$unit_contrib) - energy$upr
    
    protein_output_lwr <- - sum(x * protein$unit_contrib) + protein$lwr
    protein_output_upr <- sum(x * protein$unit_contrib) - protein$upr
    
    carbs_output_lwr <- - sum(x * carbs$unit_contrib) + carbs$lwr
    carbs_output_upr <- sum(x * carbs$unit_contrib) - carbs$upr
    
    fat_output_lwr <- - sum(x * fat$unit_contrib) + fat$lwr
    fat_output_upr <- sum(x * fat$unit_contrib) - fat$upr
    
    vitaminc_output_lwr <- - sum(x * vitaminc$unit_contrib) + vitaminc$lwr
    vitaminc_output_upr <- sum(x * vitaminc$unit_contrib) - vitaminc$upr
    
    calcium_output_lwr <- - sum(x * calcium$unit_contrib) + calcium$lwr
    calcium_output_upr <- sum(x * calcium$unit_contrib) - calcium$upr
    
    ghge_output_lwr <- - sum(x * ghge$unit_contrib) + ghge$lwr
    ghge_output_upr <- sum(x * ghge$unit_contrib) - ghge$upr
    
    
    
    # collect in a named vector
    constr_all <- c(
      energy_lwr = energy_output_lwr, 
      energy_upr = energy_output_upr, 
      
      protein_lwr = protein_output_lwr, 
      protein_upr = protein_output_upr, 
      
      carbs_lwr = carbs_output_lwr, 
      carbs_upr = carbs_output_upr, 
      
      fat_lwr = fat_output_lwr, 
      fat_upr = fat_output_upr, 
      
      vitaminc_lwr = vitaminc_output_lwr, 
      vitaminc_upr = vitaminc_output_upr, 
      
      calcium_lwr = calcium_output_lwr, 
      calcium_upr = calcium_output_upr, 
      
      ghge_lwr = ghge_output_lwr, 
      ghge_upr = ghge_output_upr
      
    )
    
    # key step:
    # select the ones that we want, for example, tag1
    # need to watch out for the names 
    tags_lwr <- paste0(tag_outcomes, '_lwr')
    tags_upr <- paste0(tag_outcomes, '_upr')
    
    constr <- constr_all[c(tags_lwr, tags_upr)]
    
    # res <- list(constr = constr, 
    #             tags_lwr = tags_lwr, 
    #             tags_upr = tags_upr)
    
    return (constr)
  }
  
  # possibly better to also return the input
  
  return(f_constr)
}



#' New diet discovery
#' 
#' @description
#' Find new diet combination that satisfies a set of nutrition and environmental impact constraints.
#' 
#' @param diet0 A vector of current diet intake
#' @param diet0_upr A vector of values that set the upper bound of diet search
#' @param diet0_lwr A vector of values that set the lower bound of diet search
#' @param tag_outcomes A vector of tag_outcome of interest
#' @param constraint_val A list of constraint values that match tag_outcomes
#' @param print_runtime Whether to print runtime. Default is True.
#' 
#' @return A list of results
#' @export

find_new_diet <- function(diet0, 
                          diet0_upr, 
                          diet0_lwr, 
                          tag_outcomes, 
                          constraint_val, 
                          print_runtime = T){
  
  
  # initial values and lower and upper bounds of n foods 
  # probably better to always start from the current diet
  # diet0 <- cd$intake_mean
  # diet0_lwr <- cd$intake_lwr
  # diet0_upr <- cd$intake_upr
  # constraint value list
  # constraint_val <- constval$val_std 
  # tag_outcomes <- c('energy', 'protein', 'ghge')
  
  # set objective
  f_obj <- f_make_objective_function(diet0 = diet0)
  
  # set inequ constraints
  f_ineq <- f_make_constraint_function(
    constraint_values = constraint_val, 
    tag_outcomes = tag_outcomes)
  
  # number of constraints
  # two times of the number of tags
  nc <- 2*length(tag_outcomes) 
  # other options
  opts <- list( "algorithm" = "NLOPT_GN_ISRES",
                "xtol_rel"= 1.0e-15,
                "maxeval"= 160000,
                # 2*n tags
                "tol_constraints_ineq" = rep( 1.0e-10, nc))
  
  # set timer
  start_time <- Sys.time()
  # run the algorithm
  run_optim <- nloptr::nloptr(
    x0          = diet0,        # initial value for x
    eval_f      = f_obj,        # objective function
    lb          = diet0_lwr,        # lower bound for x
    ub          = diet0_upr,        # upper bound for x
    eval_g_ineq = f_ineq,       # inequality constraint
    opts        = opts          # options
  )
  
  end_time <- Sys.time()
  runtime <- end_time - start_time
  
  # return results
  if(print_runtime == T){
    res <- list(run_optim = run_optim, 
                runtime = runtime)
  }else{
    res <- list(run_optim = run_optim)
  }
  
  return(res)
}



