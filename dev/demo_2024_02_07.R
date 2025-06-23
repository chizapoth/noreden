# script for demo: group meeting 2024.02.07

library(noreden)

# retrieve all food (28 types)
all_diet

all_diet$food_name


all_food_names <- c('Bread', 
                    'Other grains', 
                    'Cakes', 
                    'Potatoes', 
                    'Vegetables', 
                    'Legumes', 
                    'Fruit, berries', 
                    'Juice', 
                    'Nuts', 
                    'Vegetarian products', 
                    'Red meat', 
                    'White meat', 
                    'Fish', 
                    'Eggs', 
                    'Cream, cream desserts', 
                    'Milk, yoghurt',
                    'Cheese', 
                    'Butter, margarine, oil',
                    'Sugar, sweets', 
                    'Coffee, tea', 
                    'Soda, saft', 
                    'Water', 
                    'Alcoholic beverages',
                    'Non-dairy milk',
                    'Snacks',
                    'Sauces',
                    'Spices',
                    'Other')

# contrib per unit 
contrib_per_unit


# set your parameters ----
# tag_food_12 <- c('Bread', 
#                  'Vegetables', 
#                  'Red meat', 
#                  'Milk, yoghurt', 
#                  'Fish', 
#                  'Cheese', 
#                  'Eggs', 
#                  'Fruit, berries', 
#                  'Potatoes', 
#                  'Other grains', 
#                  'Butter, margarine, oil', 
#                  'Sugar, sweets'
#                  )

tag_outcome_5 <- c('energy', 'protein', 'carbs', 'fat', 'ghge')


FD1 <- c('Bread', 
                    'Other grains', 
                    #'Cakes', 
                    'Potatoes', 
                    'Vegetables', 
                    'Legumes', 
                    'Fruit, berries', 
                    'Juice', 
                    'Nuts', 
                    #'Vegetarian products', 
                    #'Red meat', 
                    'White meat', 
                    'Fish', 
                    'Eggs', 
                    'Cream, cream desserts', 
                    #'Milk, yoghurt',
                    #'Cheese', 
                    'Butter, margarine, oil',
                    'Sugar, sweets', 
                    'Coffee, tea', 
                    #'Soda, saft', 
                    'Water')
                    #'Alcoholic beverages',
                    #'Non-dairy milk',
                    #'Snacks',
                    #'Sauces',
                   # 'Spices',
                    #'Other'
                    #'


FD2 <- c('Bread', 
                    #'Other grains', 
                    'Cakes', 
                    'Potatoes', 
                    'Vegetables', 
                    #'Legumes', 
                    'Fruit, berries', 
                    #'Juice', 
                    #'Nuts', 
                    #'Vegetarian products', 
                    #'Red meat', 
                    'White meat', 
                    'Fish', 
                    'Eggs', 
                    'Cream, cream desserts', 
                    'Milk, yoghurt',
                    'Cheese', 
                    'Butter, margarine, oil',
                    'Sugar, sweets', 
                    'Coffee, tea', 
                    #'Soda, saft', 
                    'Water', 
                    #'Alcoholic beverages',
                    #'Non-dairy milk',
                    #'Snacks',
                    #'Sauces',
                    'Spices')#,
                    #'Other')

# run these two lines
TAG_FOOD <- FD1
#TAG_FOOD <- FD2
#TAG_FOOD <- FD3
# TAG_FOOD <- FD4


TAG_OUTCOME <- tag_outcome_5
COEF_REDUCE <- 0.8


# select food and cpu ----
# contrib_per_unit

diet_selected <- select_diet(
  data_diet = all_diet,
  tag_food = TAG_FOOD)

# diet_selected

# computes contrib per unit
cpu_selected <- select_perunit(
  data_perunit_contrib = contrib_per_unit, 
  tag_food = TAG_FOOD, 
  tag_outcome = TAG_OUTCOME)
# cpu_selected

# foods <- c('Bread', 'Vegetables', 'Red meat')
# tag_outcomes <- c('energy', 'protein', 'ghge')
# select_perunit(data_perunit_contrib = contrib_per_unit, tag_food = foods, tag_outcome = tag_outcomes)

# total contrib ----# 

tc <- compute_total_contrib(
  data_diet = diet_selected, 
  data_perunit_contrib = cpu_selected)
# tc

# set constraints ----# 

constr_coef_df <- set_constr_coef(
  tag_outcome = TAG_OUTCOME, 
  coef_lwr = rep(0.9, length(TAG_OUTCOME)), 
  coef_upr = rep(1.0, length(TAG_OUTCOME)))
# constr_coef_df

# reduce ghge to 0.9
constr_coef_df_red <- reduce_constr(
  data_constr_coef = constr_coef_df, 
  tag_outcome_reduce = 'ghge', 
  coef_reduce = COEF_REDUCE)
# constr_coef_df_red


constr_val_reduce <- compute_constr(
  data_total_contrib = tc$total_contrib, 
  data_constr_coef = constr_coef_df_red)
constr_val_reduce


# constraints std -----
stdcoef <- compute_stdcoef(data_perunit_contrib = cpu_selected)
# stdcoef


cpu_selected_std_res <- compute_std_unit_contrib(
  uc_raw = cpu_selected,
  std_coef = stdcoef$std_coef)

cpu_selected_std <- cpu_selected_std_res$uc_std

# total contrib (std)
tc_std <- compute_total_contrib(
  data_diet = diet_selected, 
  data_perunit_contrib = cpu_selected_std)
# tc_std

# alternatively, directly use tc multiply by coef
# tc$total_contrib$total_contrib * coefs$std_coef
# ok 

# constraints based on std total contrib
cd_constr_std <- compute_constr(
  data_total_contrib = tc_std$total_contrib, 
  data_constr_coef = constr_coef_df)


constr_val_reduce_std <- compute_constr(
  data_total_contrib = tc_std$total_contrib, 
  data_constr_coef = constr_coef_df_red)

constr_val_reduce_std





# find new diet ----

# split constraint values 
constval <- values_by_tag_outcome(
  data_unit_contrib = cpu_selected_std, 
  data_constr = constr_val_reduce_std)


# constval$food_name
# constval$tag_outcome
# constval$val
# constval$val$energy



# f_obj <- f_make_objective_function(diet0 = x0)
# f_ineq <- f_make_constraint_function(
#   constraint_values = constval$val, 
#   tag_outcomes = tags)

# fo(x = (x0-2))



# run ----#

res <- find_new_diet(diet0 = diet_selected$intake_mean, 
                     diet0_upr = diet_selected$intake_upr, 
                     diet0_lwr = diet_selected$intake_lwr, 
                     tag_outcomes = TAG_OUTCOME, 
                     constraint_val = constval$val, 
                     print_runtime = T)

res



# present results ----

new_diet <- return_new_diet(
  result_obj = res$run_optim, 
  data_current_diet = diet_selected)

new_diet

# compute difference
new_old_compare <- compare_new_diet(data_new_diet = new_diet, 
                                    data_current_diet = diet_selected)

new_old_compare

# validate constraints
new_diet_validate <- validate_diet_contrib(data_new_diet = new_diet, 
                                           data_unit_contrib = cpu_selected,
                                           data_constr = constr_val_reduce)

new_diet_validate


# plot abs change and perc change -----
# fit in the exisitng pipeline 
d_compare_gram <- prep_diet_comparison_gram(
  new_old_compare)

plot_diet_comparison_gram(plot_obj = d_compare_gram,
                          title_text = 'New diet',
                          axis_x_text = 'Food groups',
                          axis_y_text = 'Intake (grams)')





# table  ----
d_tb_validate <- prep_validate_table(
  data_validate_diet = new_diet_validate)
table_validate(d_tb_validate)


# plot 2 AFTER FIXING ----
d_compare_percent <- prep_diet_comparison_percent(
  data_dietsummary = new_old_compare)

plot_diet_comparison_percent(plot_obj = d_compare_percent,
                             title_text = 'Percent change',
                             axis_x_text = 'Food groups',
                             axis_y_text = 'Percent')


# _______ ----
# debug ----
ddd
plot_diet_comparison_percent(plot_obj = ddd,
  title_text = 'Percent change',
axis_x_text = 'Food groups',
axis_y_text = 'Percent')

