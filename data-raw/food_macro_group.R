# big macro group -----
# first define big groups
grain <- c('Bread', 'Other grains', 'Cakes')
fruit_vege <- c('Potatoes', 'Vegetables', 'Legumes', 'Fruit, berries', 
                'Juice', 'Nuts', 'Vegetarian products')
meat <- c('Red meat', 'White meat')
fish_egg <- c('Fish', 'Eggs')
dairy <- c('Cream, cream desserts', 'Milk, yoghurt', 'Cheese')
fats <- c('Butter, margarine, oil')
beverages <- c('Coffee, tea', 'Soda, saft', 'Water', 
               'Alcoholic beverages', 'Non-dairy milk')
sugar_other <- c('Sugar, sweets', 'Snacks', 'Sauces', 'Spices', 'Other')

# reorder food names to make the plot easier to read
names_ordered <- c(grain, fruit_vege, meat, fish_egg, 
                   dairy, fats, beverages, sugar_other)

names_ordered

# put to a df 
foodname_group <- data.frame(
  group_macro = c(rep('grain', length(grain)), 
                  rep('fruit_vege', length(fruit_vege)), 
                  rep('meat', length(meat)), 
                  rep('fish_egg', length(fish_egg)), 
                  rep('dairy', length(dairy)), 
                  rep('fats', length(fats)), 
                  rep('beverages', length(beverages)), 
                  rep('sugar_other', length(sugar_other))), 
  food_name = names_ordered
)

# write data

save(foodname_group, file = "data/foodname_group.rda", compress = 'xz')

