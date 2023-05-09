library(readxl)
library(magrittr)
library(data.table)


# baseline
# demographics
# all foods 

baseline <- read_excel("data_example/Condensed dataset NOR-Eden optimization.xlsx", 
                        sheet = "Data N3 baseline diet")

demographics <- read_excel("data_example/Condensed dataset NOR-Eden optimization.xlsx", 
                       sheet = "Data N3 demographic variables")

nut <- read_excel("data_example/Condensed dataset NOR-Eden optimization.xlsx", 
                  sheet = "Database KBS all foods nutr-ICs")

# use data.table format
baseline <- data.table(baseline)
demographics <- data.table(demographics)
nut <- data.table(nut)



# baseline ----

baseline %>% colnames() %>% head # Nr, Totalg, 
# need to take the columns in 2: name, value

# separate numeric codes and nutrition groups
num_codes <- colnames(baseline)[3:(length(colnames(baseline)) - 63)]
num_codes <- as.numeric(codes) # 1507 foods

range(num_codes)

nut_codes <- colnames(baseline)[(length(colnames(baseline)) - 62):length(colnames(baseline))]
nut_codes  






# nutrition info ----
# background info for foods
colnames(nut)

nut$Code %>% hist

# do they match baseline colnames?
nut$Code
range(nut$Code)
# num_codes %in% nut$Code
# should be fine






# demographics ----

head(demographics, 2)
colnames(demographics) 
# age, gender, bmi
demographics$Age %>% hist
table(demographics$Gender) # more or less balanced 
demographics$BMI %>% hist # this is interesting

# other como
demographics$Diabetes %>% table



# __________ ----
# split baseline into 2 parts ----

# keep personal id in all 
# 1. totalg, nutrition codes (water, energy, ...)
# 2. individual codes
num_codes # foods
food_codes <- as.character(num_codes)
nut_codes # nutri

# nutrition columns: add ID and total
base_nutri <- baseline[, ..nut_codes] # this is how to select multiple columns! 
base_nutri <- cbind(baseline[, 1:2], base_nutri)

# foods: add ID 
base_foods <- baseline[, ..food_codes]
base_foods <- cbind(baseline[, 1], base_foods)


# demographics: merge age, gender, bmi onto 

demo <- demographics[, .(Nr, Age, Gender, BMI)]
demo

# merge nutri, foods
dt_nutri <- merge(demo, base_nutri, by = 'Nr')
dt_foods <- merge(demo, base_foods, by = 'Nr')




# __________ -----
# eda ----
library(ggplot2)
library(RColorBrewer)

dt_nutri$Totalg %>% hist # total calories

plot(dt_nutri$Age, dt_nutri$Totalg) # no obvious pattern

boxplot(Totalg ~ Gender, data = dt_nutri) # maybe men slightly more than women

plot(dt_nutri$BMI, dt_nutri$Totalg)

# plot 1: totalg, bmi, gender, age ----

plt <- ggplot(data = dt_nutri, mapping = aes(x = BMI, y = Totalg, color = Age))
plt <- plt + geom_point(alpha = 0.8, size = 2)
plt <- plt + facet_wrap( ~ Gender)
plt <- plt + scale_colour_continuous(type = 'viridis')
plt <- plt + theme_bw()
plt <- plt + theme(
  axis.text = element_text(size = 12),
  axis.title = element_text(size = 12), 
  plot.title = element_text(size = 15)
)
plt <- plt + labs(
  x = 'BMI', 
  y = 'Totalg', 
  title = 'Totalg vs BMI by gender'
)
plt


dt_nutri %>% head(2)


# explore: common foods ----

# understand which foods is more interesting

dt_foods_foodsonly <- dt_foods[, 5:ncol(dt_foods)]

# summary(dt_foods_foodsonly$`131`)
# which(dt_foods_foodsonly$`131` > 0) %>% length # 83


food_freq <- apply(dt_foods_foodsonly, 2, above_zero)

summary(food_freq)

# top 30?
which.max(food_freq) # 8001
nut[Code == '8001']

# quantile(food_freq, 0.99)
food_freq_common <- which(food_freq>300)
nut[Code %in% names(food_freq_common), 1:5]



above_zero <- function(x){
  length(which(x>0))
}

# above_zero(dt_foods_foodsonly$`131`)

# plot 2: certain foods ----
dt_foods$Gender <- as.character(dt_foods$Gender)
# banana (2012)
# tomato (1122)
# mais (1361)
# dt_foods$`2012`

plt <- ggplot(data = dt_foods, mapping = aes(x = BMI, y = `1361`, color = Age))
plt <- plt + geom_point(alpha = 0.8, size = 2)
plt
plt <- plt + facet_wrap( ~ Gender)
plt <- plt + scale_colour_continuous(type = 'viridis')
plt <- plt + theme_bw()
plt
# plt <- plt + theme(
#   axis.text = element_text(size = 12),
#   axis.title = element_text(size = 12), 
#   plot.title = element_text(size = 15)
# )
# plt <- plt + labs(
#   x = 'BMI', 
#   y = 'Totalg', 
#   title = 'Totalg vs BMI by gender'
# )
plt


# other types 
# salami (3277)
# majones (6451)

plt <- ggplot(data = dt_foods, mapping = aes(x = BMI, y = `6451`, color = Age))
plt <- plt + geom_point(alpha = 0.8, size = 2)
plt
plt <- plt + facet_wrap( ~ Gender)
plt <- plt + scale_colour_continuous(type = 'viridis')
plt <- plt + theme_bw()
plt
# plt <- plt + theme(
#   axis.text = element_text(size = 12),
#   axis.title = element_text(size = 12), 
#   plot.title = element_text(size = 15)
# )
# plt <- plt + labs(
#   x = 'BMI', 
#   y = 'Totalg', 
#   title = 'Totalg vs BMI by gender'
# )
plt









