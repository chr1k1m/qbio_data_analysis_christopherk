name = "christopher"
age = 19
birthday = "10/03/2002"

list_of_numbers = c(1, 3, 4, 8, 1, 44, -3, 29, -12)
x = min(list_of_numbers)
y = max(list_of_numbers)

quad_form = function(a, b, c){
  x = {-b+sqrt(b^2-4*a*c)}/2*a
  return = x
}

if (1 > 2){
  print("The if statement is true.")
} else if(2 > 1){
  print("The else if statement is true.")
} else{
  print("Neither the if NOR the else if is true.")
}
print("Green eggs and ham.")

if(1 > 2 | "red" == "blue"){
  print("I do not like green eggs and ham.")
}

if("red" == "green" | 2 > 1){
  print("I do not like them, Sam-I-Am.")
}
#"I do not like them, Sam-I-Am."

if(!(3 != 3 | "pass" == "fail")){
  print("Would you like them here or there?")
}
#"Would you like them here or there?"

x = -1
y = 3

if(x < y & x > y){
  print("I would not like them here or there.")
} else if (x < y){
  print("I would not like them anywhere.")
} else if(x > y){
  print("Would you like them in a house?")
} else{
  print("Would you like them with a mouse?")
}
#"I would not like them anywhere."

x = 0
while(x < 5){
  x = x + runif(1)
}
x

for (i in 1:5){
  print("USC")
}

i = 11
while(i > 1){
  i = i - 1
  print(i)
}

for(i in 1:10){
  print(10 - i + 1)
}

for(i in 1:6){
  for(j in 1:6){
    print(i+j)
  }
}

for(i in 1:6){
  print(i + 1:6)
}

mean_mat = matrix(1:100, nrow = 10, ncol = 10)
row_means = rowMeans(mean_mat)
col_means = colMeans(mean_mat)

?ifelse

if(!require(tidyverse)){
  install.packages("tidyverse")
}

library(tidyverse)

print("(1)")
str(mtcars)

print("(2)")
print(head(mtcars))

print("(3)")
library(tidyverse)
glimpse(mtcars)

mtcars$cyl

mtcars[3, ]

mtcars[, 1]

mtcars_4 = mtcars$hp
mtcars_4 = mtcars[, 4]

mtcars[3, 4]
mtcars$hp[3]

head(mtcars)

mtcars$wt[5]

mtcars[, 1:3]

dim(mtcars)
mtcars[1:2, 8:11]

n = 1:16 * 2
mtcars[n, ]

mtcars_copy = mtcars
mtcars_copy$mpg = mtcars_copy$mpg * 2
mtcars_copy$super_mpg = mtcars_copy$mpg * 100
glimpse(mtcars_copy)
mtcars$mpg = mtcars_copy$super_mpg
glimpse(mtcars)

getwd()

cereal = read.csv("cereal.csv")
str(cereal)

setwd("./Downloads/")

cereal = read.csv("cereal.csv")
str(cereal)
cereal_names = cereal$name
str(cereal_names)
favorite_cereals = c("Cheerios", "Honey Nut Cheerios", "Special K")

color_vector = c("cyan", "yellow", "magenta", "key")
color_vector[2]
color_vector[3]
color_vector[2:3]

numeric_values = c(1, 2, NA, 3, NA, NA, NA, 4)
mask = is.na(numeric_values)
mask
mask = !is.na(numeric_values)
mask
numeric_values = numeric_values[mask]
print(numeric_values)

favorite_cereals %in% cereal_names
cereal_names %in% favorite_cereals
# smaller one comes first

favorite_cereal_data = cereal[favorite_cereals, ]
dim(favorite_cereal_data)

numeric_values = c(1, 2, NA, 3, NA, NA, NA, 4)
convert_to = ifelse(is.na(numeric_values), -1, numeric_values)
print(convert_to)

numeric_values = c(1, 2, NA, 3, NA, NA, NA, 4)
numeric_values[is.na(numeric_values)] = -1
numeric_values

numeric_values = c(1, 2, NA, 3, NA, NA, NA, 4)
numeric_values = numeric_values[!is.na(numeric_values)]
numeric_values
sort = ifelse(numeric_values < 2.5, "small", "large")
print(sort)

cereal = read.csv("cereal.csv")
cereal_copy = cereal
str(cereal_copy)
cereal_copy$type_full = ifelse(cereal_copy$type == "C", "Cold", "Hot")
str(cereal_copy)
cereal$type = cereal_copy$type_full
str(cereal)