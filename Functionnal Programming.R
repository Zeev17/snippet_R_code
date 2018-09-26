setwd("C:/Users/Install/Desktop/writing function")
##################################################################################################
#Functionnal Programming
##################################################################################################
##################################################################################################
#Using a for loop to remove duplication
##################################################################################################
#Imagine we have a data frame called df
df <- data.frame(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
# Initialize output vector
output <- vector("double", ncol(df))  

# Fill in the body of the for loop
for (i in seq_along(df)) {            
  output[[i]] <- median(df[[i]])
}

# View the result
output
##################################################################################################
#Turning the for loop into a function
##################################################################################################
# imagine you need to do this to another data frame df2
#and edit every reference to df to be df2 instead.

#Turn the for loop snippet into a function called col_median()
col_median <- function(df) {
  output <- vector("double", ncol(df))
  for (i in seq_along(df)) {            
  output[[i]] <- median(df[[i]])
  }
  output
}
##################################################################################################
#What about column means?
##################################################################################################
#What if instead of medians of every column you actually want means?
#Let's write a col_mean()
# Create col_mean() function to find column means
col_mean <- function(df) {
  output <- numeric(length(df))
  for (i in seq_along(df)) {
    output[[i]] <- mean(df[[i]])
  }
  output
}
##################################################################################################
#What about column standard deviations?
##################################################################################################
#what about one for standard deviations?
# Define col_sd() function
col_sd <- function(df) {
  output <- numeric(length(df))
  for (i in seq_along(df)) {
    output[[i]] <- sd(df[[i]])
  }
  output
}
##################################################################################################
#Uh oh...time to write a function again
##################################################################################################
#How can we write a function that will take column summaries for any summary function we provide?
x<- sample(1:100, 25, replace = TRUE)
f1 <- function(x) abs(x - mean(x)) ^ 1
f2 <- function(x) abs(x - mean(x)) ^ 2
f3 <- function(x) abs(x - mean(x)) ^ 3
f1(x)
f2(x)
#Edit the function f() to take a second argument power
#Edit the body of f() so that the absolute deviations raised to power are returned.
# Add a second argument called power
f <- function(x, power) {
  # Edit the body to return absolute deviations raised to power
  abs(x - mean(x))^power
}
##################################################################################################
#Using a function as an argument
##################################################################################################
#we can remove the duplication in our set of summary functions 
#by requiring the function doing the summary as an input. 
col_summary <- function(df, fun) {
  output <- vector("numeric", length(df))
  for (i in seq_along(df)) {
    output[[i]] <- fun(df[[i]])
  }
  output
}
# Find the column medians using col_median() and col_summary()
col_median(df) 
col_summary(df, median)

# Find the column means using col_mean() and col_summary()
col_mean(df)
col_summary(df, mean)

# Find the column IQRs using col_summary()
col_summary(df, IQR)

#If you've used the lapply() or sapply() functions before
#, then you've passed functions as arguments.
lapply(df, mean)

sapply(df, mean)
sapply(df, col_summary, fun = median)
sapply(df, col_summary, fun = mean)
lapply(df, col_summary, fun = sd)

##################################################################################################
#The map functions
##################################################################################################
#All the map functions in purrr take a vector x?
#as the first argument, then return .f applied to each element of .x
#The type of object that is returned is determined by function suffix 

#map() returns a list or data frame
#map_lgl() returns a logical vector
#map_int() returns a integer vector
#map_dbl() returns a double vector
#map_chr() returns a character vector

#Let's repeat our column summaries using a map function instead of our col_summary()
# Load the purrr package
library(purrr)
# Use map_dbl() to find column means
map_dbl(df, mean)
# Use map_dbl() to column medians
map_dbl(df, median)
# Use map_dbl() to find column standard deviations
map_dbl(df, sd)
##################################################################################################
#The ... argument to the map functions
##################################################################################################
#map_dbl(df, mean, trim = 0.5)
#Multiple arguments can be passed along using commas to separate them
#map_dbl(df, mean, trim = 0.5, na.rm = TRUE)
#You don't have to specify the arguments by name, but it is good practice!

#We'll apply our new knowledge to a subset of the planes data frame available in the nycflights13
library(nycflights13)
planes_full <- nycflights13::planes
library(dplyr)
index <- sample(1:nrow(planes_full), 6 , replace = TRUE)
planes <- planes_full[index,] %>% select(c("year","engines","seats","speed"))
str(planes)
# Find the mean of each column
map_dbl(planes, mean)
# Find the mean of each column, excluding missing values
map_dbl(planes, mean, na.rm = TRUE)
# Find the 5th percentile of each column, excluding missing values
map_dbl(planes, quantile, probs = c(0.05), na.rm = TRUE)
#Other nice features of the map functions are that they're implemented in C,
##################################################################################################
#Picking the right map function
##################################################################################################
#Choosing the right map function is important
#However, if you know what type of output you expect, you are better to use the corresponding function.
#We've created a new data frame, df3, in your workspace
#Use a map function to find which columns are numeric, the type of each column
df3 <- data.frame(
  A = rnorm(1:10),
  B = sample(c("A","B"), 10, replace = TRUE),
  C = 1:10,
  D = rnorm(1:10)
)
df3
# Find the columns that are numeric
map_lgl(df3, is.numeric)
# Find the type of each column
map_chr(df3, typeof)

# Find a summary of each column
map(df3, summary)

##################################################################################################
#Solve a simple problem first
##################################################################################################
#Our goal is to fit a separate linear regression of miles per gallon (mpg) against weight (wt) 
#for each group of cars in our list of data frames, where each data frame in our list represents a different group

#create a df "cycl" makes up with a df by cyl
cyl <- split(mtcars, mtcars$cyl)
str(cyl)
cyl[[1]]
# Examine the structure of cyl
str(cyl)

# Extract the first element into four_cyls
four_cyls <- cyl[[1]]

# Fit a linear regression of mpg on wt using four_cyls
lm(mpg ~ wt, data = four_cyls)

##################################################################################################
#Using an anonymous function
##################################################################################################
#We now have a snippet of code that performs the operation we want on one data frame.
#One option would be to turn this into a function, for example:
fit_reg <- function(df) {
  lm(mpg ~ wt, data = df)
}
#Then pass this function into map()
map(cyl, fit_reg)
#But it seems a bit much to define a function for such a specific model 
#we will just use the function anonymously inside our call to map()

#Rewrite the map() call to use the anonymous function function(df) lm(mpg ~ wt, data = df)
# Rewrite to call an anonymous function
map(cyl, ~ lm(mpg ~ wt, data = .))
# Rewrite to use the formula shortcut instead
map(cyl, function(x) lm(mpg ~ wt, data = x))

##################################################################################################
#Using a string
##################################################################################################
#There are also some useful shortcuts that come in handy 
#when you want to subset each element of the .x argument
#if the .f argument to a map function is set equal to a string, let's say "name"
#then purrr extracts the "name" element from every element of .x.

#This is a really common situation you find yourself in when you work with nested lists.
#if we have a list of where every element contains an a and b element:
list_of_results <- list(
  list(a = 1, b = "A"), 
  list(a = 2, b = "C"), 
  list(a = 3, b = "D")
)
map(list_of_results, "a")

#Now take our list of regresssion models:
map(cyl, ~ lm(mpg ~ wt, data = .))
#It might be nice to extract the slope coefficient from each model
#You'll do this in a few steps:
#first fit the models,
#then get the coefficients from each model using the coef()
#then pull out the wt estimate using the string shortcut
model <- lm(mpg ~ wt, data = four_cyls)
str(model, give.attr = FALSE)
coef(model)[2]
# Save the result from the previous exercise to the variable models
models <- map(cyl, ~ lm(mpg ~ wt, data = .))
# Use map and coef to get the coefficients for each model: coefs
coefs <- map(models, coef)
# Use string shortcut to extract the wt coefficient 
map(coefs, "wt")
##################################################################################################
#Using a numeric vector
##################################################################################################
list_of_results <- list(
  list(a = 1, b = "A"), 
  list(a = 2, b = "C"), 
  list(a = 3, b = "D")
)
map(list_of_results, 1)
# use map_dbl with the numeric shortcut to pull out the second element
map_dbl(coefs, 2)
##################################################################################################
#Putting it together with pipes
##################################################################################################
#purrr also includes a pipe operator: %>%
#x %>% f(y) is another way of writing f(x, y)

#Take a look at our code to get our list of models:
cyl <- split(mtcars, mtcars$cyl) 
map(cyl, ~ lm(mpg ~ wt, data = .))
#We could rewrite this using the pipe operator as:
split(mtcars, mtcars$cyl) %>% 
  map(~ lm(mpg ~ wt, data = .))

#One of the powerful things about the pipe is we can chain together many operations.
#Here is our complete code, written with pipes, instead assigning each step to a variable
mtcars %>% 
  split(mtcars$cyl) %>%
  map(~ lm(mpg ~ wt, data = .)) %>%
  map(coef) %>% 
  map_dbl("wt")

#We've written some code in the editor to pull out the R2 from each model.
#Rewrite the last two lines to use a pipe instead.

# Define models (don't change)
models <- mtcars %>% 
  split(mtcars$cyl) %>%
  map(~ lm(mpg ~ wt, data = .))

#From this
summaries <- map(models, summary)
map_dbl(summaries, "r.squared")
#Rewrite to be a single command using pipes 
models %>% map(summary) %>% map_dbl("r.squared")


