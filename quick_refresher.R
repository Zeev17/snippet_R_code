#setwd("C:/Users/Install/Desktop/writing function")
##################################################################################################
#Writing a fucntion
##################################################################################################
#write a function ratio() that takes arguments x and y and returns their ratio, x / y

# Define ratio() function
ratio <- function(x, y) {
  x / y
}

# Call ratio() with arguments 3 and 4
ratio(3, 4)
##################################################################################################
#Arguments
##################################################################################################
mean(c(1:9, NA), trim = 0.1, na.rm = TRUE)
##################################################################################################
#Function output
##################################################################################################
f <- function(x) {
  if (TRUE) {
    return(x + 1)
  }
  x
}
f(is.character(15))
f(!is.character(15))
#The body of the conditional is always evaluated and the function returns early without ever running x
##################################################################################################
#
##################################################################################################
j <- function(){
  if(!exists("a")) {
    a <- 1
  } else {
    a <- a + 1
  }
  print(a)
  }
j()
##################################################################################################
#Testing your understanding of scoping
##################################################################################################
y <- 10
f <- function(x) {
  x + y
}
#What will f(10) return? 
#20
##################################################################################################
y <- 10
f <- function(x) {
  y <- 5
  x + y
}
#What will f(10) return?
#15
##################################################################################################
f <- function(x) {
  y <- 5
  x + y
}
f(5)
#Now, what will typing y return?
#error because it is inside the function's body
##################################################################################################
a <- list(
  a = 1:3,
  b = "a string",
  c = pi,
  d = list(-1,-5)
)
str(a[4])
str(a[[4]])
str(a[[4]][1])
str(a[[4]][[1]])
##################################################################################################
#Subsetting lists
##################################################################################################
#create "tricky-list"
tricky_list <- list(
  nums = rnorm(1:10),
  y = c(FALSE,FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE),
  x = list("hello",
          "hi",
          "goodbye",
          "bye"),
  model = lm(formula = mpg ~ wt, data = mtcars)
  )
# 2nd element in tricky_list
typeof(tricky_list[[2]])

# Element called x in tricky_list
typeof(tricky_list[["x"]])
typeof(tricky_list$x)
# 2nd element inside the element called x in tricky_list
typeof(tricky_list[["x"]][[2]])
typeof(tricky_list[[3]][[2]])

##################################################################################################
#Exploring lists
##################################################################################################
# Guess where the regression model is stored
names(tricky_list)

# Use names() and str() on the model element
names(tricky_list[["model"]])
str(tricky_list[["model"]])

# Subset the coefficients element
tricky_list[["model"]][["coefficients"]]

# Subset the wt element
tricky_list[["model"]][["coefficients"]][["wt"]]

##################################################################################################
#for loops
##################################################################################################
primes_list <- list(2,3,5,7,11,13)
for (i in 1:length(primes_list)) {
  print(primes_list[[i]])
}

df <- data.frame(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
for (i in 1:col(df)) {
  print(median(df[[i]]))
}
##################################################################################################
#A safer way to create the sequence
##################################################################################################
#Let's take a look at 
#i in 1:ncol(df)
#What might surprise you is that this isn't the best way to generate such a sequence
# Replace the 1:ncol(df) sequence
for (i in seq_along(df)) {
  print(median(df[[i]]))
}
# Change the value of df
#df <- data.frame()

# Repeat for loop to verify there is no error
for (i in seq_along(df)) {
  print(median(df[[i]]))
  }
##################################################################################################
#Keeping output
##################################################################################################
#Before you start the loop, you must always allocate sufficient space for the output, 
#let's say an object called output
#A general way of creating an empty vector of given length is the vector()
#he type of the vector ("logical", "integer", "double", "character", etc.) and the length

#vector, i.e. assign the result to output[[i]]
#ask why we are using double brackets here when output is a vector
# It's primarily for generalizability: this subsetting will work whether output is a vector or a list.

# Create new double vector: output
output <- vector("double", ncol(df))

# Alter the loop
for (i in seq_along(df)) {
  # Change code to store result in output
  output[[i]] <- median(df[[i]])
}

# Print output
output