setwd("C:/Users/Install/Desktop/writing function")
##################################################################################################
#Method Function
##################################################################################################
df <- data.frame(
  a = rnorm(1:10),
  b = rnorm(1:10),
  c = rnorm(1:10),
  d = rnorm(1:10)
)

#We have a snippet of code that successfully rescales a column to be between 0 and 1:
(df$a - min(df$a, na.rm = TRUE)) /  
  (max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE))

##################################################################################################
#Start with a snippet of code
##################################################################################################

#Our goal over the next few exercises is to turn this snippet
#, written to work on the a column in the data frame df
#into a general purpose rescale01() function that we can apply to any vector.

#The first step of turning a snippet into a function is
#to examine the snippet and decide how many inputs there are
#then rewrite the snippet to refer to these inputs using temporary names
#These inputs will become the arguments to our function, so choosing good names for them is important.

#In this snippet, there is one input: the numeric vector to be rescaled 
#(currently df$a)
# It's quite common in R to refer to a vector of data simply as x

# Define example vector x
x <- 1:10

# Rewrite this snippet to refer to x
(x - min(x, na.rm = TRUE)) /
  (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

##################################################################################################
#Rewrite for clarity
##################################################################################################
#Our next step is to examine our snippet and see if we can write it more clearly.
#(x - min(x, na.rm = TRUE)) /
  #(max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
#One obviously duplicated statement is min(x, na.rm = TRUE)
#since we also need the maximum value of x, it would be even better to calculate the range once
#I suggest we call it rng (for "range").
# Define example vector x
x <- 1:10

# Define rng
rng <- range(x, na.rm= TRUE)
       
# Rewrite this snippet to refer to the elements of rng
(x - min(x, na.rm = TRUE)) /
  (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

##################################################################################################
#Finally turn it into a function!
##################################################################################################
#What do you need to write a function?
#A name
#know the arguments to the function
#the form of body function
# Define example vector x
x <- 1:10 

# Use the function template to create the rescale01 function
rescale01 <- function(x) {
  # body
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

# Test your function, call rescale01 using the vector x as the argument
rescale01(x)

##################################################################################################
#How should you write a function ?
##################################################################################################
#FIRST begin with a concrete problem
#with data and you should know the answer
#SECOND get a working snippet of code
#Inside the snippet choose which code should be the input
#Rewrite the nippet with temporary variables
#After a cleary snippet THEN THIRD you can run it into a function
#This way our clear snippet is just the body of the function
#FINALLY test your fucntion.

##################################################################################################
#Start with a simple problem
##################################################################################################
#Let's tackle a new problem
#We want to write a function, both_na()
#that counts at how many positions two vectors, x and y, both have a missing value.

#We should start by solving a simple example problem first.
#Let's define an x and y where we know what the answer both_na(x, y)
x <- c( 1, 2, NA, 3, NA)
y <- c(NA, 3, NA, 3,  4)
#Then both_na(x, y) should return 1, 
#since there is only one element that is missing in both x and y, the third element.

#You may find the is.na() and sum() functions useful, as well as the & operator.
# Define example vectors x and y
x <- c( 1, 2, NA, 3, NA)
y <- c(NA, 3, NA, 3,  4)

# Count how many elements are missing in both x and y
sum(is.na(x) & is.na(y))

##################################################################################################
#Rewrite snippet as function
##################################################################################################
# Define example vectors x and y
x <- c( 1, 2, NA, 3, NA)
y <- c(NA, 3, NA, 3,  4)

# Turn this snippet into a function: both_na()
both_na <- function(x,y) {
  sum(is.na(x) & is.na(y))
  
}
#Try out our function
both_na(x,y)

##################################################################################################
#Put our function to use
##################################################################################################
#Consider the following vectors
x <-  c(NA, NA, NA)
y1 <- c( 1, NA, NA)
y2 <- c( 1, NA, NA, NA)
#What would you expect both_na(x, y1) to return?
both_na(x, y1)
#What about both_na(x, y2)?
both_na(x, y2)
#What should both_na(x, y2) return? 
#y2 & x haven't the same length and this is generating a warning message

##################################################################################################
#Good function names
##################################################################################################
vect_x <- 1:10
#Consider the following function, f2()
f2 <- function(x) {
  if (length(x) <= 1) return(NULL)
  x[-length(x)]
}
#CHOSE remove_last()
remove_last <- function(x) {
  if (length(x) <= 1) return(NULL)
  x[-length(x)]
}
remove_last(vect_x)

##################################################################################################
#Argument names
##################################################################################################
#Take a look at this function, which calculates a confidence interval for a population mean:
mean_ci <- function(c, nums) {
  se <- sd(nums) / sqrt(length(nums))
  alpha <- 1 - c
  mean(nums) + se * qnorm(c(alpha / 2, 1 - alpha / 2))
}
#The argument nums is a sample of data
#argument c controls the level of the confidence interval.
#c = 0.95, we get a 95% confidence interval.

#Are c and nums good arguments names?
# Rewrite mean_ci to take arguments named level and x
mean_ci <- function(level, x) {
  se <- sd(x) / sqrt(length(x))
  alpha <- 1 - level
  mean(x) + se * qnorm(c(alpha / 2, level / 2))
}
##################################################################################################
#Argument order
##################################################################################################
#Arguments are often one of two types:
#(1)Data arguments supply the data to compute on.
#(2)Detail arguments control the details of how the computation is done.
# Alter the arguments to mean_ci
mean_ci <- function(x, level = 0.95) {
  se <- sd(x) / sqrt(length(x))
  alpha <- 1 - level
  mean(x) + se * qnorm(c(alpha / 2, 1 - alpha / 2))
}
##################################################################################################
#Return statements
##################################################################################################
#mean_ci(numeric(0))
#returns a confidence interval with missing values at both ends 
#In this case, they decided it would make more sense 
#to produce a warning "x was empty" and return c(-Inf, Inf)
# Alter the mean_ci function
mean_ci <- function(x, level = 0.95) {
  if (length(x) == 0) {
    warning("`x` was empty", call. = FALSE)
    return(c(-Inf, Inf))
  }
  se <- sd(x) / sqrt(length(x))
  alpha <- 1 - level
  mean(x) + se * qnorm(c(alpha / 2, 1 - alpha / 2))
}
#Test : it's worked
x <- vector()
mean_ci(x)
##################################################################################################
#What does this function do?
##################################################################################################
#Here's a poorly written function, which is also available in your workspace:
x <- c(1, 2, NA, 4, 5)

f <- function(x, y) {
  x[is.na(x)] <- y
  cat(sum(is.na(x)), y, "\n")
  x
}
#Your job is to turn it in to a nicely written function.
#What does this function do?
# Call f() with the arguments x = x and y = 3
f(x,2)

# Call f() with the arguments x = x and y = 10
f(x,10)
##################################################################################################
#Let's make it clear from its name
##################################################################################################
#f() takes a vector x and replaces any missing values in it with the value y

#Imagine you came across the line df$z <- f(df$z, 0)
#Now you know, it replaces any missing values in the column df$z with value 0

#Let's rename our function and arguments to make it obvious to everyone what we are trying to achieve.
z <- rnorm(1:10)
z[c(2,4)] <- NA
df<- data.frame(z)
str(df)

# Rename the function f() to replace_missings()
replace_missings <- function(x, replacement) {
  # Change the name of the y argument to replacement
  x[is.na(x)] <- replacement
  cat(sum(is.na(x)), replacement, "\n")
  x
}

# Rewrite the call on df$z to match our new names
df$z <- replace_missings(df$z,0)
##################################################################################################
#Make the body more understandable
##################################################################################################
#in your code, it's pretty obvious what you are trying to achieve
#The body of our replace_missings() function is still a little messy.
replace_missings <- function(x, replacement) {
  # Define is_miss
  is_miss <- is.na(x)
  
  # Rewrite rest of function to refer to is_miss
  x[is_miss] <- replacement
  cat(sum(is_miss), replacement, "\n")
  x
}
##################################################################################################
#Much better! But a few more tweaks...
##################################################################################################
#Did you notice replace_missings() prints some output to the console?
#That output isn't exactly self-explanatory.
#It would be much nicer to say "2 missing values replaced by the value 0".

#The official R way to supply simple diagnostic information is the message() function

replace_missings <- function(x, replacement) {
  is_miss <- is.na(x)
  x[is_miss] <- replacement
  
  # Rewrite to use message()
  message(sum(is_miss), " missings replaced by the value ", replacement)
  x
}

# Check your new function by running on df$z
replace_missings(df$z, replacement = 0)