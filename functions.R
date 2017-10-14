
#####################################################################################

# Exercise 1: Writing R functions

#####################################################################################

### 1(a): filling in the blanks
#
# Sum values in a column of a data frame.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d, provided as a string
#
# RETURN VALUE:
# if the specified column exists and contains numbers, returns the sum of
# all values in the column; otherwise, returns NULL
sum_column <- function(d, var) {
  # Set a default value to return
  result <- NULL
  x <- d[[var]] # Remember, as we showed in class: there are two ways
  # to access a column, and this is one; you should try
  # and figure out why the other way won't work here,
  # but that's not part of the homework
  if (!is.null(x)) { # This tests to see whether the column exists in
    # d; again, you should try and find a way to test
    # this out for yourself to verify that it's true,
    # but that's not part of the homework
    # YOUR CODE HERE: if x contains numbers, set the variable
    # result to be the sum of the values in x
    if (is.numeric(x)){
      sum(x) -> result
    }
    # You will need to do a bit of research in order to figure out how
    # to test whether a vector contains numbers.
  }
  # YOUR CODE HERE: return the result
  return(result)
}

#####################################################################################

### 1(b): a function with one argument and one return value
#
# Sum values in a vector.
#
# ARGUMENTS:
# x: a vector
#
# RETURN VALUE:
# if the vector contains numbers, returns the sum of all values;
# otherwise, returns NULL
#
# [YOUR FUNCTION HERE]
#define the function with one arg x
my_sum <- function(x) {
  #loop the vector given
  #when it is a number
  if (is.numeric(x)) {
    #give the variable result a initial value 0
    result <- 0
    for (n in x) {
      #print(n)
      #add the value of every data in x to result
      result <- result + n
    }
  } else {
    #give the variable result a initial value NULL
    result <- NULL
  }
  #return the value of result
  return(result)
}

#####################################################################################

### 1(c): a function with two argument and one return value
#
# ARGUMENTS: x <- a vector
#            k <- a number
#
# RETURN VALUE:
# if all elements in vector x and the given k are numbers, returns the sum divided by k;
# otherwise, returns NULL
#
sum_divided_by <- function(x, k) {
  #check if both x and k are numeric (x already checked once inside th function my_sum)
  if (is.numeric(x) && is.numeric(k)) {
    #recall the my_sum function for calculate the sum of x
    #then divide it by k
    result <- my_sum(x) / k
  } else {
    #give the variable result a initial value NULL
    result <- NULL
  }
  #return the value of result
  return(result)
}

#####################################################################################

### 1(d): one more function
#
# ARGUMENTS: x <- a vector
#
# RETURN VALUE:
# if the vector contains numbers returns its mean,
# which equals to its sum divided by its length;
# otherwise, returns NULL
#
my_mean <- function(x) {
  #calculate the length of the vector
  #don't need to check if l is a number here
  l <- length(x)
  #recall the sum_divided_by function for calculate the sum of x devided by the length
  #if x or l are not numeric, it returns NULL
  mean <- sum_divided_by(x, l)
  #return the value of mean
  return(mean)
}



#####################################################################################

# Exercise 2: Working with ggplot

#####################################################################################

### 2(a): Creating violin plots
#
# Return a violin plot.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the dependent variable, provided as a
#     string
# grouping_var: the name of a column of d containing a grouping variable,
#               provided as a string
#
# RETURN VALUE:
# A ggplot plot object containing a violin plot, grouped by the values
# of the grouping variable.
#
grouped_violin_plot <- function(d, var, grouping_var) {
  # Create the base ggplot object
  p <- ggplot2::ggplot(d, ggplot2::aes_string(y=var,
                                              x=grouping_var,
                                              fill=grouping_var))
  # YOUR CODE HERE: Create a violin plot
  #draw the violin plot with geom_violin() function
  p <- p + ggplot2::geom_violin()
  #now it is default color
  #color can be defined with the scale_fill_manual() function
  #p <- p + ggplot2::scale_fill_manual(values=c("red", "green", "blue"))
  
  return(p) }




#####################################################################################

# Exercise 3: Permutation tests

#####################################################################################

### 3(a): Writing a more generic function for taking a test statistic
#
# Difference in the medians between two groups.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the dependent variable, provided as a string
# grouping_var: the name of a column of d containing a grouping variable, provided as a string
# group1: the value of grouping_var that corresponds to the first group
# group2: the value of grouping_var that corresponds to the second group
#
# RETURN VALUE:
# The median value of var for the first group, minus the median value of var for the second
# group.
#
difference_in_medians <- function(d, var, grouping_var, group1, group2) {
  d_1 <- dplyr::filter(d, get(grouping_var) == group1)
  d_2 <- dplyr::filter(d, get(grouping_var) == group2)
  # YOUR CODE HERE: 
  x_1 <- d_1[[var]]
  x_2 <- d_2[[var]]
  #call the median() function to calculate the median number..
  result <- median(x_1)-median(x_2)
  #assign the difference in the medians to the variable 'result'
  #the function round(x,n) keep n digits after the '.' of float number x
  #result <- round(median(x_1)-median(x_2), 1)
  return(result)
}

#####################################################################################

### 3(b): Writing a more generic function of the randomization function
#
# Randomize the order of a column.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the variable to randomize,
#      provided as a string
#
# RETURN VALUE:
# A data frame or tibble exactly the same as d, except with the order of
# var permuted randomly.
#
randomize <- function(d, var) {
  # YOUR CODE HERE: generate a shuffled version of d[[var]]
  d[[var]] <- sample(d[[var]], nrow(d))
  return(d) }

#####################################################################################

### 3(c): A function to get a statistic for multiple permutations
#
# Perform a permutation test for two groups.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of the column in d on which the test statistic will be calculated,
#      provided as a string
# grouping_var: the name of the column in d which gives the grouping
# group1: the value of grouping_var corresponding to the first group
# group2: the value of grouping_var corresponding to the second group
# statistic: a function yielding a test statistic, which takes as input
#            a data frame, the name of a variable on which to calculate the
#            test statistic, the name of a grouping variable, the value of
#            the grouping variable corresponding to the first group, and
#            the value of the grouping variable corresponding to the second
#            group
# n_samples: the number of permutation samples to draw (default: 9999)
#
# RETURN VALUE:
#
# A list containing two elements:
#
#  - observed: the value of statistic() in d
#  - permuted: a vector containing the values of statistic() under n_samples
#              permutations
#
permutation_twogroups <- function(d, var, grouping_var, group1, group2, statistic,
                                  n_samples=9999) {
  observed_statistic <- statistic(d, var, grouping_var, group1, group2)
  permutation_statistics <- rep(0, n_samples)
  for (i in 1:n_samples) {
    # YOUR CODE HERE: use randomize(...) to create a permutation and then
    #                 fill in the vector permutation_statistics with the
    #                 value of statistic(...) for this new permutation
    fake <- randomize(d, var)
    #print(fake)
    permutation_statistics[i] <- statistic(fake, var, grouping_var, group1, group2)
    #fake_1 <- dplyr::filter(fake, get(grouping_var) == group1)
    #fake_2 <- dplyr::filter(fake, get(grouping_var) == group2)
    #print(fake_1)
    #permutation_statistics[i] <- median(fake_1[[var]]) - median(fake_2[[var]])
    #print(my_mean(fake_1[[var]]))
  }
  result <- list(observed=observed_statistic,
                 permuted=permutation_statistics)
  return(result)
}

#####################################################################################

### 3(f): Test statistics
#
new_test_statistic <- function(d, var, grouping_var, group1, group2) {
  d_1 <- dplyr::filter(d, get(grouping_var) == group1)
  d_2 <- dplyr::filter(d, get(grouping_var) == group2)
  # YOUR CODE HERE: 
  x_1 <- d_1[[var]]
  x_2 <- d_2[[var]]
  #call the my_mean() function to calculate the mean of each..
  result <- my_mean(x_1)-my_mean(x_2)
  #assign the difference in the means to the variable 'result'
  return(result)
}



#####################################################################################

### 3(g): Calculating p-values
#
#Insert the following functions in your functions file.
#
permutation_pvalue_right <- function(p) {
  n_above <- sum(p$permuted >= p$observed)
  n_samples <- length(p$permuted)
  return((n_above + 1)/(n_samples + 1))
}
#
permutation_pvalue_left <- function(p) {
  n_below <- sum(p$permuted <= p$observed)
  n_samples <- length(p$permuted)
  return((n_below + 1)/(n_samples + 1))
}








