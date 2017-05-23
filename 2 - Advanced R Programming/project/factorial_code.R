Factorial_loop <- function(x)
{
  if(is.character(x))
    stop("Your input is a character. It must be numeric.")
  if(x < 0)
    stop("Please enter a number greater than 0.")
  
  if (x == 0)
  {
    result <- 1
    return(result)
  }
  else
  {
    result <- 1
    while (x > 0)
    {
      result <- result * x
      x <- x - 1
    }
    return(result)
  }
}

###########################################################
library(purrr)

Factorial_reduce <- function(x)
{
  if(x > 0)
  {
    reduce(1:x, function(x, y)
    {
      x * y
    })
  }
}
  

###########################################################

Factorial_func <- function(x)
{
  if (x == 0)
  {
    return(1)
  }
  else
  {
    x * Factorial_func(x - 1)
  }
}

###########################################################


factorial_tbl <- c(1, 2, 6, rep(NA, 22))


Factorial_mem <- function(n){
  if (n < 0){
    message("Please enter a number greater than 0.")
  }
  else if (n == 0){
    1
  }
  else if(!is.na(factorial_tbl[n])){
    factorial_tbl[n]
  } 
  
  else {
  factorial_tbl[n] <<- n * Factorial_mem(n-1)
  Factorial_mem(n)
  }
}


#https://github.com/beyhangl/AdvancedRCourse/blob/master/ProjectPart1.R
