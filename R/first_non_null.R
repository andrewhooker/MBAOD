# from microbenchmark package

first_non_null <- function (...) 
{
  isnotnull <- function(x) !is.null(x)
  Find(isnotnull, list(...))  
}
