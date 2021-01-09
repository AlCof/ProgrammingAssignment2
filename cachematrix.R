setwd('D:/Coursera/RProgramming/Assignments')
##
## Setting input x as a matrix 
## and setting the solved value as "n" as null
## then changing every other reference to "mean" to "solve"
## this function will create a special matrix which can cache its inverse
makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
  n <- NULL
  set <- function(y) {
    x <<- y
    n <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) n <<- solve
  getsolve <- function() n
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
##
## similar step is done here, changing "mean" to "solve" and "m" to "n"
## this part of the function would solve for 
## the inverse of the matrix that was returned by the function above
cacheSolve <- function(x, ...) {
  n <- x$getsolve()
  if(!is.null(n)) {
    message("getting the inverse of the matrix")
    return(n)
  }
  data <- x$get()
  n <- solve(data, ...)
  x$setsolve(n)
  n
}
