## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This Function returns list that contains functions associated
## with given matrix x. Function setsolve sets inverse of matrix x
## and function getsolve lets you get its value within
## makeCacheMatrix function scope.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<<-function(y){
    x<<-y
    m<<-NULL
  }
  get <- function() x
  setsolve<-function(solve) m<<-solve
  getsolve<-function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## Write a short comment describing this function
## This function checks if there is anything stored as m, where m is taken
## from getsolve function from argument x (created by function makeCacheMatrix).
## If there is one, it returns it and prints mesage about it.
## Otherwise it calculates inverse of the matrix that was passed
## to function makeCacheMatrix and it saves it with function setsolve.



cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
