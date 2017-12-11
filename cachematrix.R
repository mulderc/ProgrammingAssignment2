## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function takes a matrix x, assumed to be invertable, and creates an object 
## which has the data supplied by X, a null data object m, and 4 functions 
## set, get, setsolve, getsolve
## this object is justed by cacheSolve to calculate the inverse of the matrix and 
## then save the results so we don't have to caluculate it again

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function
## this function takes the object create by the makeCacheMatrix()
## sees if the inverted X matrix has been solved
## If it has then it returns the already solved inverted matrix
## if it has not already been solved then it calculates the inverse of the matrix X

cacheSolve <- function(x, ...) {
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
