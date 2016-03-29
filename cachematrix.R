## Overall description of what functions do

## makeCacheMatrix is a function that sets the value of a matrix, gets the value of the matrix, sets the inverse value of the matrix via the SOLVE function, and gets the inverse value of matrix 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
      x <<- y
      inv <<- NULL
  } 
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}


## cacheSolve is a function that checks to see if the inverse of a matrix has already been calculated. 
## If so, function gets inverse from the cache and skips the computation. 
## Else, function calculates inverse and sets the value of inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setmean(inv)
    inv
}
