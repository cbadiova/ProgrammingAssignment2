## the following are functions that cache the inverse of an invertible matrix

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(solve) {
    inv <<- solve
  }
  getInv <- function() {
    inv
  }
  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv)
}

# The cacheSolve function computes the inverse of the "matrix" 
# returned by function makeCacheMatrix
# It checks if the inverse has already been computed. 
# It computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setInv
  inv
}



