
# makeCacheMatrix


## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  mat_inverse <- NULL
  set <- function(y) {
    x <<- y
    mat_inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) mat_inverse <<- inverse
  getInverse <- function() mat_inverse
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# cacheSolve function  

# This function computes the inverse of the special "matrix" created by 
# makeCacheMatrix above. If the inverse has already been calculated (and the 
# matrix has not changed), then it should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  mat_inverse <- x$getInverse()
  if (!is.null(mat_inverse)) {
    message("getting cached data")
    return(mat_inverse)
  }
  mat <- x$get()
  mat_inverse <- solve(mat, ...)
  x$setInverse(mat_inverse)
  return(mat_inverse)
}