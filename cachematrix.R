
# makeCacheMatrix

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

cacheSolve <- function(x, ...) {
  
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