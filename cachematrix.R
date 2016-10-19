#Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below I present two  functions that store a matrix and cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set.inverse <- function(inverse) inv <<- inverse
  get.inverse <- function() inv
  list(set = set,
       get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)

}

## The function cacheSolve computes the inverse of a  "matrix" created by the function  
## makeCacheMatrix. If the inverse has already been computed (and the 
## matrix has not been  changed), then it has to  retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$get.inverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$set.inverse(inv)
}
