## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    ##Cache the matrix. New value is set and inverse is reset.
    x <<- y
    inv <<- NULL
  }
  get <- function() x ##returns the matrix.
  setInv <- function(inverse) inv <<- inverse ##Cache the inverse matrix
  getInv <- function() inv ##returns the inverse matrix.
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  m <- x$get()
  inv <- solve(m, ...)
  x$setInv(inv)
  inv
}