##R Programming course: Week 3 (2nd) assignement : Lexical Scoping
##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function (x = matrix ()) {
  inv <- NULL
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  get <- function () x
  setInverse <- function (inverse) inv <<- inverse
  getInverse <- function () inv
  list(set = set, get = get,
    setInverse = setInverse,
    getInverse = getInverse)
}

#The following function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the value already exists, value will be taken from cache, if not, it will be calculate using the function. 
cacheSolve <- function(x, ...) {
 ## Return the inverse of "x"
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv_x <- solve(data,...)
  x$setinv(inv)
  inv
}