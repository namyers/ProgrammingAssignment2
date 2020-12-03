## These functions allow to solve for the inverse
## of a Matrix and stores it in a cache to prevent
## the need to perform the calculation again.

## This function defines some smaller functions
## to store information in the parent environment
## and creates a matrix to use with the following
## function.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function uses the above function as an
## object to run the calculations and, if the
## data doesn't change, will pull up the recent
## inverted matrix.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached matrix")
    return(i)
  }
  matrix <- x$get()
  i <- solve(matrix, ...)
  x$setinverse(i)
  i
}
