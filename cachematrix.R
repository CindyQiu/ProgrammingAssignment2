## Matrix inversion is usually a time consuming computation, so it's worth having 
## a function to cache the inverse rather than computing it repeatedly. 

## makeCacheMatrix is a function to create a special matrix which contains the
## space to cache the inverse along with a list of four functions to set the value 
## of the original matrix, get the original matrix, set the value of the inverse
## and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve returns the inverse of the special matrix as created by makeCacheMatrix.
## It first checks whether the inverse has been calculated. If yes, it returns the
## inverse directly. If not, it calculates the inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (!is.null(inv)){
    message("getting cached data")
    return (inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv
}
