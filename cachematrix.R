## Matrix inversion is a costly computation   
## It will be useful to cache inverse of a matrix instead of repeated calculations
# The below two functions explains how to cache a inverse of a matrix

## The below function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) I <<- inverse
  getinverse <- function() I
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The below function computes the inverse of matrix created by makeCacheMatrix function.
## If the inverse already exists it retrieves from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  I <- x$getinverse()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setinverse(I)
  I
}
