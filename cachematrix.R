## With the following functions we can find the inverse of a matrix and cache it.
## By caching the inverse of a matrix there will be no need to calculate it more than once. 
## If we cache the inverse then we can just retrieve it and avoid time-consuming computations.

## How to get the inverse of a matrix:

## a<- makeCacheMatrix(X)    -- we create the special "matrix" object where X is an invertible matrix
## cacheSolve(a)             -- we calculate the inverse and we cache it
## cacheSolve(a)             -- we retrieve the inverse so we don't have to calculate it


## It creates a special "matrix" object in which we can cache the inverse of each matrix.

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

## If the inverse has been calculated then it will be retrieved. Otherwise it will be calculated and then it will be cached.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}