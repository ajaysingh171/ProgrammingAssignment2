## What my functions do
## Matrix inversion is very time consuming, hence the following functions enable the user to cache the inverse 
## of a matrix. Thus, instead of computing the inverse of a matrix repeatedly, it can be cached to save time.


## This function creates a special "matrix" object that can cache its inverse. 
## The special"matrix" is really a list containing functions to: 
## set the value of the matrix, get the value of the matrix,
## set the value of the inverse and get the value of the inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x

  setinverse <- function(inverse) m <<- inverse
  
  getinverse <- function() m
  
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. However,
## it first checks to see If the inverse has already been calculated (and the matrix has not changed), 
## If so, it gets the inverse from the cache and skips the calculation should retrieve the inverse from the cache.
## Otherwise, it calculates the inverse of the matrix and sets the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }

  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
