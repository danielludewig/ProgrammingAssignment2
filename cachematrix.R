## Put comments here that give an overall description of what your
## functions do

## Function that turns  matrix into an object that can cache it's inverse within
## the functions environment. 

makeCacheMatrix <- function(x = matrix()) {
      I <- NULL
      set <- function(y) {
            x <<- y
            I <<- NULL
      }
      get <- function() x
      setInverse <- function(Inverse) I <<- Inverse
      getInverse <- function() I
      list(set = set, get = get, 
           setInverse = setInverse, getInverse = getInverse)
}


## A function that computes the inverse of the object created by the above 
## function, unless has previously been calculated, in which case it will 
## retrieve the result from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      I <- x$getInverse()
      if(!is.null(I)) {
            message("getting cached data")
            return(I)
      }
      data <- x$get()
      I <- solve(data, ...)
      x$setInverse(I)
      I
}
