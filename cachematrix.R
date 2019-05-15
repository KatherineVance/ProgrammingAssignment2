## These two functions compute and store the inverse of a matrix
## so that it doesn't need to be re-calculated each time it will
## be used

## This function takes a matrix as input and creates a list of 4 
## elements that can store the matrix and its cached inverse.

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) I <<- solve
  getinverse <- function() I
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates and stores the inverse of the 
## matrix in makeCacheMatrix.

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
