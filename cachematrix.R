## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y = matrix()){ 
    x <<- y       ##Whenever we change the matrix, inverse is reset to NULL.
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(z = matrix()) inverse <<- z
  getinverse <- function() inverse
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  a <- x$get()
  if(!is.null(inverse)){
    message("Printing inverse from cache")
    return(inverse)
  }
  inverse <- solve(a)
  x$setinverse(inverse)
  inverse
}