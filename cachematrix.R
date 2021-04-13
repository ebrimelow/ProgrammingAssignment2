## This code contains a pair of functions that cache the inverse of a matrix.


## This function, makeCacheMatrix, creates a special "matrix" object that can 
## cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(Y){
    X <<- Y
    inv <<- NULL
  }
  
  get <- function() mat
  
  setsolve <- function(solve) inv <<- solve
  getsolve <- function() inv
  
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## This function, cacheSolve, computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cacheSolve should retrieve the  
## inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  inv <- X$getsolve()
  if(!is.null(inv)) {
    message("Getting cached data...")
    return(inv)
  }
  data <- X$get()
  inv <- solve(data,...)
  X$setsolve(inv)
  inv
}