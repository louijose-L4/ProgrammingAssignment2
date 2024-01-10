## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#the 'makecachematrix is calculates the inverse of matrix and saves in the cache for future purpose'

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve(x)
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
#the function cachesolve first checks if the inverse matrix is saved in cache already,
#if so. it returns the cached data else if solves the inverse matrix and saves the cached data.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
  inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
  }

