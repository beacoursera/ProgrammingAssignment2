## The following pair of functions calculate and caches the inverse of a matrix.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {                  ## caches the value of a matrix
    x <<- y
    i <<- NULL
  }
  get <- function() x                   ## gets the value of the cached matrix
  setinv <- function(solve) i <<- solve ## calculates and caches the inverse of the matrix
  getinv <- function() i                ## getse the cached inverse of the matrix
  list(set = set, get = get,            ## creates a list of the four functions above
       setinv = setinv, 
       getinv = getinv)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
#  If the inverse has already been calculated (and the matrix has not changed), 
#  then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {                     ## if inverse has already been calculated, 
    message("getting cached data")      #  then it is retrieved from the cache
    return(i)
  }
  data <- x$get()                       
  i <- solve(data, ...)                 ## returns a matrix that is the inverse of 'x'
  x$setinv(i)
  i
}
