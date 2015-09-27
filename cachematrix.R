## makeCacheMatrix creates the matrix and set the inverse, cacheSolve pulls
## matrix data from Cache

## Creating matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## get inverse of matrix from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv = x$getinv()
  if (!is.null(inv)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  mat.data = x$get()
  inv = solve(mat.data, ...)
  x$setinv(inv)
  
  return(inv)
}

