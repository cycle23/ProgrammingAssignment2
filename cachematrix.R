## cacheMatrix provides functions to create a matrix object and a cached 
## result of its inverse.


## makeCacheMatrix: Create a "matrix" object to cache inverse result.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve: Return the inverse of 'inCache', using the matrix cache.                
cacheSolve <- function(inCache, ...) {
        ## Return a matrix that is the inverse of 'inCache'
  i <- inCache$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- inCache$get()
  i <- solve(data, ...)
  inCache$setinv(i)
  i
}

