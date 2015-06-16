## cacheMatrix provides functions to create a matrix object and a cached 
## result of its inverse.


## makeCacheMatrix: Create a "matrix" object to cache inverse result.
makeCacheMatrix <- function(x = matrix()) {
  # initially no inverse value
  i <- NULL
  # will 'memento' the matrix input 'y' to internal 'x'
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  # returns our local 'x'
  get <- function() x
  # store some value for the inverse 
  setinv <- function(inv) i <<- inv
  # return the cache inverse value
  getinv <- function() i
  # define and return the "matrix" list object
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve: Return the inverse of 'inCache', using the matrix cache.                
cacheSolve <- function(inCache, ...) {
  # Try to access the cached value
  i <- inCache$getinv()
  # if we have cache..
  if(!is.null(i)) {
    # return the cached value
    return(i)
  }
  # otherwise, retrive the stored data
  data <- inCache$get()
  # calculate the inverse, passing any optional parameters
  i <- solve(data, ...)
  # set the calculated value to cache
  inCache$setinv(i)
  # return the calculated value
  i
}

