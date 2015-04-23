makeCacheMatrix <- function(x = matrix()) {
  ## This function creates a special "matrix" object that can cache its inverse.
  
  # inv will store the cached inverse matrix
  inv = NULL
  set = function(y) {
    #  <<- operator used to assign a value to an object in an environment 
    #that is different from the current environment.
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  # Return the matrix with the new defined functions
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
  ## This function computes the inverse of the special "matrix" returned 
  ## by makeCacheMatrix above. If the inverse has already been calculated 
  ## (and the matrix has not changed), then the cachesolve should retrieve 
  ## the inverse from the cache.
  
  inv <- x$getinv()
  
  # If the inv is already calculated, return inv
  if (!is.null(inv)) {
    return(inv)
  }
  
  # The inverse is not yet calculated, so calculate..
  data <- x$get()
  inv <- solve(data, ...)
  
  # Cache the inv
  x$setinv(inv)
  
  # Return inv
  inv
}
