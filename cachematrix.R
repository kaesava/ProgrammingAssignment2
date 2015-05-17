## These functions cache the inverse of a matrix

## This function can be used to get and set the matrix and a cache of the 
## inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function calculates the inverse of a  metrix; if the inverse is available
## in the cache, it is returned, else it is calculated, stored and returned
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse()
  if(!is.null(i)) {
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
}