## makeCacheMatrix creates a special matrix that can keep the
## original matrix and inverse of the matrix (if set)
## cacheSolve receives an instance of makeCacheMatrix and
## checks if inverse is available in cache. If not, computes the inverse,
## saves to the cache, and return the result

## This function wraps a matrix and returns a list of 
## operation to set and get the matrix and it's inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(inputMatrix) {
    x <<- inputMatrix
    inverseMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(inverseM) inverseMatrix <<- inverseM
  getInverse <- function() {
    inverseMatrix
  }
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## This function returns the inverse of matrix stored in parameter
## It first checks for cache, if not availalbe, computes the cache.
## Otherwise just returns from the cache.

cacheSolve <- function(x, ...) {
  
  inverseMatrix <- x$getInverse()
  
  if(!is.null(inverseMatrix)) { ## Found in cache
    return(inverseMatrix)
  }
  
  m <- x$get()
  inverseMatrix <- solve(m)
  x$setInverse(inverseMatrix)
  inverseMatrix
}
