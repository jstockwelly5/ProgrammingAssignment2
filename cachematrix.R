## These functions create a matrix object that can store a matrix inverse so that it will not be recomputed

## This function creates the cached matrix object

makeCacheMatrix <- function(x = matrix()) {

      inv <- NULL
      
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      
      get <- function() {
            x
      }
      
      setinv <- function(inverse) {
            inv <<- inverse
      }
      
      getinv <- function() {
            inv
      }
      
      list(set = set, get = get, setinv = setinv, getinv = getinv)
      
}


## This function checks the cache to see if it exists, and if not, calculates the inverse and stores it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
      inv <- x$getinv()
      if (!is.null(inv)) {
            message("Getting cached data")
            return(inv)
      }
      
      data <- x$get()
      
      inv <- solve(data)
      
      x$setinv(inv)
      
      inv
      
}
