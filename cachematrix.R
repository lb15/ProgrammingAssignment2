## Two functions to calculate, cache, and return the inverse of matrices.

## Function creates object to store matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i = NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinvr <- function(solve) i <<- solve
  getinvr <- function() i
  list(set = set, get = get,
       setinvr = setinvr,
       getinvr = getinvr)
}


## Function to return cached matrix inverse stored in makeCacheMatrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinvr()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinvr(i)
  i
}

