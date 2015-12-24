## The first function, makeCacheMatrix creates a vector which is really a list containing a function to
#set the value of the vector
#get the value of the vector
#set the value of the inverse matrix
#get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get, setinv = setinv , getinv = getinv)
}

##The following function calculates the inverse matrix of the special "vector" 
#created with the above function. However, it first checks to see if 
#the inverse matrix has already been calculated. If so, it gets the inverse matrix from 
#the cache and skips the computation. Otherwise, it calculates the 
#inverse matrix of the data and sets the value of the inverse matrix in the cache via the 
#setinv function.

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
