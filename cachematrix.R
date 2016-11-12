## Below are two functions that are used to create a special matrix, 
## that stores a square matrix and cache's its inverse. It must be kept
## in mind that only square matrices can have an inverse. 


##The first function, makeCacheMatrix creates a special "Matrix", which 
##is really a list containing a function to:
##set the values of the matrix
##get the values of the matrix
##set the values of the inverse of the matrix
##get the values of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x<<- y
    inv <<- NULL
  }
  get <- function()  x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates the inverse of
## the special "Matrix" created with the above function.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of 
## the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached inverse of the matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
