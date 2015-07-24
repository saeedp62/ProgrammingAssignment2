## Following are a pair of functions that cache the inverse of a matrix

##1- makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##2- cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##               If the inverse has already been calculated (and the matrix has not changed),
##               then cacheSolve should retrieve the inverse from the cache.

## The first function, makeVector creates a special "matrix", 
## which is really a list containing a function to
##    set the value of the matrix
##    get the value of the matrix
##    set the value of the inverse matrix
##    get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  matInv <- NULL
  set <- function(y) {
    x <<- y
    matInv <<- NULL
  }
  get <- function() x
  setinv <- function(inv = matrix()) matInv <<- inv
  getinv <- function() matInv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The following function calculates the inverse of the special "matrix" created 
## with the above function. However, it first checks to see if the inverse matrix
## has already been calculated. If so, it gets the inverse matrix from the cache 
## and skips the computation. Otherwise, it calculates the inverse of the matrix
## and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matInv <- x$getinv()
  if(!is.null(matInv)) {
    message("getting cached data")
    return(matInv)
  }
  data <- x$get()
  matInv <- solve(data)
  x$setinv(matInv)
  matInv
}
