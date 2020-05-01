## A pair of functions to cache the inverse of a matrix
## in a custom object so that the solve function does 
## not need to be called more than once on the same matrix.

## makeCacheMatrix(x = matrix()) creates an object with two
## variables:
##
## x: is the matrix to be cached or an empty matrix if none
##    provided
## i: is the inverse of the matrix which is null when the
##    object is first created or the value of x is modified
##
## Cache object has 4 functions:
##
## set(x): sets initial matrix x and clears cached inverse i
## get(): returns the value of the initial matrix x
## setinverse(inverse): sets the value of the cached inverse
##                      i to the given inverse matrix
## getinverse(): returns the value of the cached inverse i

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL                              ##initializing i 
  set <- function(y) {
    x <<- y
    i <<- NULL  ##using "<<-" operator so that value of x an i is set in a differnt 
                ##environment than the global environment
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve(x, ...) takes an object created by the
## makeCacheMatrix function and checks to see if the
## inverse variable i is NULL.  If an inverse solution
## already exists, this is returned.  Otherwise the
## inverse is calculated and stored in the cache object
## for subsequent use before it is returned by this function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse()
  if(!is.null(i)) {                  ##if matrix inverse is already there 
                                     ## in the cache ,it is fetched
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i        
  
}