## These functions cache the inverse of a square inversible matrix. 
## Caching this function is useful so that we don't need to keep writing the function
## a lot of times during the program.

## This first function creats a list of functions that will set and get the matrix we want to invert
## and set and get the inverse of the stored matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getsolve <- function()i
  list(set=set,get=get,setinverse=setinverse,getsolve=getsolve)
}


## This function gets the matrix stored in the makeCacheMatrix and returns its inverse.
## The inverse computed in the cacheSolve is then stored in the makeCacheMatrix.Due to this,
## the functions first checks if there is a inverse already stored.

cacheSolve <- function(x, ...) {
  i <- x$getsolve()
  if(!is.null(i)){
    message ("getting cached data")
    return (i)
  }
  data <- x$get()
  i = solve(data)
  x$setinverse(i)
  return(i)       
}
