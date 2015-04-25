## Caching the Inverse of a Matrix:
##
## This .R files contains two functions to compute the inverse of a given "matrix" 
## but caching the inverse of the matrix rather than computing it repeatedly. 
## The file contains following two functions:
##
## 1. makeCacheMatrix: This function creates a special "matrix" object 
##    that can cache its inverse.
## 2. cacheSolve: This function computes the inverse of the special "matrix" 
##    returned by makeCacheMatrix above. If the inverse has already been calculated 
##    (and the matrix has not changed), then the function cacheSolve retrieves 
##    the inverse from the cache. The inverse of a square matrix is done 
##    with the solve function in R.  

## The function makeCacheMatrix creates a special "matrix", 
## which is a list containing a function to
## a) set the values of the matrix (set)
## b) get the values of the matrix (get)
## c) set the values of the inverse of the matrix (setInv)
## d) get the values of the inverse of the matrix (getInv)

makeCacheMatrix <- function(x = matrix()) {
  XI <- NULL
  set <- function(Y) {
    x <<- Y
    XI <<- NULL
  }
  get <- function() x
  setInv <- function(Xinv) XI <<- Xinv
  getInv <- function() XI
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## The following function calculates the inverse of a "matrix" created 
## with the above function. It first checks to see if the inverse of the matrix
## has already been calculated. If so, it gets the inverse from the cache 
## and skips the computation. Otherwise, it calculates the inverse of the data 
## and sets the value of the inverse in the cache via the setInv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  XI <- x$getInv()
  if(!is.null(XI)) {
    message("getting cached data")
    return(XI)
  }
  data <- x$get()
  XI <- solve(data, ...)
  x$setInv(XI)
  XI
}
