## Put comments here that give an overall description of what your functions do
## => This is a source code about R Programming - week 2 - assignment.
##    Before executing this code, you need to install a package named "MASS".
##    This package contains a function called "ginv" returns the inversion of matrix.
##    So if you don't install this package, do it before you execute this code.


install.packages("MASS")
library(MASS)

## Write a short comment describing this function
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- ginv(data)
  x$setInverse(m)
  m
}


## Test code
#m <- matrix(c(1,0,0,0,1,0,0,0,1), 3,3)
m <- matrix(c(1,3,2,4),2,2)
cachedMat <- makeCacheMatrix(m)
print(cachedMat$get())

## setting inversed matrix into cacheMatrix
cacheSolve(cachedMat)

## once again, setting inversed matrix
cacheSolve(cachedMat)
# with a message, got a inversed matrix