## Week 2 Assignment by SeHun Shin.
## => This is a source code about R Programming - week 2 - assignment.


## Write a short comment describing this function
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL # inner variable for inversion of matrix
  # inner variable m is for matrix
  # setting the matrix
  set <- function(y) {
    m <<- y
    i <<- NULL
  }
  # getting the matrix
  get <- function() m
  
  # setting inversion of matrix
  setInverse <- function(inverse) i <<- inverse
  # getting inversion of matrix
  getInverse <- function() i
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
  
  # 'm' is an inner variable for inverse.
  m <- x$getInverse()
  # if m is not null, return m with a message.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # if m is null, this calculate the inversion of matrix
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m # return m
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