## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL                                       # matrix inverse is initialized as NULL
  setMatrix <- function(y) {                              #value of the Matrix is set
    x <<- y
    invMatrix <<- NULL                                    #in case of a new matrix, matrix inverse is reset to NULL
  }
  getMatrix <- function() x                               #value of the matrix is returned                           
  setInverse <- function(inverse) invMatrix <<- inverse   #value of matrix inverse is assigned in parent environment  
  getInverse <- function() invMatrix                      #value of the matrix inverse is fetch
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

##cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated,cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  invMatrix <- x$getInverse()                             #value of the matrix inverse is fetched from makeCacheMatrix
  if(!is.null(invMatrix)) {                               #if matrix inverse is not NULL
    return(invMatrix)                                     #matrix inverse is returned
  }
  Matrixdata <- x$getMtrix()                              #original Matrix Data is fetched
  invMatrix <- solve(Matrixdata, ...)                     #solve()is used to get matrix inverse
  x$setInverse(invMatrix)                                 #matrix inverse is set
  invMatrix                                               #matrix inverse is returned
}
