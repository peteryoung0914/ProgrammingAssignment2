## This is a script made for an R Programming Assignment on lexical scoping

## These functions cache the inverse of a matrix (makeCacheMatrix) and retrieve
## a cached inverse matrix if available (cacheSolve)

## Example input and output: 
## > matrix1 <- makeCacheMatrix(matrix(c(4, 0, 0, 4, 1, 1, 0, 1, 0), c(3, 3)))
## > cacheSolve(matrix1)
## Already in cache! Retrieving cached data
##      [,1] [,2] [,3]
## [1,] 0.25    0   -1
## [2,] 0.00    0    1
## [3,] 0.00    1   -1

## FUNCTION 1: makeCacheMatrix
## Set and get the value of a matrix, set and get the value of an inverse matrix

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
  set <- function(y) {
          x <<- y
          i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## FUNCTION 2: cacheSolve
## Returns the inverse of a matrix created with makeCacheMatrix, retrieving from cache if present

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
