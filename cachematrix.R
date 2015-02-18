## makeCacheMatrix takes a matrix as argument and creates a list of functions 
## 1. setmatrix - sets the value of the matrix to input parameter and resets inverse to NULL
## 2. getmatrix - retrieves the value of the matrix
## 3. setinverse - calculates the value of the inverse of the matrix using R function solve and stores for 
## future retrieval
## 4. getinverse - retrieves the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setmatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve takes as argument the matrix created and stored by the makeCacheMatrix function,
## and returns the inverse, if it is available. If not available (that is, it is NULL), 
## it calculates the inverse using solve function in R and stores the value for future retrieval.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getmatrix()
  m <- solve(data)
  x$setinverse(m)
  m
}
