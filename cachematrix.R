## Author: Natalie Tan
## Date: 7th April 2016
## Assignment: Caching the Inverse of a Matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse. It contains a list which performs 4 functions:
## 1.set the value of the vector
## 2.get the value of the vector
## 3.set the value of the mean
## 4.get the value of the mean
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ##  1.set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x

  ## 3.set the value of the inverse of the matrix
  setinverse <- function(myinverse) m <<- myinverse
  
  ## 4.get the value of the inverse of a matrix
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
   }
   data <- x$get()
   m <- solve(data)
   x$setinverse(m)
   m
}
