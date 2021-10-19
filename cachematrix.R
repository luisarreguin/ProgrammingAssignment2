## Put comments here that give an overall description of what your
## functions do

## The assignment consists to create 2 functions:
## 1-makeCacheMatrix, 
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL # initializing inverse as NULL
  set <- function(y) {
         x <<- y
         inver <<- NULL
  }
  get <- function()x # get the matrix x
  setinv <- function(inverse) inversa <<- inverse
  getinv <- function() {
                            inver <- ginv(x)
                            inver %*%x 
                            } # obtain the inverse of the matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## This is used to get the cache data

cacheSolve <- function(x, ...) { # gets cache data
  inv <- x$getinv()
  if(!is.null(inv)) { # check whether inverse is NULL
    message("getting cached data")
    return(inv) # returns inverse value
  }
  data <- x$get()
  inv <- solve(data, ...) # calculates inverse value
  x$setinv(inv)
  inv ## Return a matrix that is the inverse of 'x'
}
