## Put comments here that give an overall description of what your
## functions do

## Calculating the inverse of a Matrix is usually a costly computation. 
## Here I cache the inverse so no further calculations have to be made.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      get <- function() x
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function() inv
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the matrix created before. 
## If the matrix hasn't changed and the inverse has already been calculated,
## it obtains the inverse from the cache rather than recalculating it.

cacheSolve <- function(x, ...) {
      MyInv <- x$getInverse()
      if(!is.null(MyInv)){
            message("cached data")
            return(MyInv)
      }
      MyMat <- x$get()
      MyInv <- solve(MyMat,...)
      x$setInverse(MyInv)
      print(MyInv)
}
