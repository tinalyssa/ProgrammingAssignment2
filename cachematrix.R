## The following pair of functions, makeCacheMatrix and cacheSolve, computes for
## the inverse of a matrix and stores it in cache. Use the makeCacheMatrix
## function to store the matrix and its inverse, and the cacheSolve to either
## access the inverse from cache, if available, or to solve for it and store in
## cache.

## makeCacheMatrix creates a list containing functions that:
##      (1) set the value of the matrix
##      (2) get the value of the matrix
##      (3) set the value of the inverse
##      (4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
        i <- NULL
        set <- function(y) {
              x <<- y
              i <<- NULL
  }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
  
}

## cacheSolve is a function that returns the inverse of a matrix 'x'. If
## the inverse has already been calculated before, the function retrieves the
## data from cache. Otherwise, it calculates for it and stores it in the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(is.null(i)) {
                message("getting cached data")
                return (i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
