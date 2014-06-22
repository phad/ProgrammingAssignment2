## A pair of functions that allow a matrix to be stored with
## a cached copy of its inverse.  Since the inverse of a matrix
## can be costly to compute, these functions make retrievals
## of the inverse very cheap after the first call.

## Obtain a list of functions that permit storing of a matrix
## and caching of its inverse.  The matrix is assumed to be
## square.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Obtain the inverse of supplied matrix 'x'. If a value
## has been previously cached, return that.  Otherwise
## calculate the inverse and store it in the cache for
## subsequent retrieval.
## 'x' is assumed to be a square matrix.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
