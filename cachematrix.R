## Calculate inverse of matrix is somewhat a complex operation, especially for
## large square matrix. Instead to calculate it everytime when needed, we could
## cache the result for better performance, and this could be achieved by below
## two functions, makeCacheMatrix and cacheSolve.
##
## makeCacheMatrix create a object to store the given matrix and provides
## interface to set and get inverse of the matrix calculated by cacheSolve.

## makeCacheMatrix is to create a special object to easily retrieve the inverse
## of 'x' without redundant calculation.
##
## To calculate the inverse of 'x', x should be a square matrix, otherwise the
## error would be returned.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    ## Get the underlying matrix 'x'
    get <- function() x
    set <- function(m) {
        x <<- m
        inverse <<- NULL
    }
    ## Get the cached inverse of 'x'.
    getinverse <- function() inverse
    ## Set the inverse of 'x'.
    setinverse <- function(m) inverse <<- m
    
    list(get = get, set = set, getinverse = getinverse, setinverse = setinverse)
}


## cacheSolve is to calculate the inverse of 'x' which is the special matrix we
## created by makeCacheMatrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if (!is.null(inverse)) {
        ## If the inverse of 'x' was cached, just return it.
        return(inverse)
    }
    ## Otherwise, we need to calculate the inverse of 'x' and cache it.
    m <- x$get()
    inverse <- solve(m)
    x$setinverse(inverse)
    
    ## Also return the result.
    inverse
}
