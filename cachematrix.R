## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
