## The functions defined below enable the use of a wrapper for a matrix so that
## the result of its inverse calculation can be cached. The makeCacheMatrix
## function creates the wrapper itself, and the cacheSolve function will return
## the inverse matrix for any such wrapper. If cacheSolve has already
## calculated the inverse for an input and cached it, it will simply return
## that inverse rather than recalculating. See the comments preceding the
## makeCacheMatrix and solveCache functions for more detail.

## The makeCacheMatrix function returns a wrapped matrix so that its inverse
## matrix can be cached after the first time it is calculated. The get and
## set functions provide access to the wrapped matrix itself, while the
## getinverse and setinverse provide access to the inverse matrix.
## For example, the cacheSolve function uses those functions to check
## whether a cached inverse exists already, and to provide the inverse
## value to be cached after the initial calculation.

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


## The cacheSolve function takes a cache matrix as input and returns the
## inverse matrix. If that inverse had already been calculated for that cache
## matrix, the cached value will be returned, avoiding any need to recalculate.
## If the inverse had not yet been calculated for that cache matrix instance,
## it will be calculated and returned, but also cached for any subsequent calls
## to this function for that same cache matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        ## Check if the inverse is available in the cache matrix
        if(!is.null(i)) {
                message("returning cached inverse")
                return(i)
        }
        ## The inverse is not available, need to calculate it
        ## Get the raw matrix that is wrapped in the cache matrix
        data <- x$get()
        ## Calculate the inverse of the matrix
        i <- solve(data, ...)
        ## Set the inverse result on the cache matrix
        x$setinverse(i)
        i
}
