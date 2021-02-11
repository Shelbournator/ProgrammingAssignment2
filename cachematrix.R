## This is a pair of functions that work together to cache the inverse of a matrix. The first function computes the inverse and
## stores it in the cache. The second function checks if the data is in the cache, and computes it if not.
##

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inversemat <- NULL
        set <- function(y) {
                x <<- y
                inversemat <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) inversemat <<- solve
        getInverse <- function() inversemat
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cachesolve
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        inversemat <- x$getInverse()
        if(!is.null(inversemat)) {
                message("getting cached data")
                return(inversemat)
        }
        data <- x$get()
        inversemat <- solve(data, ...)
        x$setInverse(inversemat)
        inversemat
}
