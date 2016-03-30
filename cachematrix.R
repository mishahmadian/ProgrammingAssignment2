## Matrix inversion is usually a costly computation and there 
## may be some benefit to caching the inverse of a matrix rather 
## than compute it repeatedly. 

## makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    rev <- NULL
    set <- function(y) {
        x <<- y
        rev <<- NULL
    }
    get <- function() x
    setRev <- function(revers) rev <<- revers
    getRev <- function() rev
    list(set = set, get = get, setRev = setRev, getRev = getRev)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        rev <- x$getRev()
        if(!is.null(rev)) {
            message("Getting from cached data")
            return(rev)
        }
        dat <- x$get()
        rev <- solve(dat, ...)
        x$setRev(rev)
        rev
}
