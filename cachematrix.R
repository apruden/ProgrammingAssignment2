# Creates a cacheable matrix object wrapping a matrix x. The cacheable matrix
# is used to cache the inverse of x calculated with cacheSolve.
#
# Args:
#   x: matrix to be wrapped in a cacheable matrix object.
#
# Return:
#   cacheable matrix object.
#
makeCacheMatrix <- function(x = matrix()) {
    cached <- NULL
    set <- function(data) {
        x <<- data
        cached <<- NULL
    }

    get <- function() x
    setinv <- function(inv) cached <<- inv
    getinv <- function() cached

    list(set=set, get=get, setinv=setinv, getinv = getinv)
}


# Calculates the inverse of a cacheable matrix object created with makeCacheMatrix
# function. The inverse is calculated once (the first function call). The result is
# cached and returned on next calls.
#
# Args:
#   x: cacheable matrix object created with makeCacheMatrix function.
#
# Return:
#   matrix inverse or x.
cacheSolve <- function(x, ...) {
    inv <- x$getinv()

    if (!is.null(inv)) {
        message('Return cached result')
        return(inv)
    }

    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
