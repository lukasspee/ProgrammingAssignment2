## A set of functions for efficiently obtaining the inverse
## of a matrix. After the initial computation of this
## inverse matrix the result is cached for subsequent calls.
##
## Usage example:
## > m <- makeCacheMatrix(matrix(c(4, 2, 7, 6), 2, 2))
## > cacheSolve(m)
##      [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4
## > cacheSolve(m)
## getting cached data
##      [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4

## Create a matrix with cache capabilities for returning its inverse.
##
## Arguments:
## `x` A plain R matrix object.
##
## Returns:
## A wrapper around `x` with the following functions:
## `set(m)`        Update the wrapped matrix with the value `m`.
## `get()`         Return the wrapped matrix.
## `setInverse(i)` Update/set the inverse of the wrapped matrix
##                 (do not call this function manually).
## `getInverse()`  Return the inverse of the wrapped matrix, return NULL
##                 if this value has not been computed yet.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
            x <<- y
            inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Return the inverse of a matrix created using `makeCacheMatrix`.
## Use cached value if available.
##
## Arguments:
## `x`   A 'matrix' object as returned by `makeCacheMatrix`.
## `...` Optional arguments that are passed on to the regular `solve`
##       function (see its documentation for more info).
##
## Returns:
## The inverse of the matrix wrapped in `x` as a plain R matrix.

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    cached <- x$get()
    inverse <- solve(cached, ...)
    x$setInverse(inverse)
    inverse
}
