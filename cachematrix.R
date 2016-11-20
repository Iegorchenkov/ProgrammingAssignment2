## Caches inversed matrix

## Creates a special matrix inversed version of which can be cached
## Example: makeCachMatrix(matrix(1:4, 2, 2))

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solved) inv <<- solved
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Work with matrix created with makeCacheMatrix
## If the matrix is inversed already it returns inversed matrix
## othrewise it inverses the matrix and caches it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cashing data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
