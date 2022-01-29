## Function that enhances matrix with ability to store internally inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    cInverse <- NULL
    set <- function(y) {
        x <<- y
        cInverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) cInverse <<- inverse
    getInverse <- function() cInverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Solves matrix and caches the result.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getInverse()
    if (!is.null(s)) {
        message("Using cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data,...)
    x$setInverse(s)
    s
}
