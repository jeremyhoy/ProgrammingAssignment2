## Matrix inversion is computationally expensive; the functions in this file allow the
## a matrix to be stored and the result of an inversion to be cached, so that it can 
## be reused.  This is based heavily on the mean example provided in the assignement.

## makeCacheMatrix creates a list of functions that store and retrieve a matrix and also 
## store and retrieve the result of a matrix inversion.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve, when passed object such as that created by makeCacheMatrix, will either 
## calculate the inversion of the matrix passed in the object and then cache the result, 
## or, if the inversion has already been calculated and cached, return the cached result.

cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
