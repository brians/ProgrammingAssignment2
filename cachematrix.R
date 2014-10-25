## implements caching matrix inversion function. sample usage:
##   m <- matrix(c(1,2,3,4), nrow=2, ncol=2)
##   a <- makeCacheMatrix(m)
##   a$get()
##   cacheSolve(a)
##   cacheSolve(a) # retrieves from cache


## given a matrix x, return a cMatrix, really a list of functions to
##   set the value of cMatrix;
##   get the value of cMatrix;
##   set the value of cMatrix inverse; and
##   get the value of cMatrix inverse from the cache

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    set_inv <- function(inv) i <<- inv
    get_inv <- function() i
    list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}


## given cMatrix x, retrieve its inverse from the cache if possible.
## otherwise compute, cache, and return the inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$get_inv()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$set_inv(i)
    i
}
