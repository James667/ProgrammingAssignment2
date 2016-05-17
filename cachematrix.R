## The first function creates a matrix that caches 
## its inverse if available.
## The second function retreives the stored inverse
## if available, otherwise it calculates it.

## makeCacheMatrix is a list of functions to set 
## (cache) and get the matrix, and to set (cache)
## and get the inverse.

makeCacheMatrix <- function(x = matrix()) {
    x_inv <- NULL
    set <- function(y) {
        x <<- y 
        x_inv <<- NULL
    }
    get <- function() x 
    set_inv <- function(inv) x_inv <<- inv
    get_inv <- function() x_inv
    list(set = set, get = get,
         set_inv = set_inv,
         get_inv = get_inv)
}


## cacheSolve checks the cache to see if the inverse
## has been computed already, returns it if it has
## otherwise it computes and returns it.

cacheSolve <- function(x, ...) {
    x_inv <- x$get_inv()
    if(!is.null(x_inv)) {
        message("retrieving cached inverse")
        return(x_inv)
    }
    data <- x$get()
    message("computing inverse")
    x_inv <- solve(data, ...)
    x$set_inv(x_inv)
    x_inv
        ## Return a matrix that is the inverse of 'x'
}
