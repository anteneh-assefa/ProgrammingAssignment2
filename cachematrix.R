## these two functions cache inverse of a square matrix and return it rather than computing repeatedly
##  

## makeCacheMatrix creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv_m <- NULL
    set <- function(y) {
        x <<- y
        inv_m <<- NULL
    }
    get <- function() x ## get x
    setinv_m <- function(inverse) inv_m <<- inverse ## set inverse of x
    getinv_m <- function() inv_m ## get inverse of x
    list(set = set, get = get,
         setinv_m = setinv_m,
         getinv_m = getinv_m)
}


## cacheSolve computes inverse of the special matrix returned by the above function

cacheSolve <- function(x, ...) {
    inv_m <- x$getinv_m()
    if(!is.null(inv_m)) {
        message("getting cached data")
        return(inv_m)
    }
    data <- x$get()
    inv_m <- solve(data, ...)
    x$setinv_m(inv_m)
    inv_m ## return inverse of 'x'
}
