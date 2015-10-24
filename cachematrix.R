## The functions contained in this file allows to perform cache-enhanced
## calculation of the inverse of a matrix

## This function creates a special matrix object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y)
    {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special matrix returned by
## makeCacheMatrix

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv))
    {
        message("Returning cached inverse")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    inv
}
