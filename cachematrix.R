## These functions allow you to create a matrix and cache its inverse
## such that the inverse can be stored rather than computed repeatedly.

## makeCacheMatrix creates a special "matrix" which is actually a list of
## functions, providing for:
## (a) setting the value of the matrix (set),
## (b) returning the value of the matrix (get),
## (c) setting the value of the inverse (setInverse),
## and (d) returning the value of the inverse (getInverse).

makeCacheMatrix <- function(x = matrix()){
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve returns the value of the inverse, if the inverse has already been
## computed. If not, the function computes the inverse, caches its value via 
## the setInverse function, and returns the value of the inverse.

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setInverse(i)
    i
}
