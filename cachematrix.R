## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of the matrix.
## These two functions cache the inverse of a matrix.


## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## input: an inversible matrix
## output: special "matrix" object

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve computes the inverse of the special "matrix" 
## input: "matrix" returned by 'makeCacheMatrix' function
## output: retrieved inverse matrix, if already been calculated for the unchanged matrix, 
##         computed inverse matrix, otherwise

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
