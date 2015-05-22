## makeCacheMatrix takes an invertible matrix and returns a list of functions
## that allow: the value of the matrix to be (re-)set; get the value of the
## matrix; set and cache the inverse of the matrix; and retrieve the inverse of
## the matrix. The second function, cachesolve, takes an object created by
## makeCacheMatrix and retrieves the matrix inverse from the cache if it has
## already been determined; otherwise it calculates, caches, and returns, the
## matrix inverse.

## makeCacheMatrix is a function that given an invertible matrix returns a
## list of functions that can: set the value of the matrix; get the value of
## the matrix; set and cache the inverse of the matrix; and get the inverse of
## the matrix.

makeCacheMatrix <- function(x = matrix()) {
    mInv <- NULL
    set <- function(y) {
        x <<- y
        mInv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) mInv <<- inverse
    getInverse <- function() mInv
    list(set = set, get = get,
         setInverse = setInverse, getInverse = getInverse)
}


## Given a list returned from makeCacheMatrix, cacheSolve checks to see whether
## the inverse of the matrix has already been cached and if it has it returns
## the inverse. Otherwise the value of the original matrix is retrieved, the
## inverse is calculated and cached, then the inverse is returned.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mInv <- x$getInverse()
    if(!is.null(mInv)) {
        message("Getting cached matrix")
        return(mInv)
    }
    data <- x$get()
    mInv <- solve(data, ...)
    x$setInverse(mInv)
    mInv
}
