## Put comments here that give an overall description of what your
## functions do


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL
        setMatrix <- function(y) {
                x <<- y
                invMatrix <<- NULL
        }
        getMatrix <- function() x
        setInvMatrix <- function(inv) invMatrix <<- inv
        getInvMatrix <- function() invMatrix
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInvMatrix = setInvMatrix,
             getInvMatrix = getInvMatrix)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
## I assumed that the matrix supplied is always invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMatrix <- x$getInvMatrix()
        if(!is.null(invMatrix)) {
                message("Cached invert matrix data")
        } else {
                data <- x$getMatrix()
                invMatrix <- solve(data)
                x$setInvMatrix(invMatrix)
        }
        x$getInvMatrix()
}
