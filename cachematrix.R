## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly
## Below is a pair of functions that cache the inverse of a matrix

## Function 1: makeCacheMatrix
## makeCacheMatrix: This function creates a special "matrix" object that can
## cache its inverse.
## Assume that the function only takes in invertible matrix

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) I <<- Inverse
        getInverse <- function() I
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## Function 2: cacheSolve
## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then the cacheSolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        I <- x$getInverse()
        if (!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        matrix <- x$get()
        I <- solve(matrix, ...)
        x$setInverse(I)
        I
}

##Testing Code
matrix1 <- makeCacheMatrix(matrix(1:4,2,2))
matrix1$get()
matrix1$getInverse()
cacheSolve(matrix1)
cacheSolve(matrix1)
matrix1$getInverse()
