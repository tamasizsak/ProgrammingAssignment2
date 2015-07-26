## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## In this assignment I wrote a pair of functions that cache the inverse of a
## matrix.

## makeCacheMatrix(): creates a special “matrix” object that can cache its
## inverse. It returns a list which is containing functions to:

## 1: set the matrix
## 2: get the matrix
## 3: set the inverse of the matrix
## 4: get the inverse of the matrix

## This list will be the input to the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
        i   = NULL
        set = function(y) {
                x <<- y
                i <<- NULL
        }
        get = function() x
        setinv = function(inverse) i <<- inverse
        getinv = function() i
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Return a matrix that is the inverse of 'x', where 'x' is the output of
## makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        i = x$getinv()
        
        # If the inverse of the matrix has already been calculated get it from
        # the cache and skips the computation.
        
        if (!is.null(i)) {
                message("Getting the cached data...")
                return(i)
        }
        
        # Otherwise, calculates the inverse of the matrix and sets the value of
        # the inverse in the cache via the setinv function.
        
        mat.data = x$get()
        i = solve(mat.data, ...)
        x$setinv(i)
        
        return(i)
}