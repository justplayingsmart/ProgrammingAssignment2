## This program creates functions that can store and cache
## the inverse of a matrix, so that it does not have to be
## recomputed

## This function creates a special object that, which is 
## a list containing functions to:
## 1. set the value of the matrix 
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                inverse <<- NULL
                x <<- y
        }
        get <- function() x
        setInverse <- function(inv) inverse <<- inv
        getInverse <- function() inverse
        list(set = set, get = get, getInverse = getInverse, 
                setInverse = setInverse)
}


## cacheSolve calculates the inverse of a given matrix. If
## the inverse is already cached, the function simply returns
## the cached inverse, with a note that it is doing so. If not,
## the function calculates the value of the inverse using the
## solve() function, and it caches the inverse for future use.

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        i <- solve(x$get(), ...)
        x$setInverse(i)
        i
}
