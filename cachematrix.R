## Functions to compute the inverse of a matrix and cache the answer for future use.
## Using an already-stored value means that the inverse is not re-calculated every
## time it is needed, so avoiding computationally-expensive operations.

## Function to create a special 'matrix' object that can cache its inverse.
## Returns a list of functions that can set and retrieve the value of a matrix
## as well as also set and retrive the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL

    ## Function to set the matrix stored in the makeCacheMatrix function
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    ## Function to retrieve the matrix stored in the makeCacheMatrix function
    get <- function() {
        x
    }
    
    ## Function to store the value of the inverse of the matrix
    setinverse <- function(inverse) {
        inv <<- inverse
    }
    
    ## Function to retrieve the stored value of the inverse of the matrix
    getinverse <- function() {
        inv
    }
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Computes the inverse of the special 'matrix' created using makeCacheMatrix.
## If it has already been computed, the cached version is returned otherwise
## the inverse is calculated and stored for future use.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## If the inverse has already been computed, return the stored value
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("Getting cached data")
        return(inv)
    }
    ## Otherwise compute and return the inverse, and store it for future use
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
