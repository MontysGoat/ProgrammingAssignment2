## This script contains a pair of functions that allow an inverse matrix to be 
## created and cached. Calculating an inverse matrix can be timely, so caching 
## it will allow that calculation to be bypassed.

## The makeCacheMatrix function creates a list of four functions:
## 
## 1. set the value of the (invertible) matrix
## 2. get the value of the (invertible) matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, 
                get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function returns the cached inverse matrix if it has already 
## been calculated, otherwise it calculates the inverse and returns it.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
        ## Return a matrix that is the inverse of 'x'
}
