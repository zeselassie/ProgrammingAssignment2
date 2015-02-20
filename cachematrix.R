## The following two functions cache the inverse of a matrix. 
## As long as the matrix is not changed the inverse of the matrix
## will be calculated only once and the cached inverse will be retrieved.

## This function has four functions that retrive or set the matrix or
## its inverse. The assumption is the input matrix is invertible.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve calculates and returns the inverse of a matrix. 
## It retrieves from the cache if it is already calculated and cached.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if (!is.null(x$inverse)){
        message("getting cached data")
        return(inverse)
    }
    inv <- solve(x$get(), ...)
    x$setinverse(inv)
    inv
}
