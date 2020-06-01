## Wrap a matrix with functions so that its inverse will be calculated once and
## cached so it can be efficiently retrieved

## Given a matrix, wrap it with functions that will get and set the value and
## get and set the inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL # store the cached inverse
    
    # function to set the matrix and clear the inverse
    set <- function(y) { 
        x <<- y
        inverse <<- NULL
    }
    
    # function to get the matrix
    get <- function() x
    
    # function to set the inverse matrix
    setinverse <- function(inv) inverse <<- inv
    
    # function to get the inverse matrix
    getinverse <- function() inverse
    
    # return the list of functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Pass a CacheMatrix (see makeCacheMatrix) which will return the inverse of the 
## matrix, using the cached value if it has already been calculated, or 
## calculates and caches the inverse before returning it
cacheSolve <- function(x, ...) {
    ## Return the cached inverse if available
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## cached inverse isn't available, calcluate and set it
    data <- x$get()     # get the matrix
    inv <- solve(data)  # get the inverse
    x$setinverse(inv)   # set the inverse
    inv                 # return the inverse
}
