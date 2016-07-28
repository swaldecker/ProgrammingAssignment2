## Functions for calculating the inverse of a nonsingular matrix or, 
## if previously calculated, retrieving the inverse from the cache.  

## A function that returns a list of functions allowing the inverse of 
## matrix 'x' to be stored in cache. 

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    
    # a function to reset the matrix 'x'
    set <- function(y) {
        
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x # get the matrix 'x'
    
    setinv <- function(inverse_mtx) inv <<- inverse_mtx # cache 'inverse_mtx'
    
    getinv <- function() inv # get the inverse of 'x'
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## A function that takes a special matrix object, 'x', which is generated 
## by the function 'makeCacheMatrix', and returns the inverse of the matrix 
## stored in 'makeCacheMatrix' (retrieved with the get() function). 
## If the inverse of this matrix has already been computed, 'cacheSolve' 
## retrieves it from the cache instead of computing it again. 

cacheSolve <- function(x, ...) {
     
    
    inv_mtx <- x$getinv()
    
    if( !is.null(inv_mtx)) {
        message("inverse read from cache")
        return(inv_mtx)
    }
    
    mtx <- x$get() # get matrix from 'makeCacheMatrix'
    inv_mtx <- solve(mtx) # solve for its inverse
    x$setinv(inv_mtx) # cache inverse
    
    inv_mtx
    
}
