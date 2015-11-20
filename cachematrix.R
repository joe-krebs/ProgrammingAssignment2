## File: cachematrix.R
## Utility functions to support matrix and inverse-caching smart matrix
## Joe Krebs
## For Coursera course rprog-034, "R Programming"

## Function: makeCacheMatrix
##
## Cached matrix supports set, get, setinverse and getinverse functions.
## Purpose is to act as a smart matrix container that remembers and returns its internally-
## cached inverse (thus avoiding costly, unnecessary matrix inverse operations) in the case
## of repeat calls when the cached matrix has not changed.
##
makeCacheMatrix <- function(mx = matrix()) {
    
    ## Initialize the cached matrix and null matrix inverse
    mx_cached <<- mx
    mx_inv_cached <<- NULL
    
    ## Set or change the cached matrix. If the new/changed matrix differs from cached value,
    ## invalidate the cached matrix inverse
    set <- function(data = NA, nrow = 1, ncol = 1, byrow = FALSE, dimnames = NULL ) {
        mx_new <- matrix(data, nrow, ncol, byrow, dimnames)
        if (is.null(mx_inv_cached) | !identical(mx_cached, mx_new)) {
            mx_cached <<- mx_new
            mx_inv_cached <<- NULL
        }
    }
    
    ## Retrieve the cached matrix
    get <- function() {
        mx_cached
    }
    
    ## Set the cached matrix inverse
    setinverse <- function(inverse) {
        mx_inv_cached <<-inverse
    }
    
    ## Retrieve the cached matrix inverse
    getinverse <- function() {
        mx_inv_cached
    }
    
    # Return internal function list
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Function: cacheSolve
##
## Return a matrix that is the inverse of 'x'. If matrix x has not changed since last call,
## return the cached matrix inverse, else compute the inverse and update the cache, then
## return
##
cacheSolve <- function(x, ...) {
    
    ## Check the cache; if not null, the cached inverse can be returned    
    mx_inv <- x$getinverse()
    if (!is.null(mx_inv)) {
        message("using cached data")
    } else {
        
        ## Cache was null, so compute the inverse and set the cache
        mx_inv <- solve(x$get())
        x$setinverse(mx_inv)
    }
    
    ## Return the inverse of 'x'
    mx_inv
}

## Function: test
##
## Run a simple battery of unit tests. Expected results are listed above each 
## call to cacheSolve()
##
test <- function() {
    mm1 <- makeCacheMatrix()
    mm1$set(data = c(1:7, 9, 10), nrow = 3, ncol = 3)
    mm1.same <- mm1
    mm2 <- makeCacheMatrix(matrix(data = c(2:8, 10, 11), nrow = 3, ncol = 3))
    
    ## Expect inverse of mm1, no message
    print(cacheSolve(mm1))
    
    ## Expect inverse of mm1, with cache hit message
    print(cacheSolve(mm1))
    
    ## Expect inverse of mm1.same, with cache hit message
    print(cacheSolve(mm1.same))
    
    ## Expect inverse of mm2, no message
    print(cacheSolve(mm2))
    
    ## Expect inverse of mm2, no message
    mm2$set(data = c(2:8, 10, 11), nrow = 3, ncol = 3)
    print(cacheSolve(mm2))
}

## Uncomment to run tests
test()
