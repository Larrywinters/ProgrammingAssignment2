## the function makeCacheMatrix creates a matrix and the 
## function cacheSolve gives the matrix reverse. 
 

## the function makeCacheMatrix has two major goals.
# 1. create a set of functions and an environment to store inputs and outputs in the cacheSolve
# 2. produce a list structure containing a few functions so that the calling for functions x$get_rv is aviallable later on
# Given all the four functions in the makeCacheMatrix is almost the same to the CacheMean example, I will not go over them because they do the exact same job.

makeCacheMatrix <- function(x = matrix()) {
        output <- NULL
        set <- function(y) {
                x <<- y
                output <<- NULL
        }
        get <- function() x
        set_rv <- function(rv) output<<- rv
        get_rv <- function() output
        list(
                set = set, get = get, set_rv = set_rv, get_rv = get_rv
        )
}


## The cacheSolve functions check and store formal variables through functions defined in makeCacheMatrix. All the following are almost the same as the Cachemean example
## except mean() is replaced by solve to do matrix inversion.

cacheSolve <- function(x, ...) {
        output <- x$get_rv()
        if (!is.null(output)) {
                message("getting cached data")
                return(output)
        }
        mymatrix <- x$get()
        output <- solve(mymatrix, ...)
        x$set_rv(output)
        output
        ## Return a matrix that is the inverse of 'x'
}

#test
m <- makeCacheMatrix(matrix(c(0,1,2,0,1,-1,2,4,0),3,3))
cacheSolve(m)
