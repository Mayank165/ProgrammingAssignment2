##
##            Programming Assignment 2: Lexical Scoping 
##
## makeCacheMatrix makes an object capable of catching its inverse,
## cacheSolve returns the inverse of the matrix passed to makeCacheMatrix
## for this code the matrix is always considered to be invertible

## makeCacheMatrix makes a special matrix object that can catch its inverse
## It also contains four functions for storing and retrieving the values

makeCacheMatrix <- function(x = matrix()) {
        
        inv = NULL # initializing an empty variable for storing the inverse
        
        ## function for storing the value of the matrix to be inverted
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## function for retrieving the value of the stored matrix
        
        get = function() x
        
        ## function for storing the value of the calculated inverse
        
        setinv <- function(solve) inv <<- solve
        
        ##function for retrieving the current value of the inverse
        
        getinv <- function() inv
        
        #list of functions
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve returns the inverse of 'x', either from the cached memory or
## after calculating the inverse if the cache is empty. In the latter case it
## also stores the calculated value in the cache. The function takes an argument
## of type makeCacheMatrix so a matrix needs to be passed to makeCacheMatrix() 
## before running this function

cacheSolve <- function(x, ...) {

        inv <- x$getinv() #retrieving the currently cached value
       
        ## if a currently cached value exits it is returned and no further
        ## calculations are performed
        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        #otherwise
        
        data <- x$get()         # gets the matrix to be inverted
        inv <- solve(data, ...) # computes the inverse of the matrix
        x$setinv(inv)           # stores the value in the cache
        inv                     # returns the value of the inverse
}