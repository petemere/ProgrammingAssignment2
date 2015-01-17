## This pair of functions provide:
## 1. The ability to produce an object of a new data type that stores a matrix
##    and its inverse, and
## 2. A function that returnes the inverse of the matrix part of an object of
##    this new data type.
## The purpose of these functions are to avoid calculating the inverse of a 
## given matrix multiple times if possible through cacheing the result of the 
## first calculation.

#### makeCacheMatrix ####
## Think of this function as returning a new kind of data type: a
## matrix-with-functions.  Specifically, the functions allow the storage and 
## retrieval of a matrix and its inverse.
##
## While a normal list object could contain a matrix and its inverse, using
## the makeCacheMatrix function guarantees correct maintenance of the two
## matrices.
## 
## For example, if you have a matrix and its inverse stored in a normal list
## object, then when you change the matrix you have to remember to delete the 
## inverse.  Using a makeCacheMatrix object means that administration will be 
## done automatically.
##
## Because this function creates a 'makeCacheMatrix' object, it is not necessary
## to supply a matrix object initially, just like it isn't necessary to supply
## a series of numbers when first creating a numeric vector object.

makeCacheMatrix <- function(x = matrix()) {
        invr <- NULL
        
        setMatrix <- function(y) {
                x <<- y
                invr <<- NULL
        }
        
        getMatrix <- function() x
        
        setInv <- function(inverse) invr <<- inverse
        
        getInv <- function() invr
        
        list(setMatrix = setMatrix,
             getMatrix = getMatrix,
             setInv = setInv,
             getInv = getInv)
}


#### cacheSolve ####
## This function replaces the standard 'solve' R function for use with an object
## created from makeCacheMatrix.  Just like the original solve function, it 
## returns the inverse of the matrix passed to it, but this function uses
## functions available in the makeCacheMatrix object to see if the inverse is
## available before trying to calculate it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        ## Extract the inverse matrix from the makeCacheMatrix object.
        inv <- x$getInv()
        ## If it is there...
        if(!is.null(inv)) {
                ## Return the cached inverse.
                message("getting cached data")
                return(inv)
        }
        ## If the inverse isn't there, extract the matrix,
        data <- x$getMatrix()
        ## Calculate the inverse the usual way,
        inv <- solve(data, ...)
        ## Store the calculated inverse in the makeCacheMatrix object for
        ## potential later use, and
        x$setInv(inv)
        ## Return the calculated invese matrix.
        inv
}
