## Put comments here that give an overall description of what your
## functions do

## This function creates and rea container (a list) for the matrix that is to be inverted, and its inversion.
## The matrix does not need to be supplied when calling makeCacheMatrix.
## The 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        setMatrix <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        getMatrix <- function() x
        
        setInv <- function(inverse) inv <<- inverse
        
        getInv <- function() inv
        
        list(setMatrix = setMatrix,
             getMatrix = getMatrix,
             setInv = setInv,
             getInv = getInv)
}


## This function takes a 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$getMatrix()
        inv <- solve(data, ...)
        x$setInv(inv)
        inv
}
