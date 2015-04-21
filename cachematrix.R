## File contains functions that cache a matrix's inverse and return the inverse
## instead of calculating it repeatedly

## Create a "matrix object" capable of storing its inverse

makeCacheMatrix <- function(x = matrix()) {
    #stores the inverse
    matInv <- NULL
    
    getMatrix <- function() x
    setMatrix <- function(mat)
    {
        # assign mat to x only if the differ.
        # if they are identical, preserve the value of matInv
        if(!identical(x, mat))
        {
            x <<- mat
            matInv <<- NULL
        }
    }
    
    getInverse <- function() matInv
    setInverse <- function(i) matInv <<- i
    
    
    list(getMatrix = getMatrix, setMatrix = setMatrix, 
         getInverse = getInverse, setInverse = setInverse)
}


## computes the inverse of a matrix if it is not cached and returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invr <- x$getInverse()
    if(!is.null(invr))
    {
        return(invr)
    }
    
    mat <- x$getMatrix()
    invr <- solve(mat, ...)
    x$setInverse(invr)
    invr
}
