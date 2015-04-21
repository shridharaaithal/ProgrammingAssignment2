## File contains functions that cache a matrix's inverse and return the inverse
## instead of calculating it repeatedly

## Usage examples
## ma <- makeCacheMatrix(1:4, 2, 2)
## cacheSolve(ma)  #returns the inverse of matrix(1:4, 2, 2) and caches the value
## ma$setMatrix(matrix(1:4, 2, 2)) #because the new matrix is same as the existing matrix
##                                 #inverse calculated earlier remains unchanged
## ma$getInverse() #returns the previously calculated inverse
## ma$setMatrix(matrix(4:7, 2, 2)) #replaces the matrix and sets inverse to null
## ma$getInverse() #returns NULL
## cacheSolve(ma) #computes the inverse of the new matrix and returns the value


## Create a "matrix object" capable of caching its inverse
makeCacheMatrix <- function(x = matrix()) {
    # Stores the inverse
    matInv <- NULL
    
    # Return the matrix that was used to create the special object
    getMatrix <- function() 
    {
        x
    }
    
    # Assign a new value to the stored matrix. 
    # Assignment happens only when the new matrix differs from the existing matrix 
    setMatrix <- function(mat)
    {
        # Assign mat to x only if they differ.
        # If they are identical, preserve the value of matInv
        if(!identical(x, mat))
        {
            x <<- mat
            matInv <<- NULL
        }
    }
    
    # Return the stored value of the matrix inverse
    getInverse <- function() 
    {
        matInv
    }
    
    # Set the value of matrix inverse
    setInverse <- function(i) 
    {
        matInv <<- i
    }
    
    # Return a list of various objects created
    list(getMatrix = getMatrix, setMatrix = setMatrix, 
         getInverse = getInverse, setInverse = setInverse)
}


## Computes the inverse of a matrix if it is not cached and returns the inverse
cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    
    # Get the value of the matrix inverse. If it is computed return it.
    invr <- x$getInverse()
    if(!is.null(invr))
    {
        return(invr)
    }
    
    # Following lines are executed only when matrix inverse is not computed yet.
    
    # Retrieve the matrix from the matrix object created using makeCacheMatrix
    mat <- x$getMatrix()
    
    # Compute Inverse
    invr <- solve(mat, ...)
    
    # Set the value of the inverse in the matrix object
    x$setInverse(invr)
    
    # Return the inverse
    invr
}
