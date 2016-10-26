
## Writing the problem given in the course material
## Caching the Inverse of a Matrix:

## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly

## Please find the function (1) store matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y         # Make the object available in other environment
                inv <<- NULL    # NULL object
        }
        
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        # Similar to the given code in example of mean         
        list(set = set,  get = get,  setInverse = setInverse,  getInverse = getInverse)      
}
                
# (2) function to calculate the inverse of a matrix
# the matrix should be full rank and square matrix, but that check has not been performed
# to make the function full proof, this error should be built 
                
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
         }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv                     # Inverse of matrix
}        
