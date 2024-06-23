## First, create the special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  # Initialise the inverse
        
        # Set the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # Get the matrix
        get <- function() x
        
        # Set the inverse of the matrix
        setInverse <- function(inverse) inv <<- inverse
        
        # Het the inverse of the matrix
        getInverse <- function() inv
        
        # Return the list of methods
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        
        # If the inverse is already calculated, retrieve it from the cache
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # Otherwise, calculate the inverse
        mat <- x$get()
        inv <- solve(mat, ...)
        
        # Set the inverse in the cache
        x$setInverse(inv)
        
        inv  # Return the inverse
}
