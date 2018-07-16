## This function will create a matrix and a few other supporting functions within the main function in order to be 
## used by a separate function "cacheSolve()" to either retrieve an already stored value in
## memory or calculate the value once again and store that value in memory.

## Create matrix which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        # Define function to set the value of the matrix. It also clears the old
        # coputation results from the cache
        set <- function(y) {
                x <<- y    # Set the value
                m <<- NULL # Clear the cache
        }
        # Define function to get the value of the matrix
        get <- function() x
        # Define function to execute the inverse, to be used only used by getinverse() when
        # there is no cached inverse
        setInverse <- function(inverse) m <<- inverse
        # Define function to get the inverse
        getInverse <- function() m
        
        # Return a list with the above four functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

# Return inverse of matrix x
# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# and the matrix has not changed, then the cachesolve retrieves the 
# inverse from the cache.

# Return inverse of matrix x
# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# and the matrix has not changed, then the cachesolve retrieves the 
# inverse from the cache.

cacheSolve <- function(x) {
        m <- x$getInverse() # This fetches the cached value for the inverse
        if(!is.null(m)) { # If the cache was not empty, we can just return it
                message("getting cached data")
                return(m)
        }
        # If the cache is empty, we need to calculate the new matrix, cache it, and then return it.
        data <- x$get()  # Get value of matrix
        m <- solve(data) # Calculate inverse
        x$setInverse(m)  # Cache the result
        m                # Return the inverse
}
