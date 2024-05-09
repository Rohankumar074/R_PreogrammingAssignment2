# Create a special matrix object that can cache its inverse
# This function is adapted from a Coursera Data Science: R Programming assignment, authored by GitHub user PamlaM.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                                 # Initialize 'inv' to hold matrix inverse
    set <- function(y) {                        # Define 'set' function to update matrix value
        x <<- y                                 # Assign new matrix value in the parent environment
        inv <<- NULL                            # Reset inverse to NULL since matrix changed
    }
    get <- function() x                         # Define 'get' function to retrieve matrix value
    setinverse <- function(inverse) inv <<- inverse  # Assign 'inv' value in parent environment
    getinverse <- function() inv                      # Retrieve 'inv' value where called
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  # Return list of functions accessible with $ operator
}

# Compute the inverse of a cached matrix object
# This function is adapted from a Coursera Data Science: R Programming assignment, authored by GitHub user PamlaM.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()                           # Retrieve cached inverse if available
    if (!is.null(inv)) {
        message("Getting cached data")               # Display message indicating cached data retrieval
        return(inv)                                 # Return cached inverse
    }
    data <- x$get()                                 # Retrieve matrix data
    inv <- solve(data, ...)                         # Compute inverse of the matrix
    x$setinverse(inv)                               # Cache the computed inverse
    inv                                            # Return the computed inverse
}
