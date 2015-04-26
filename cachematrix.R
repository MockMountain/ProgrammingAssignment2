## Matrix functions to cache matrix inversions.
## Based on the assignment example for caching the mean of a vector.

## Creates a special matrix which can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    # Initialize the inverse to NULL.
    inv <- NULL

    # Define helper functions to get and set stored matrix x and
    # its inverse inv.
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv

    # Return a list of the helper functions.
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Retrieve inverse from cache or calculate and store.
cacheSolve <- function(x, ...) {
    # Return the inverse if it has already been computed.
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }

    # The inverse has not been computed.
    # Get and compute the inverse.
    data <- x$get()
    inv <- solve(data, ...)

    # Store the computation back into the special matrix.
    x$setinv(inv)

    # Return the computed inverse.
    inv
}


## Some test code.
#test <- matrix(rnorm(9), 3, 3)
#t2 <- matrix(rnorm(9), 3, 3)
#special <- makeCacheMatrix(test)
#print(cacheSolve(special))
#f2 <- makeCacheMatrix(t2)
#print(cacheSolve(special))
#print(cacheSolve(f2))
#print(cacheSolve(special))
