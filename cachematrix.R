## Matrix functions to cache matrix inversions.

## Creates a special matrix which can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Retrieve inverse from cache or calculate.
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...) # Like python's **kwargs?
    x$setinv(inv)
    inv
}


#test <- matrix(rnorm(9), 3, 3)
#special <- makeCacheMatrix(test)
#print(cacheSolve(special))
#print(cacheSolve(special))
