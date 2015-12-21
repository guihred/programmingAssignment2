## Make cache matrix is a function/object that produces a way of
## caching the inverse of a matrix in order to avoid recalculating such a costly
## procedure.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function()
        x
    setinverse <- function(inverse)
        m <<- inverse
    getinverse <- function()
        m
    list(
        set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


## This function gets the value of the cached inverse of the 
## matrix and solves it if the calculations haven't been made yet.



cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
