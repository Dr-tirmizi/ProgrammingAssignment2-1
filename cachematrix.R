# Programming Assignment 2
# Setting x input as a matrix
# Further setting s as Null and changing mean to solve in this program to run.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function()
        x
    setInverse <- function(inverse)
        inv <<- inverse
    getInverse <- function()
        inv
    list(
        set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse
    )
}

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}

#codes to Test the program to run the functions

m <- matrix(rnorm(16), 4, 4)
m1 <- makeCacheMatrix(m)
cacheSolve(m1)
