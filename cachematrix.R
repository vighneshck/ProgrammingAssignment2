## The function makeCacheMatrix() has 4 different functions:
## set(): to take in a matrix and store it in x
## get(): to obtain the stored matix x
## setinv(): to find the inverse of the matrix x by using solve() and store it in inv
## getinv(): to obtain the stored inverse matrix inv
## The function cacheSolve(), meanwhile, retrieves the inverse matrix inv from the cache, and if the obtained object is null, then the inverse is computed again. If a precomputed value already exists, then the function continues to use the solve() on this value.

## Function to set/get the matrix, and to set/get the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Function to solve the inverse of the matrix from scratch if not computed, and to continue solving from cache if already precomputed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("retrieving cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
