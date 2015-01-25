## makeCacheMatrix creates a special matrix object that can cache its inverse
## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix
## both functions are based on the example functions provided with the assignment

## creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## takes a matrix x as an input
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        ## returns a list of four functions to calculate inverse
}


## compute the inverse of the matrix returned by makeCacheMatrix
## retrieves inverse from cache if it has already been calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
                ## checks for cached matrix and returns cached matrix if it exists
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
        ## calculates and returns inverse if not cached
}