## Time saving functions for solving the inverse of a matrix

## Funtion to cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() { x }
        setsolve <- function(solve) { m <<- solve }
        getsolve <- function() { m }
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Function to either solve and print the inversed matrix or print the previously
## cached inverse matrix if not yet evaluated 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

