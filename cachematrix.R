## The functions caches the inverse of a given matrix. The inverse of a matrix requires time concuming computations.
## After the computation is done once, it cashes the result, so if the same result is needed in the future, the computations
## will not be done again.

## The first function creates a matrix, which is list containing some functions 

makeCasheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }  
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## The following function calculates the inverse of the matrix created with the above function. 
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the mean from the cache and skips the computation.

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
