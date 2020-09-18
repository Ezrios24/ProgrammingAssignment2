## Set of functions that cache the inverse of a matrix
## Usage:  Pass the result of a makeCacheMatrix call to cacheSolve

## Function that set the matrix and the inverse in an enviroment
## PARAMETERS
## x is an invertible matrix
## x = makeCacheMatrix(matrix(rnorm(16), 4, 4))
## x$set(matrix(rnorm(25), 5, 5))

makeCacheMatrix <- function(x = matrix()) {
        ## Initialize objects
        m <- NULL
        ## Define "behaviors" or functions for objects of type
        ## makeCacheMatrix
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        ## Define the getter for the matrix "x"
        get <- function() x
        setsolve <- function(solve) m<<- solve
        getsolve <- function() m
        ## Create a new object by returning a list()
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Compute and cache the inverse of a matrix
## PARAMETERS
## x is the result from the previous makeCacheMatrix call
## ... aditional arguments to pass to solve function
## USAGE 
## x = makeCacheMatrix(matrix(rnorm(16), 4, 4))
## cachesolve(x)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setsolve(m)
        m
}
