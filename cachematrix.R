## Programming Assignment 2, Course 2, Week 3

## These two function cache the inverse of a matrix.
## For a more detailed explanation on inversing a matrix visit: https://www.mathsisfun.com/algebra/matrix-inverse.html

## makeCacheMatrix is used to facilitate caching. A matrix is used for the input.
## The makeCacheMatrix object consists of 4 functions: set, get, setinverse, getinverse.

makeCacheMatrix <- function(x = matrix()) {
        
        # Follows the same format a the makeVector example
        i <- NULL
        
        # set function - sets the matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        # get function - gets the matrix
        get <- function() x
        
        # setinverse - sets the inverse
        setinverse <- function(inverse) i <<- inverse
        
        # getinverse - gets the inverse
        getinverse <- function() i
        
        # Returns functions to work environment
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Calculates the inverse of the matrix created in makeCacheMatrix

cacheSolve <- function(x, ...) {
        
        # Gets the inverse of the matrix stored in cache and check if calculated.
        i <- x$getinverse()
        
        # If calculated then return the inverted matrix from cache.
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        # If not then get the matrix.
        data <- x$get()
        
        # Set inverse of matrix.
        i <- solve(data, ...)
        
        # Cache the result.
        x$setinverse(i)
        
        # Return result
        i
}

