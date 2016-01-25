## Functions that cache the inverse of matrix
## R Programming - Programming Assignment 2: Lexical Scoping

## makeCacheMatrix creates a special "matrix" object that includes the following functions:
## set the values of the matrix
## get returns the values of the matrix
## setinv sets the inverse of the matrix
## getinv returns the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
 	## Initialize m with NULL
	m <- NULL

	##Reset x and m 
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

	##get x
        get <- function() x

	##set m
        setInverse <- function(invm) m <<- invm

	##get m
        getInverse <- function() m

	##list with functions
        list(
		set = set, 
		get = get,
             	setInverse = setInverse,
             	getInverse = getInverse)
}


## The cacheSolve function calculates the inverse of the matrix stored makeCacheMatrix
## object above, but first it check to see if the inverse is already cached.

## If the inverse is already cached, cacheSolve does not calculate the inverse. 
## Instead, cacheSolve returns the cached invers

cacheSolve <- function(x, ...) {
	##get m
        m <- x$getInverse()

	##check if m aready exists
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
	
	##if not set get new inverse
        data <- x$get()
        m <- solve(data)

	##set m
        x$setInverse(m)

	##return m
        m
}
