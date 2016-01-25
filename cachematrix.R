## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
