## This file contains two functions that use the solve function in R
## to either take the inverse of a matrix and cache the inverse of a matrix.

## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function()x 
	setinverse <- function(solve) m <<- solve
	getinverse <- function() solve
	list(set = set, get = get,
	setinverse = setinverse,
	getinverse = getinverse)

}


## This function computes the inverse of the special matrix returned by
## the above function. If the inverse has been calculated (and the matrix
## has not changed), then this function should retrieve the inverse from
## the cache.

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
