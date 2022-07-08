# For this assignment, assume that the matrix supplied is always invertible.
# Below we call the function with a matrix, compute the inverse, retrieve the inverse from the cache list,
# change the call matrix to the inverse, compute the inverse on that and return the original function.

## This function creates a special “matrix” object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function() inv <<- solve(x) #calculate the inverse
	getInverse <- function() inv
	list(set = set,
			 get = get,
			 setInverse = setInverse,
			 getInverse = getInverse)
}

## This function computes the inverse of the special “matrix” returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache. Computing the inverse of a square matrix can be done
## with the solve function in R. For example, if X is a square invertible matrix, then solve(X)
## returns its inverse.

cacheSolve <- function(x, ...) {
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
