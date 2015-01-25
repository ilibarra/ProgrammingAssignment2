##############
# The script demonstrates the use of Lexical Scooping for calculating the
# inverse of a matrix and caching this value into a variable to avoid 
# calculating it more than once. 
#
# e.g. Run the following commands:
# 	M <- makeCacheMatrix(matrix(4:7,nrow=2,ncol=2))
# 	cacheSolve(M); cacheSolve(M)
#	M <- makeCacheMatrix(matrix(c(1,2,3,2,5,2,6,-3,1), nrow=3, ncol=3))
#	cacheSolve(M); cacheSolve(M)
#
# Created on Jan 25, 2015 @author: ilibarra
##############

### It generates a matrix object with a cached inverse matrix option
makeCacheMatrix <- function(X = matrix()) {
	I <- NULL
	## new matrices: flush old inverse matrix and reset input matrix
	set <- function(y) {
		X <<- y
		I <<- NULL
	}
	## getters and setters
	get <- function() X
	setinverse <- function(solve) I <<- solve
	getinverse <- function() I
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}

### Calculate an inverse matrix from the output one. The result is
### stored into a cached variable 
cacheSolve <- function(X, ...) {
	## if the value has been calculated before, stop and return current I.
	I <- X$getinverse()
	if(!is.null(I)) {
		message("getting cached data")
		return(I)
	}
	## if the inverse matrix does not exists, solve it and store it.
	data <- X$get()
	I <- solve(data)
	X$setinverse(I)
	I
}