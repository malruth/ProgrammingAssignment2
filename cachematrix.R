## The following functions work in concert to expand the basic matrix functionality
##		with caching of inverse values. The inverse of a matrix is set to NULL each
##		time its value is changed. Each time the inverse is requested, it is
##		calculated only if its value is currently NULL. This means that an
##		inverse only needs to be calculated once for any given matrix, until
##		that matrix is changed.


## The following function returns a list of accessor and mutator functions for
##		the manipulation of the base matrix. These provide an interface through
##		which the matrix is manipulated, which allows further functions to
##		optimimization certain operations.
makeCacheMatrix <- function(baseMatrix = matrix()) {
	## baseMatrix - The matrix to be stored for optimization purposes
	
	## The inverse is left unset during initialization
	inverse <- NULL
	
	## Creates a mutator function that assigns a new base matrix
	set <- function(newMatrix) {
		baseMatrix <<- newMatrix	## reassign a new base matrix
		inverse <<- NULL			## clear the inverse matrix cache
	}
	
	## Creates an accessor function that returns the base matrix
	get <- function() baseMatrix
	
	## Creates a mutator function that assigns a new inverse matrix
	setInverse <- function(newInverse) inverse <<- newInverse
	
	## Creates an accessor function that returns the inverse matrix
	getInverse <- function() inverse
	
	## Packages the interface into a list and returns it
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The following function makes use of the above list interface to optimize the
##		operation of creating an inverse of a given base matrix. When this function
##		is called to request that inverse, it first checks if that inverse is
##		undefined. If it is undefined, it initializes it. Following this test,
##		it returns it. This results in successive calls being able to skip the
##		initialization step and simply return the prepared inverse immediately.
cacheSolve <- function(baseMatrix, ...) {
	## baseMatrix - The interface to a base matrix created by the previous function
	
	## retrieve the existing cached inverse, which is NULL following the
	##		initialization and any later reassignments of the base matrix.
	inverse <- baseMatrix$getInverse()
	
	if(is.null(inverse)) {				## if that cached copy is unset
		message("DEBUG Message: Calculating Inverse!")
		data <- baseMatrix$get()		## extract the base matrix from the interface
		inverse <- solve(data)			## calculate the inverse of the base matrix
		baseMatrix$setInverse(inverse)	## and store it into the matrix interface
	}
	
	## Return the inverse cache, regardless of what actions took place just now
	inverse
}
