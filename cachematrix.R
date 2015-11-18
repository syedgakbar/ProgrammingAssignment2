## This work is done as part of the R Programming Assignment##2
## https://class.coursera.org/rprog-034

## What this R code provide helper function to create a CachedMatrix (by calling the makeCacheMatrix).
## This object functions can then be used to get and set the base Matrix and the Cached Inverse value.
## You can calculate the Inverse of this CachedMatrix by cacheSolve helper function which also stores
## the calculated Inverse in the cahche for future calls.

## Example (R Code)
## sampleMatrix <- matrix(rnorm(4 * 4), 4, 4)
## cachedMatrix <- makeCacheMatrix(sampleMatrix)
## inverse <- cacheSolve(cachedMatrix)


## Create a Cached Matrix Solver (for Inverse calculation) which provides helper functions to not 
## only calculate Inverse but also store this in the Cache so that Cache result can be used
## for future call (as long as base matrix is not changed)
makeCacheMatrix <- function(mat = matrix()) {
		
	# Built or clear the cache variable on the first call
	cahcedInverse <- NULL
	
	# Make sure this is valid matrix
	if(!is.matrix(mat)) {
		message("Error: Not a Matrix.")
		return(mat)
	}
	
	# Handle the new matrix assignment. 
	set <- function(value) {
	
			# Make sure this is valid matrix
			if(!is.matrix(value)) {
				message("Error: Not a Matrix.")
				return(value)
			}
	
			mat <<- value
			# Reset the Cache when new Matrix is aassign
			cahcedInverse <<- NULL
	}
	
	# Return the original matrix
	get <- function() mat
	
	# Get and Set for the Cached Matrix Inverse
	set.inverse <- function(value) cahcedInverse <<- value
	get.inverse <- function() cahcedInverse
	
	# Expose all the helper functions
	list(set = set, get = get,
		 set.inverse = set.inverse,
		 get.inverse = get.inverse)
}


## Solve function for the CacheMatrix (as returned by the makeCacheMatrix funciton call). This function
## should be used to calculate the Inverse of the CacheMatrix, as this function also update the Cache 
## after calculating the Inverse of the given Matrix, and only calculate the new Inverse if Matrix has been
## changed since last call
cacheSolve <- function(cachedMatrix, ...) {
        
	cahcedInverse <- cachedMatrix$get.inverse()
	
	# Check if we have the pre-calculated Inverse of this Matrix in Cache
	if(!is.null(cahcedInverse)) {
		message("Getting cached data.")
		return(cahcedInverse)
	}
	
	# Get the original matrix
	mat <- cachedMatrix$get()
	
	# Calculate the Inverse of it
	cahcedInverse <- solve(mat, ...)
	
	# Update the cache with this new calculated inverse
	cachedMatrix$set.inverse(cahcedInverse)
	
	# Return a matrix that is the inverse of given matrix
	cahcedInverse
}
