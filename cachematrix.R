# This set of functions can cache the inverse of a matrix from its
#  computation for the first time, and be retrieved quickly for future use.

# This function creates a matrix object for which the calculated inverse
#  can be cached.

makeCacheMatrix <- function(x = matrix()){
	m <- NULL		# m is the inverse of the matrix which is reset
				#	to NULL whenever makeCacheMatrix is called
	set <- function(y){	
		x <<- y
		m <<- NULL
	}
	get<- function() x	# returns the value of the matrix
	setinverse <- function(solve) m <<-solve
	getinverse <- function() m		# list of internal functions to
		list(set = set, get= get,	# allow calling by another function
			setinverse = setinverse,
			getinverse = getinverse)
}


# This function retreives the cached value of the previously calculated
#  inverse of a matrix, or calculates for the first time the inverse
#  of a matrix from the makeCacheMatrix function above.

cacheSolve <- function(x,...){	#x is an object created by makeVector
	m <- x$getinverse()
	if(!is.null(m)){			#if mean was already cached
		message("getting cached data")
		return(m)
	}
	data <- x$get()		#if there is not previously cached data,the
	m <- solve(data,...)	# matrx, x, is loaded and inverse calculated
	x$setinverse(m)
	m
}
