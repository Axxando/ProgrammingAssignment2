## This is the R code for the Coursera course "R Programming" in December 2014
## Used and pushed to  my github account for Progamming Assignment 2


## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) { #input x will be matrix

	i <- NULL	# i will be our inverse of x and set to NULL
				# every time makeCacheMatrix is called

	set <- function(y) {	# create a list 
			x <<- y			# containing the original matrix
			i <<- NULL		# and will contain the inverted matrix
	}
	
	get <- function() x		# this function returns the value of the original matrix

	setsolve <- function(solve) i <<- solve	# this is called by cacheSolve() 
											# during the first cacheSolve()
	
	getsolve <- function() i	# this will return the cached value to cacheSolve() on
                                # subsequent accesses
	
	list(set = set, get = get,	# This is a list of the internal functions ('methods') 
		 setsolve = setsolve,	# so a calling function knows how to access those methods
		 getsolve = getsolve)	
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the cachesolve should retrieve the inverse from the cache.
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) { # the input x is an object created by makeCacheMatrix
	
	i <- x$getsolve()	# accesses the object 'x' and gets the value of the solve
	
	if(!is.null(i)) {	# if solve was already cached (not NULL) ...
			message("getting cached data")	# ... send this message to the console
			return(i)						# ... and return the solve ... "return" ends
											#   the function cacheSolve(), note
	}
	
	data <- x$get()	 # we reach this code only if x$getsolve() returned NULL
	
	i <- solve(data, ...)	# if i was NULL then we have to calculate the solve
	
	x$setsolve(i)	 # store the calculated solve value in x (see setsolve() in makeCacheMatrix

	i	# return the solve to the code that called this function
}

