# makeCacheMatrix()
# takes a matrix as an argument
# and outputs a list containing 4 objects
# $set, $get, $setInv, $getInv
# note that all these 4 objects, when valid, are matrices. 
# else they are NULLs
makeCacheMatrix <- function(x = matrix()) {

	# initialise Inv to NULL
	Inv <- NULL				
	
	# takes the matrix x and assigns it into $get object
	get <- function() x		
	
	# takes a matrix and assigns it externally 
	# to the Inv object
	setInv <- function(mInv) Inv <<- mInv
	
	# passes the Inv object from setInv()
	# to $getInv object
	getInv <- function() Inv
	
	# returns the list of objects & their 
	# respective output values set
	list(
		get = get,
		setInv = setInv,
		getInv = getInv
		)
}


# cacheSolve()
# is used to compute a matrix's inverse
#
# Pre-requisite: must first run the makeCacheMatrix() function on the matrix
cacheSolve <- function(x, ...) {
	
	# first we check the $getInv object from makeCacheMatrix()'s list
	# for any cached inverse matrix value
	# and assign it to cInv
	cInv <- x$getInv()
	
	# if cInv is NOT NULL, we will return the cached value
	if(!is.null(cInv)) {
			message("getting cached data")
			return(cInv)
	}
	
	# otherwise, we get the $get object from makeCacheMatrix()
	# , which is the actual matrix to compute inverse for
	# into Mx object
	Mx <- x$get()
	
	# now calculate the inverse of Mx and assign it to xM
	xM <- solve(Mx)
	
	# makes use of the setInv() function from makeCacheMatrix
	# to set xM into makeCacheMatrix()'s Inv object, which
	# then caches it into the $getInv object
	x$setInv(xM)
	
	# prints the inverse matrix
	message("not cached, computing...")
	xM
		
}