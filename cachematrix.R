## Put comments here that give an overall description of what your
## functions do
## 

##The first call of makeCacheMatrix creates a new
##	function and the associated closure environment and
##   	sets the initial value of 'inv' to NULL.
##   It's not necessary to use it for the initial matrix
## 	passed to the makeCacheMatrix function.
##   	But it could be used if we want to work on a new one
##The other three functions are straightforward: 
##	get returns the input data
##   	setinv sets the inverted matrix into the cache
##   	getinv returns the inverted matrix from the cache
##The list function defines the $ operations and is
##	the only thing returned to the parent environment
##	on the initail call to create the new function

makeCacheMatrix <- function(x = matrix()) {
	set <- function(y) {

		x <<-y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(x) inv <<- x
	getinv <- function() inv
	list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## Write a short comment describing this function
##cachesolve is called by passing in the name of the
##	initial function/closure environment created by
##	the first call to makeCacheMatrix
##It's operation is pretty straightforward
##	It calls the various $ accessible methods of the 
##    closure function to:
##		fetch the inverse from the value cached 
##		in the parent environment
##		if it's not Null return it
##		If it's null 
##			get the input matrix from the env
##			compute the inverse
##			set the value of the cache variable 'inv'
##			return the value of the cached variable

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if (!is.null(inv)) {
		message("getting cached inverse")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv

}
