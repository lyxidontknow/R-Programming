## makeCacheMatrix is a function that returns a list of four functions that can 
## be called by cacheSolve.
## cacheSolve computes the inverse of a matrix and returns it. 
## If the inverse of that particular matrix is already calculated, cacheSolve 
## simply returns it

rm(list=ls())		## remove all objects to make a fresh start

## This function creates a special "matrix"(a list of four functions) that can 
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL		# create a variable to store the inverse
	## manually set the matrix and its inverse to be stored in makeCacheMatrix
	set <- function(y){
	    ## <<- assigns value to an object that can be stored in makeCacheMatrix, 
	    ## not just makeCacheMatrix$set
	    x <<- y
		inverse <<- NULL
	}
	get <- function() x		# get the matrix x that is going to inverted
	setinverse <- function(inv) inverse <<- inv		# set the inverse
	getinverse <- function() inverse		# get the inverse
	# return a list of functions
	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function takes the special "matrix" returned by makeCacheMatrix, and 
## computes the inverse of the matrix as the argument of makeCacheMatrix

cacheSolve <- function(x) {
	inv <- x$getinverse()		## get the inverse stored in makeCacheMatrix
	## if the stored inverse has been calculated, generate a dignostic message 
	## and return the inverse
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	## if the inverse has not been calculated, compute the inverse of the matrix
	data <- x$get()		# get the matrix that is going to be inverted
	inv <- solve(data)	# compute the inverse of the matrix
	x$setinverse(inv)	# cache the inverse
	inv
}
