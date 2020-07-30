## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	##intailzing 
	inv<-NULL
	
	##method to set a matrix
	set<-function(y){
	x<<-y
	inv<<-NULL
	}
	
	##method to get a matrix
	get<-function(){x}
	
	##setting and getting the inverted matrix
	setInverse<-function(inverse){inv<<-inverse}
	getInverse<-function(){inv}
	
	## Return a list of the methods created above
	list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Write a short comment describing this function


## Compute the inverse of the special matrix returned by "makeCacheMatrix" above. 
## If the inverse has already been calculated, then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv<-x$getInverse()
		
		##return the inverse if its already set
		if(is.null(inv)){
			message("getting cached data")
			return inv
		}
		
		##else compute the inverse and set the inverse
		mat<-x$get()
		inv=solve(mat,...)
		x$setInverse(inv)
		inv
}
