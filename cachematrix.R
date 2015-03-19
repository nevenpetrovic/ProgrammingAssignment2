## This function creates a special matrix object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { # Initialize the function which requires a matrix as its only argument
		inverse <- NULL    		  # Initialize a local variable to the function (inverse) to NULL.
		
		# construct an object function called set
		# the purpose of this function is to reset (inverse) to NULL
		# and set the cached x value to the passed in matrix
		
		set <- function(y) {
            	x <<- y
                	inverse <<- NULL
        	}
		
		get <- function() return(x) 			      # return the cached matrix 
        	setinverse <- function(inv) inverse <<- inv     # function requiring a solve and will set the cached solve
        	getinverse <- function()  return(inverse) 	# function which returns the cached invers matrix
		getevn <- function() environment()
        	list(set = set, get = get,
             	setinverse = setinverse,			# Lists out the values of the functions in the
             	getinverse = getinverse,			# makeCacheMatrix frame
             	getevn = getevn)

}


## This function computes the inverse of the special
## matrix returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache


cacheSolve <- function(x, ...) {  			# Initialize the function which requires a makeCacheMatrix object
      	
		 inverse <- x$getinverse() 		# Initialize a local variable 
            
		 if(!is.null(inverse)) {		# If inverse is not null it will print a message to the command line                    
                  message("getting cached data")  # and return the inverse obtained from the makeCacheMatrix object
			return(inverse)	      
             }
             data <- x$get()				# if inverse is null, the function will obtain the matrix from the makeCacheMatrix object and assign it to the data variable
             inverse <- solve(data, ...)		# calculate the inverse of the matrix obtained from the makeCacheMatrix object.
             x$setinverse(inverse)			# assigned the calculated ivverse to the makeCacheMatrix inverse
             return(inverse)				# return the calculated inverse
 
}
