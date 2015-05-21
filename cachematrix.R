## Put comments here that give an overall description of what your
## functions do# 
#This program is for the class Coursera/R Programming
# I will attempt to make a program that is similar to the example, but inverts a matrix.

# This function Creats a matrix makeCacheMatrix().  
# Generates i as a null.(The next part will check to see if this is still null later....)
# Sets the matrix.
# Gets the Matrix.
# Sets value of inverse
# Gets value of said inverse


makeCacheMatrix <- function(x = matrix()) {
    i <- NULL                           #sets i to null, = location we will store the inverse.
    set <- function(y) {                
        x <<- y
        i <<- NULL
    }                                   #endSet
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(	set=set, 
		get=get, 
		setinverse=setinverse, 
		getinverse=getinverse)
}                                       #endmakeCacheMatrix

# This next function checks if the inverse exists.
# If TRUE, it gets the result and skips the computation.
# If FALSE, it computes the inverse.
# Then, sets the value in the cache setinverse function.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
                                        # This part checks to see if the inverse exists.
    if(!is.null(i)) {
        message("Getting cached data...")
        return(i)                       # If = True returns i and the above statement.
    }                                   #endif
    data <- x$get()                     # If "i is null" then computes inverse matrix using solve().
    i <- solve(data)                    # inverts matrix
    x$setinverse(i)                     #sets inverse
    i
}                                       #end cacheSolve
