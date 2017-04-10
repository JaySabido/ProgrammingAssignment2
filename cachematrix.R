## Caching the Inverse of a Matrix - Programming Assignment #2
## by Jay Sabido

## These pair of functions makeCacheMatrix () and 
## cacheSolve(), cache the inverse of a given matrix

## 1st function makeCacheMatrix: This function creates a special "matrix" 
## object that will cache a given matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {   # initialize the matrix x
                                        # Default value of x is an empty matrix
        invx <- NULL                    # initialize invx to NULL
        setmatrix <- function(y) {      # Define a setmatrix() function
                x <<- y                 # Assign the input argument to 
                                        # the x object in the parent environment
                invx <<- NULL           # Assigns NULL to the invx object in 
                                        # the parent environment. Clears the invx object
        }
        getmatrix <- function() x       # Define a getmatrix() function
        setinv <- function(solve) invx <<- solve  # Sets the inverse 
        getinv <- function() invx       # function for the getter of the inverse
        
        # Next step is to assign each of the four functions as an element 
        # within a list() and naming these functions. These will be returned to the parent environment
        list(setmatrix = setmatrix,     # Gives the name 'setmatrix' to the setmatrix() function
             getmatrix = getmatrix,     # Gives the name 'getmatrix' to the getmatrix() function
             setinv = setinv,           # Gives the name 'setinv' to the setinv() function
             getinv = getinv)           # Gives the name 'getinv' to the getinv() function
}


## The cacheSolve function computes the inverse of the special "matrix" 
## returned by the makeCacheMatrix function above. If the inverse has 
## already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        invx <- x$getinv()              # Retrieve the inverse
        if(!is.null(invx)) {            # If the value here is not equal to NULL, then there's a valid cached inverse matrix that will be returned
                message("getting cached inversed matrix")
                return(invx)
        }
        datamat <- x$getmatrix()        # Gets the matrix from the input object
        invx <- solve(datamat, ...)     # Calculates the inverse of the matrix
        x$setinv(invx)                  # Use the setinv() function on the input object to set the inverse in the input object
        invx                            # Returns the value of the inverse to the parent environment
}
