## SUMMARY:
## makeCacheMatrix and cacheSolve are two functions 
## that manage a matrix and its inverse.
##
## Because computing the inverse of a matrix can be expensive,
## calling cacheSolve will calculate the inverse of a given
## matrix only once, then use the cached instance on 
## subsequent requests.
##
## See the demonstrate() function below for a walkthrough.



## makeCacheMatrix creates a wrapper for a matrix,
## exposing the matrix through get and set functions
## and creating a placeholder for the inverse
## of that matrix.
##
## The inverse is not automatically generated (call 
## cacheSolve for that)
##
## PARAMETERS: 
##   x, The matrix to be stored and manipulated
##
## RETURNS: 
##   The four getters and setters are exposed through a 
##   returned list.
makeCacheMatrix <- function(x = matrix()) {
     #initializes the cached inverse matrix
     inverse <- NULL    
     
     #Allows external calls to access and overwrite the matrix 
     # in this environment and re-initialize the cached inverse matrix
     set <- function(y) {
          x <<- y              
          inverse <<- NULL     
     }
     get <- function() x
     
     #Allows external calls to access and overwrite the cached inverse matrix
     # and returns the cached inverse matrix
     setInverse <- function(i) inverse <<- i 
     getInverse <- function() inverse 
     
     ## Return a list containing the four function pointers that expose the
     ## matrix and its inverse (if cached)
     list(set = set, 
          get = get, 
          setInverse = setInverse,
          getInverse = getInverse)
     
}


## cacheSolve returns the inverse of a matrix, but uses a cached
## version of the inverse if possible.  
##
## It takes a wrapped matrix built in makeCacheMatrix,
## calculates the inverse matrix once, and stores the result
## in the list containing the original matrix.  Subsequent
## calls to cacheSolve do not recalculate the inverse, but instead
## use the cached result.
##
## PARAMETERS:
##   A list containing the matrix to invert
##   The ... parameter for additional optional parameters
##
## RETURNS:
##   The inverse of the matrix (a cached version, if available)
cacheSolve <- function(x, ...) {
     
     ##Reference the current cached value
     inverse <- x$getInverse()     
     
     ## Use the cached value if it exists, or calculate and cache it if it doesn't exist
     if(!is.null(inverse)) {
          message("Cached inverse found.  No recalculation needed.  Yeehoo!")          
     } else {
          message("Generating inverse (no cache)")
          inverse <- solve(x$get())     
          x$setInverse(inverse)    
     }
     
     ##Return the inverse
     inverse                       
     
}

demonstrate <- function() {
     ## Sample matrix and inverse from
     ## http://www.mathwords.com/i/inverse_of_a_matrix.htm
     
     message()
     message("Thanks to http://www.mathwords.com/i/inverse_of_a_matrix.htm ")
     message(" for the example [4, 3; 3, 2] matrix and it's inverse, [-2, 3; 3, -4].")
     message()
     message("Building invertible matrix, store in variable 'm':")
     m <- matrix(c(4, 3, 3, 2), 2)
     print(m)
     
     message()
     message("Build cache matrix list with makeCacheMatrix(m), store in cm")
     cm <- makeCacheMatrix(m)
     
     message()
     message("Matrix 'm' is now accessible through cm$get()")
     print(cm$get())
     message("...but the inverse is not yet calculated cm$getInverse():")
     print(cm$getInverse())
     
     message("Call cacheSolve(cm)...Inverse should be calculated because")
     message(" it's the first lookup...")
     k <- cacheSolve(cm)
     message("Printing inverse with cm$getInverse()")
     print(cm$getInverse())
     message("Or by capturing the direct output from the cacheSolve(cm) function...")
     print(k)
     
     message()
     message("Calling cacheSolve(cm) again should use the cached")
     message(" version, no recalculation needed.")
     cacheSolve(cm)
     message("Printing inverse with cm$getInverse()")
     print(cm$getInverse())
     
     message()
     message("Per the website referenced above, the inverse")
     message(" of the [4, 3; 3, 2] matrix is [-2, 3; 3, -4] ")
}

