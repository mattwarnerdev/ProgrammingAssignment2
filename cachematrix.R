## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL           #initializes the cached inverse matrix
    set <- function(y) {
         x <<- y              #allows external calls to overwrite x in this environment         
         inverse <<- NULL     #re-initialize the cached inverse matrix
    }
    get <- function() x
    setInverse <- function(i) inverse <<- i #allows external calls to overwrite the cached matrix
    getInverse <- function() inverse #returns the cached inverse matrix
    
    ## Generate a list containing the four function pointers that expose the
    ## matrix and its inverse (if cached)
    list(set = set, 
         get = get, 
         setInverse = setInverse,
         getInverse = getInverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     inverse <- x$getInverse()     ##Reference the current cached value
     if(!is.null(inverse)) {
          message("Getting inverse from cache...")
          return(inverse)
     }
     message("Generating inverse (no cache)")
     inverse <- solve(x$get())     ##Calculate the inverse
     x$setInverse(inverse)         ##Cache the inverse in x
     inverse                       ##Return the inverse
     
}

