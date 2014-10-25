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
}

makeVector <- function(x = numeric()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setmean <- function(mean) m <<- mean
     getmean <- function() m
     list(set = set, get = get,
          setmean = setmean,
          getmean = getmean)
}

cachemean <- function(x, ...) {
     m <- x$getmean()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- mean(data, ...)
     x$setmean(m)
     m
}